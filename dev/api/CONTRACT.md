# replikit Excel API — request/response contract (DRAFT)

Two-call handshake. The engine holds warm **structural** state (portfolios, SMAs,
rules, accounts). The **market data** is supplied per request by the Excel client
via the Bloomberg Excel add-in (`=BDP()`), so the API box needs **no** Bloomberg
entitlement.

```
Excel                                  API (warm structural registry)
  |  1. POST /required-inputs  ------->  compute involved securities + fields
  |  <---- securities + fields --------  (OTC options excluded; server fills those)
  |  2. write =BDP() on Securities sheet, wait for fill
  |  3. POST /proposed-trade (+market_data) -->  price via per-request provider,
  |  <---- proposed_trades ------------       optimize, return trades
  |  4. write Results sheet
```

Everything is JSON. Security ids on the wire are the internal **lowercase** bbids,
except `bdp_id`, which is the exact string to feed `=BDP()`.

## Auth

Login exchange. The caller authenticates with their **own database credentials**;
the server validates them by opening a throwaway DB connection, then issues a
short-lived signed token used as the bearer on every other call. Authenticate-only
— a valid login just proves DB access; data still comes from the shared warm
registry.

### `POST /login` (public)
```json
{ "username": "alice", "password": "..." }
```
→ `200 { "token": "<jwt-like>", "token_type": "Bearer", "expires_in": 43200 }`
or `401 { "error": "invalid_credentials" }`.

Bare usernames get the Supabase pooler project-ref appended automatically; pass a
username containing `.` to bypass.

### Protected endpoints
Everything except `POST /login`, `GET /health`, and the docs (`/__docs__`,
`/openapi.json`) requires:
```
Authorization: Bearer <token from /login>
```
`401 {"error":"unauthorized"}` on a missing/invalid/expired token; `503
{"error":"auth_not_configured"}` if the server has no `REPLIKIT_API_SECRET`
(the HMAC signing key) set — fails closed.

Token is a stateless HMAC-SHA256 signed `{sub, exp}` (default TTL 12h via
`REPLIKIT_TOKEN_TTL_SEC`), so any worker in a pool can validate it without shared
state.

## Freshness & refresh

Holdings/NAV/rules are loaded into memory **once at server startup** (market data
is still per-request). Every response carries **`data_as_of`** — the UTC timestamp
of that load — so staleness is always visible (Excel shows "Holdings as of: …").

**`POST /refresh`** (auth-required) rebuilds the in-memory registry from current DB
state — call it after positions change in Supabase instead of restarting the
server. Returns `{ "status": "refreshed", "data_as_of": "..." }`. In a worker pool
each worker holds its own registry, so refresh reaches only the worker that serves
the call — refresh all workers or run it on a schedule.

## `POST /rebalance`

Optimize **every tracking SMA of a base** to its target (full CVXR `optimize_sma`,
rule-constrained), not a single trade. Prices the **whole book**, so call
`/required-inputs` first with **empty `trades`** to get the (large) security list.

### Request
```json
{ "portfolio": "ccmf", "market_data": [ { "id": "...", "PX_LAST": ..., ... }, ... ] }
```

### Response `200`
```json
{
  "data_as_of": "...",
  "rebalance_trades": [
    { "Portfolio": "amap", "Security": "www us equity", "TradeQuantity": -152,
      "TradePctNav": -0.021, "CurrentShares": 238009, "TargetShares": 237857 }
  ],
  "top_movers_smas": [ "amap", "atom_core" ],
  "top_movers": [
    { "Security": "vtrs us equity", "BaseWeight": 0.051529,
      "SmaWeights": {
        "amap":      { "CurrentWeight": 0.036875, "TargetWeight": 0.041002, "CurrentShares": 12000, "TradeShares": 900 },
        "atom_core": { "CurrentWeight": 0.041002, "TargetWeight": 0.041000, "CurrentShares":  8300, "TradeShares":  -5 }
      } }
  ],
  "totals": { "Direct": ..., "Swap": ... },
  "warnings": [ { "portfolio": "atom_core", "error": "…infeasible…" } ]
}
```

- **`rebalance_trades`** — per SMA per security, the trade to reach target
  (`TargetShares − Current`), non-zero trades only. `TradePctNav` = target − current
  weight. This can be large with distorted prices; with real prices only drifted
  names appear.
- **`top_movers`** — the top-N securities (default 10) needing the most
  rebalancing (ranked by average `|target %NAV − current %NAV|` across SMAs),
  each showing `BaseWeight` (the security's weight in the base fund) and
  `SmaWeights` (a `{ sma_name: {…} }` object). Each SMA entry carries
  `CurrentWeight` (current %NAV in that SMA — what Excel shows in the cell),
  `TargetWeight` (the optimizer's rule-constrained target %NAV), `CurrentShares`,
  and `TradeShares` (shares to trade to reach target, + buy / − sell). An SMA
  that neither holds nor targets the security is all-zero. Excel puts
  `CurrentWeight` in the cell and the other three (plus the weights) in a hover
  note. **`top_movers_smas`** lists the SMA names in column order.
- **`warnings`** — SMAs whose optimize failed (e.g. infeasible).

CVXR is pre-warmed at startup (a ~1s one-time cost) so the first rebalance is fast.

---

## 1. `POST /required-inputs`

Ask the server which securities Excel must price. Only the server knows the
holdings, so only it can produce this list.

### Request
```json
{
  "portfolio": "ccmf",
  "trades": [
    { "security": "aapl us equity", "qty": 100, "swap": false }
  ],
  "flow_to_derived": true
}
```

- `portfolio` — base portfolio short name.
- `trades` — intended trades; any security not currently held is added to the
  price request so it can be valued.
- `flow_to_derived` — include the derived SMAs' holdings (default `true`).

### Response `200`
```json
{
  "request_id": "9f1c...-uuid",
  "as_of": "2026-08-04T15:32:00Z",
  "fields": ["PX_LAST", "OP006", "EQY_SH_OUT", "..."],
  "securities": [
    { "id": "aapl us equity", "bdp_id": "AAPL US Equity" },
    { "id": "xyz us equity",  "bdp_id": "XYZ US Equity"  }
  ]
}
```

- `request_id` — opaque token that ties the two calls together. The server
  stashes the manifest of required (security, field) pairs against it so call 2
  can validate completeness and log exactly what priced the trade (audit).
- `fields` — the union of `PX_LAST`, `OP006` (delta), and every rule `bbfield`
  any active rule references. Excel lays the Securities sheet out as one column
  per field, one row per security.
- `securities` — the rows. **OTC options are omitted** — Bloomberg can't price
  them on their own id; the server fills them from Enfusion/Supabase.

---

## 2. `POST /proposed-trade`

### Request
```json
{
  "request_id": "9f1c...-uuid",
  "portfolio": "ccmf",
  "trades": [
    { "security": "aapl us equity", "qty": 100, "swap": false }
  ],
  "flow_to_derived": true,
  "market_data": [
    { "id": "aapl us equity", "PX_LAST": 231.42, "OP006": null, "EQY_SH_OUT": 15200.0 },
    { "id": "xyz us equity",  "PX_LAST":  48.10, "OP006": null, "EQY_SH_OUT":  8300.0 }
  ]
}
```

- `market_data` — one object per row of the filled Securities sheet, keyed by
  internal `id` plus each field mnemonic. Missing/blank BDP cells → `null`.

### Response `200`
```json
{
  "request_id": "9f1c...-uuid",
  "as_of": "2026-08-04T15:32:07Z",
  "holdings": [
    { "Portfolio": "ccmf",    "SharesHeld": 5000, "PctNav": 0.031, "Swap": false },
    { "Portfolio": "sma_abc", "SharesHeld": 40,   "PctNav": 0.012, "Swap": true  }
  ],
  "proposed_trades": [
    { "Portfolio": "ccmf",    "Security": "aapl us equity", "TradeQuantity": 100, "TradePctNav": 0.0006, "MarginalShares": null, "DriftShares": null, "CurrentShares": 5000, "TargetShares": 5100, "LimitingRule": null },
    { "Portfolio": "sma_abc", "Security": "aapl us equity", "TradeQuantity":  12, "TradePctNav": 0.003,  "MarginalShares": 8,    "DriftShares": 4,    "CurrentShares": 40,   "TargetShares": 52,   "LimitingRule": null }
  ],
  "totals": { "Direct": 112, "Swap": 0 },
  "warnings": []
}
```

- **`holdings`** — current position of each traded security per portfolio (base +
  SMAs): `SharesHeld`, `PctNav` (fraction of NAV, = shares · replication-price /
  NAV), and the `Swap` flag.
- **`proposed_trades`** — first row per security is the **base trade** (the
  replication input); the rest are replicated into each tracking SMA subject to
  its rules. `TradeQuantity` = the rule-constrained trade (`trade_shares`);
  `TradePctNav` = that trade as a fraction of NAV. `MarginalShares` = the
  **rule-allowed** portion of this trade's replication (the marginal clamped to
  the position's rule limits); `DriftShares` = the remainder of the actual trade
  (pre-existing drift correction). They **sum to `TradeQuantity`**. `LimitingRule`
  names the binding rule (null when unconstrained).
- **`totals`** — `TradeQuantity` summed across **all** rows (base + SMAs), split by
  swap flag: `Direct` (not on swap) and `Swap` (on swap).
- **`warnings`** — any SMA that errored out, with its cause (see below).

### Errors
- `422` — required data missing/non-finite. Body lists the offenders so Excel can
  highlight them:
  ```json
  { "error": "incomplete_market_data",
    "missing": [ { "id": "xyz us equity", "field": "PX_LAST" } ] }
  ```
- `409` — `request_id` unknown/expired (re-run call 1).
- `400` — malformed request; `404` — unknown portfolio.

---

## Notes / decisions baked in

- **Two calls, not one**, because the client cannot derive the involved-securities
  set (holdings live server-side).
- **Strict validation** in call 2: a missing security or field is rejected, never
  silently defaulted — a wrong price is a wrong compliance decision.
- **Serialized per worker.** Call 2 mutates the global active provider
  (`set_security_data_provider`) for the duration of the request. plumber serves
  one request at a time per process, so run a small **pool of workers** behind a
  reverse proxy for concurrency; each worker has its own structural registry.
- **`request_id` state** can be an in-process TTL cache (e.g. 5 min). If a request
  hits a different worker for call 2 than call 1, either use sticky routing or
  make the manifest recomputable from the request body (it is — call 2 carries
  `portfolio`/`trades`/`flow_to_derived`, so the server can recompute and validate
  without shared state). Recompute is simpler than sticky sessions — prefer it.
