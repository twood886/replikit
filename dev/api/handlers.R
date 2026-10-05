# dev/api/handlers.R  (DRAFT)
#
# Transport-free request handlers. Each takes a parsed body (a list; `trades`
# and `market_data` are data.frames) and returns list(status = <int>, body = <list>).
# plumber.R wraps these with HTTP; test-harness.R calls them directly with a
# fabricated payload. Both share the warm structural registry and a `.con`
# Supabase connection defined by the caller (resolved by lexical scope at call
# time).
#
# SOURCE ORDER: source("request-provider.R") BEFORE this file — the handlers use
# apply_request_market_data() from there.

`%||%` <- function(a, b) if (is.null(a)) b else a

.new_request_id <- function() {
  if (requireNamespace("uuid", quietly = TRUE)) return(uuid::UUIDgenerate())
  paste0("req-", as.integer(Sys.time()), "-", sample.int(1e6, 1))
}

# Auth (login/token + DB validation) lives in auth.R, sourced by plumber.R.

# --- structural freshness / refresh -----------------------------------------
# Timestamp of the last structural (holdings/NAV/rules) load, surfaced in every
# response so staleness is always visible. Set at warm-start and by /refresh.
.state <- new.env(parent = emptyenv())
.state$structure_as_of <- NA

set_structure_as_of <- function(t = Sys.time()) .state$structure_as_of <- t

data_as_of_str <- function() {
  if (inherits(.state$structure_as_of, "POSIXct")) {
    format(.state$structure_as_of, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  } else {
    NA_character_
  }
}

# Empty the in-memory registries so a reload rebuilds them from current DB state.
# A plain re-run of load_all_* would keep the existing objects (.portfolio() and
# friends short-circuit on exists()), so /refresh must clear first.
clear_registries <- function() {
  regs <- replikit::get_registries()
  for (nm in c("portfolios", "securities", "smarules")) {
    e <- regs[[nm]]
    if (is.environment(e)) rm(list = ls(e, all.names = TRUE), envir = e)
  }
  invisible(NULL)
}

# --- scope helpers ----------------------------------------------------------

# Involved securities = trade securities + holdings of base + (optionally) the
# derived SMAs. Lowercase ids, matching the registry.
.involved_securities <- function(portfolio_name, trade_secs, flow_to_derived = TRUE) {
  base <- replikit::.portfolio(portfolio_name, create = FALSE)
  ids  <- vapply(base$get_position(), function(p) p$get_id(), character(1))

  if (isTRUE(flow_to_derived)) {
    derived <- tryCatch(replikit::get_tracking_smas(base), error = function(e) list())
    for (d in derived) {
      ids <- c(ids, vapply(d$get_position(), function(p) p$get_id(), character(1)))
    }
  }
  unique(tolower(c(ids, trade_secs)))
}

# Split ids into OTC options (server prices these) vs everything else (client BDPs).
.split_otc <- function(ids) {
  env <- replikit::get_registries()$securities
  is_otc <- vapply(ids, function(id) {
    identical(
      tryCatch(get(id, envir = env)$get_instrument_type(), error = function(e) ""),
      "OTC Option"
    )
  }, logical(1))
  list(otc = ids[is_otc], bdp = ids[!is_otc])
}

# Full price scope for a request, in one Supabase lookup. Involved securities
# PLUS the underlyings of any in-scope OTC options: the client isn't asked to
# price the OTC option itself, but its underlying's price is needed both to
# derive the option price and to compute the option's exposure
# (|delta| * underlying_price, read off the underlying Security). Returns the
# augmented involved set and the OTC specs so callers don't fetch twice.
.request_scope <- function(portfolio, trade_secs, flow_to_derived) {
  involved  <- .involved_securities(portfolio, trade_secs, flow_to_derived)
  otc_specs <- replikitdata::otc_specs_for(involved, .con)
  underlyings <- tolower(vapply(
    otc_specs, function(s) s$underlying_id, character(1), USE.NAMES = FALSE
  ))
  list(involved = unique(c(involved, underlyings)), otc_specs = otc_specs)
}

# --- handlers ---------------------------------------------------------------

# POST /required-inputs — which securities/fields must the client price?
handle_required_inputs <- function(body) {
  trade_secs <- tolower(body$trades$security)
  scope      <- .request_scope(body$portfolio, trade_secs, body$flow_to_derived %||% TRUE)
  split      <- .split_otc(scope$involved)   # OTC -> server; the rest (incl.
                                             # OTC underlyings) -> client BDP.

  # The registry id (the fetch_securities bbid) is BDP-valid as-is for every
  # instrument type and BDP is case-insensitive, so no reverse mapping is needed.
  fields <- unique(c("PX_LAST", "OP006", replikit:::.rule_bbfields()))

  list(status = 200L, body = list(
    request_id  = .new_request_id(),
    as_of       = format(Sys.time(), tz = "UTC"),
    data_as_of  = data_as_of_str(),
    fields      = as.list(fields),
    securities  = lapply(split$bdp, function(id) list(id = id, bdp_id = id))
  ))
}

# One output row from an sma$replicate_trade_qty() result (or the base echo).
# marginal/drift decompose the (unconstrained) trade; TradeQuantity is the
# rule-constrained total actually proposed. Share counts are whole; TradePctNav
# is a fraction of NAV (Excel formats it as a percentage).
.trade_row <- function(portfolio, security, trade_qty, trade_pct_nav = NA_real_,
                       marginal = NA_real_, drift = NA_real_,
                       current = NA_real_, target = NA_real_,
                       limiting_rule = NA_character_, replacement = NA_character_) {
  data.frame(
    Portfolio      = portfolio,
    Security       = security,
    TradeQuantity  = trade_qty,
    TradePctNav    = trade_pct_nav,
    MarginalShares = marginal,
    DriftShares    = drift,
    CurrentShares  = current,
    TargetShares   = target,
    LimitingRule   = limiting_rule,
    Replacement    = replacement,
    stringsAsFactors = FALSE
  )
}

# One current-holdings row
# (Portfolio | Security | Shares Held | % of NAV | Swap Flag | Replacement).
.holding_row <- function(portfolio, security, shares, pct_nav, swap,
                         replacement = NA_character_) {
  data.frame(
    Portfolio   = portfolio,
    Security    = security,
    SharesHeld  = shares,
    PctNav      = pct_nav,
    Swap        = swap,
    Replacement = replacement,
    stringsAsFactors = FALSE
  )
}

# A security's replacement role in an SMA, as a human label for Excel: "replaces
# X" when it is a replacement target, "replaced by Y" when it is a replaced
# source, else NA. Lets the results sheets show why an unexpected name appears.
.replacement_note <- function(portfolio, sec) {
  src <- tryCatch(portfolio$get_replaced_security(sec), error = function(e) NULL)
  if (!is.null(src) && length(src)) {
    return(paste("replaces", paste(src, collapse = ", ")))
  }
  reps <- tryCatch(portfolio$get_replacement_security(), error = function(e) list())
  if (sec %in% names(reps)) {
    return(paste("replaced by", paste(reps[[sec]]$security, collapse = ", ")))
  }
  NA_character_
}

# limiting_rule from .unconst_to_const_shares is a NAMED number (name = rule) when
# constrained, or an unnamed NA_character_ when unconstrained. We want the name.
.limiting_rule_name <- function(lr) {
  if (length(lr) && !is.null(names(lr))) names(lr)[1] else NA_character_
}

# Replication price (|delta| * underlying price) — the exposure the engine uses
# for weights, so it is the right basis for a "% of NAV".
.repl_price <- function(sec) {
  tryCatch(
    replikit::.security(sec, create = FALSE)$get_replication_price(),
    error = function(e) NA_real_
  )
}

# shares * price / nav, as a fraction of NAV (NA when it can't be computed).
.pct_nav <- function(shares, price, nav) {
  if (!is.finite(price) || !is.finite(nav) || nav == 0) return(NA_real_)
  shares * price / nav
}

# Current position swap flag for a portfolio (default when it holds no position).
.pos_swap <- function(portfolio, sec, dflt = FALSE) {
  tryCatch(portfolio$get_position(sec)$get_swap(), error = function(e) dflt)
}

# Current position quantity for a portfolio (0 if it holds none).
.pos_qty <- function(portfolio, sec) {
  tryCatch(portfolio$get_position(sec)$get_qty(), error = function(e) 0)
}

# Pre-warm CVXR: the first CVXR problem in a process pays a ~1s one-time cost to
# initialize its S7 machinery. Solve a trivial problem at startup (exercises the
# same Variable/Problem/sum_squares/psolve+OSQP path optimize_sma uses) so the
# first real rebalance is fast. Data-independent, so it always warms.
.prewarm_cvxr <- function() {
  tryCatch({
    v <- CVXR::Variable(2)
    p <- CVXR::Problem(CVXR::Minimize(CVXR::sum_squares(v)), list(v >= 0))
    CVXR::psolve(p, solver = "OSQP")
  }, error = function(e) NULL)
  invisible(NULL)
}

# POST /proposed-trade — price from posted market data, replicate the base trade
# into each tracking SMA subject to its rules, return the proposed trades.
handle_proposed_trade <- function(body) {
  trades      <- body$trades         # data.frame: security, qty[, swap]
  market_data <- body$market_data    # data.frame: id, PX_LAST, ...
  fld         <- body$flow_to_derived %||% TRUE

  scope <- .request_scope(body$portfolio, tolower(trades$security), fld)

  # Strict validation: every non-OTC security in scope (incl. OTC underlyings)
  # must be priced. A missing price is a wrong compliance decision — reject it.
  split   <- .split_otc(scope$involved)
  missing <- setdiff(split$bdp, tolower(market_data$id))
  if (length(missing) > 0) {
    return(list(status = 422L, body = list(
      error   = "incomplete_market_data",
      missing = lapply(missing, function(id) list(id = id, field = "PX_LAST"))
    )))
  }

  # Price the whole scope from the payload (underlyings included so OTC exposures
  # are correct), then run the engine against the warm structure.
  apply_request_market_data(market_data, scope$involved, scope$otc_specs)

  base      <- replikit::.portfolio(body$portfolio, create = FALSE)
  base_name <- base$get_short_name()
  smas <- if (isTRUE(fld)) {
    tryCatch(replikit::get_tracking_smas(base), error = function(e) list())
  } else list()

  rows      <- list()
  holdings  <- list()
  warnings  <- list()
  total_direct <- 0
  total_swap   <- 0

  # Accumulate a trade into the Direct/Swap totals by its swap flag.
  add_total <- function(shares, swap) {
    if (isTRUE(swap)) total_swap <<- total_swap + shares
    else              total_direct <<- total_direct + shares
  }

  base_nav <- base$get_nav()

  # Base trade quantities keyed by security, applied to the trading base. Passed
  # whole to each SMA so replacement overflow (e.g. ET -> PAGP) routes correctly.
  base_trades <- setNames(as.numeric(trades$qty), tolower(trades$security))

  # 1) Base rows + base holdings, one per input trade.
  for (i in seq_len(nrow(trades))) {
    sec   <- tolower(trades$security[i])
    qty   <- trades$qty[i]
    price <- .repl_price(sec)
    base_swap <- if (!is.null(trades$swap)) isTRUE(trades$swap[i]) else FALSE

    base_cur <- .pos_qty(base, sec)
    holdings[[length(holdings) + 1L]] <- .holding_row(
      base_name, sec, base_cur, .pct_nav(base_cur, price, base_nav), base_swap
    )
    rows[[length(rows) + 1L]] <- .trade_row(
      base_name, sec, qty, trade_pct_nav = .pct_nav(qty, price, base_nav),
      current = base_cur, target = base_cur + qty
    )
    add_total(qty, base_swap)
  }

  # 2) Per-SMA replication, replacement-aware: one call routes the whole trade
  #    set and returns rows for the traded securities AND any replacement targets
  #    that receive overflow (so a restricted name shows its replacement buy
  #    instead of the replacement being liquidated). Capture (don't swallow)
  #    failures so a dropped SMA is visible with its cause.
  for (sma in smas) {
    res <- tryCatch(
      sma$get_trade_constructor()$replicate_base_trades(sma, base_trades, base_name),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      warnings[[length(warnings) + 1L]] <- list(
        portfolio = sma$get_short_name(), error = conditionMessage(res)
      )
      next
    }
    sma_nav  <- sma$get_nav()
    sma_name <- sma$get_short_name()
    for (sec in names(res)) {
      r        <- res[[sec]]
      price    <- .repl_price(sec)
      sma_swap <- .pos_swap(sma, sec, dflt = FALSE)
      repl_note <- .replacement_note(sma, sec)
      holdings[[length(holdings) + 1L]] <- .holding_row(
        sma_name, sec, r$current_shares,
        .pct_nav(r$current_shares, price, sma_nav), sma_swap, repl_note
      )
      # Constrained marginal: rule-allowed portion of the direct replication;
      # drift is the remainder, so Marginal + Drift = TradeQuantity. For a routed
      # replacement target the direct marginal is 0, so the buy shows as drift.
      cmarg <- round(
        min(max(r$current_shares + r$marginal_shares, r$min_allowed_shares),
            r$max_allowed_shares) - r$current_shares
      )
      rows[[length(rows) + 1L]] <- .trade_row(
        sma_name, r$security_id, r$trade_shares,
        trade_pct_nav = .pct_nav(
          r$constrained_target_shares - r$current_shares, price, sma_nav
        ),
        marginal = cmarg, drift = r$trade_shares - cmarg,
        current = r$current_shares, target = r$constrained_target_shares,
        limiting_rule = .limiting_rule_name(r$limiting_rule),
        replacement = repl_note
      )
      add_total(r$trade_shares, sma_swap)
    }
  }

  list(status = 200L, body = list(
    request_id      = body$request_id,
    as_of           = format(Sys.time(), tz = "UTC"),
    data_as_of      = data_as_of_str(),
    holdings        = do.call(rbind, holdings),
    proposed_trades = do.call(rbind, rows),
    totals          = list(Direct = total_direct, Swap = total_swap),
    warnings        = warnings
  ))
}

# POST /rebalance — optimize every tracking SMA of a base to target (full CVXR),
# subject to its rules, and return: the per-SMA trades to reach target; the top
# movers (securities ranked by avg |target %NAV - current %NAV| across SMAs); and
# Direct/Swap totals. Prices the WHOLE book (all SMA holdings), not just a trade.
handle_rebalance <- function(body, top_n = NULL) {
  if (is.null(top_n)) top_n <- as.integer(body$top_n %||% 20L)
  market_data <- body$market_data
  portfolio   <- body$portfolio

  # Whole-book price scope: holdings of the base + all its tracking SMAs.
  scope <- .request_scope(portfolio, character(0), TRUE)
  split <- .split_otc(scope$involved)
  missing <- setdiff(split$bdp, tolower(market_data$id))
  if (length(missing) > 0) {
    return(list(status = 422L, body = list(
      error   = "incomplete_market_data",
      missing = lapply(missing, function(id) list(id = id, field = "PX_LAST"))
    )))
  }
  apply_request_market_data(market_data, scope$involved, scope$otc_specs)

  base <- replikit::.portfolio(portfolio, create = FALSE)
  smas <- tryCatch(replikit::get_tracking_smas(base), error = function(e) list())

  base_nav <- base$get_nav()

  rows     <- list()
  warnings <- list()
  mv_sec   <- character(0)   # per-appearance security id, for top-mover averaging
  mv_drift <- numeric(0)     # matching |target%NAV - current%NAV|
  sma_order <- character(0)  # SMA short names, in encounter order (= column order)
  wt        <- list()        # wt[[sec]][[sma_name]] = security's current %NAV in that SMA
  total_direct <- 0
  total_swap   <- 0

  for (sma in smas) {
    res <- tryCatch(sma$get_trade_constructor()$optimize_sma(sma),
                    error = function(e) e)
    if (inherits(res, "error")) {
      warnings[[length(warnings) + 1L]] <- list(
        portfolio = sma$get_short_name(), error = conditionMessage(res)
      )
      next
    }
    nav      <- sma$get_nav()
    sma_name <- sma$get_short_name()
    if (!(sma_name %in% sma_order)) sma_order <- c(sma_order, sma_name)
    for (sec in names(res$shares)) {
      price   <- .repl_price(sec)
      cur_q   <- .pos_qty(sma, sec)
      tgt_q   <- res$shares[[sec]]
      trade   <- round(tgt_q - cur_q)
      tgt_pct <- res$weights[[sec]] %||% NA_real_
      cur_pct <- .pct_nav(cur_q, price, nav)

      mv_sec   <- c(mv_sec, sec)
      mv_drift <- c(mv_drift, abs((tgt_pct %||% 0) - (cur_pct %||% 0)))
      if (is.null(wt[[sec]])) wt[[sec]] <- list()
      wt[[sec]][[sma_name]] <- list(
        CurrentWeight = cur_pct,   # security's current %NAV in this SMA
        TargetWeight  = tgt_pct,   # optimizer's rule-constrained target %NAV
        CurrentShares = cur_q,
        TradeShares   = trade      # shares to trade to reach target (+ buy / - sell)
      )

      if (abs(trade) >= 1) {
        swap <- .pos_swap(sma, sec, FALSE)
        rows[[length(rows) + 1L]] <- data.frame(
          Portfolio     = sma_name,
          Security      = sec,
          TradeQuantity = trade,
          TradePctNav   = tgt_pct - (cur_pct %||% NA_real_),
          CurrentShares = cur_q,
          TargetShares  = tgt_q,
          stringsAsFactors = FALSE
        )
        if (isTRUE(swap)) total_swap <- total_swap + trade
        else              total_direct <- total_direct + trade
      }
    }
  }

  # Top movers: ranked by avg |target%NAV - current%NAV| across SMAs, showing the
  # security's weight in the base fund and its weight in each individual SMA
  # (one column per SMA; column order comes from top_movers_smas).
  top_movers <- list()
  if (length(mv_sec)) {
    agg    <- tapply(mv_drift, mv_sec, mean)
    take   <- utils::head(order(agg, decreasing = TRUE), top_n)
    nms    <- names(agg)
    top_movers <- lapply(take, function(i) {
      sec <- nms[i]
      sw  <- lapply(sma_order, function(nm) {
        v <- wt[[sec]][[nm]]                          # per-SMA weight + detail
        if (is.null(v)) {                             # not held/targeted by that SMA
          list(CurrentWeight = 0, TargetWeight = 0, CurrentShares = 0, TradeShares = 0)
        } else v
      })
      names(sw) <- sma_order
      list(
        Security   = sec,
        BaseWeight = .pct_nav(.pos_qty(base, sec), .repl_price(sec), base_nav),
        SmaWeights = sw
      )
    })
  }

  list(status = 200L, body = list(
    request_id       = body$request_id,
    as_of            = format(Sys.time(), tz = "UTC"),
    data_as_of       = data_as_of_str(),
    rebalance_trades = if (length(rows)) do.call(rbind, rows) else NULL,
    top_movers       = top_movers,
    top_movers_smas  = as.list(sma_order),            # column order for per-SMA weights
    totals           = list(Direct = total_direct, Swap = total_swap),
    warnings         = warnings
  ))
}
