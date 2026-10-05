# dev/api/test-harness.R  (DRAFT)
#
# Exercise both endpoint handlers end to end with a FABRICATED market-data
# payload — no HTTP, no Excel, no Bloomberg. This mirrors the real two-call flow:
#   1. handle_required_inputs() -> the securities/fields the client must price
#   2. fabricate the values Excel/BDP would return for exactly those
#   3. handle_proposed_trade() -> proposed trades
#
# Run from the repo root:
#   Rscript dev/api/test-harness.R
# or source() it in an interactive session after devtools::load_all() on both
# replikit and replikitdata.
#
# EDIT THESE to a real base portfolio + a security it can trade:
PORTFOLIO <- "ccmf"
TRADE <- data.frame(
  security = "www us equity", qty = 100, swap = FALSE,
  stringsAsFactors = FALSE
)

library(replikit)
source("dev/api/request-provider.R", local = TRUE)
source("dev/api/handlers.R", local = TRUE)

# --- warm structural state (no Bloomberg) -----------------------------------
replikitdata::db_connect()   # establish the package-level DB connection first
.con <- replikitdata::get_db_connection()
replikitdata::load_all_portfolios_from_db(con = .con, refresh_market_data = FALSE)

# --- 1) /required-inputs ----------------------------------------------------
req <- handle_required_inputs(list(
  portfolio = PORTFOLIO, trades = TRADE, flow_to_derived = TRUE
))
stopifnot(req$status == 200L)
ids    <- vapply(req$body$securities, function(s) s$id, character(1))
fields <- unlist(req$body$fields)
cat(sprintf("[1] required-inputs: %d securities, %d fields\n",
            length(ids), length(fields)))

# --- 2) fabricate the market data Excel/BDP would return --------------------
# Deterministic stand-ins: equities priced flat, no delta; rule fields = 1.
fake_field <- function(field, n) {
  if (identical(field, "PX_LAST")) return(rep(100, n))
  if (identical(field, "OP006"))   return(rep(NA_real_, n))
  rep(1, n)
}
market_data <- data.frame(id = ids, stringsAsFactors = FALSE, check.names = FALSE)
for (f in fields) market_data[[f]] <- fake_field(f, length(ids))

# --- 3) /proposed-trade -----------------------------------------------------
resp <- handle_proposed_trade(list(
  request_id      = req$body$request_id,
  portfolio       = PORTFOLIO,
  trades          = TRADE,
  flow_to_derived = TRUE,
  market_data     = market_data
))
if (resp$status != 200L) {
  cat(sprintf("[3] proposed-trade FAILED (%d):\n", resp$status))
  utils::str(resp$body)
  quit(status = 1, save = "no")
}
cat("[3] proposed-trade OK:\n")
print(resp$body$proposed_trades)

# --- 4) negative test: drop a required price -> expect 422 ------------------
bad <- market_data
bad$PX_LAST[1] <- NA_real_
neg <- handle_proposed_trade(list(
  request_id = req$body$request_id, portfolio = PORTFOLIO, trades = TRADE,
  flow_to_derived = TRUE, market_data = bad[-1, , drop = FALSE]  # also drop the row
))
stopifnot(neg$status == 422L, neg$body$error == "incomplete_market_data")
cat(sprintf("[4] validation OK: 422 with %d missing\n", length(neg$body$missing)))

cat("\nAll harness checks passed.\n")
