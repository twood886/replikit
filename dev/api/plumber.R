# dev/api/plumber.R  (DRAFT skeleton)
#
# Run with:
#   library(plumber); pr("dev/api/plumber.R") |> pr_run(port = 8000)
#
# Thin HTTP layer over the handlers in handlers.R. Structural state is hydrated
# ONCE at process start and kept warm; market data comes per request from the
# Excel client, so this process needs NO Bloomberg connection.

library(plumber)
library(replikit)
# plumber evaluates this file with the working directory set to its own folder,
# so source siblings by bare name (not "dev/api/...").
source("request-provider.R", local = TRUE)
source("handlers.R", local = TRUE)
source("auth.R", local = TRUE)

# --- warm structural hydrate (no market data) -------------------------------
# refresh_market_data = FALSE skips the Bloomberg round trip and the global OTC
# override setup: structure only. Keeps a warm Supabase connection for
# per-request NAV/OTC-spec lookups. db_connect() must precede get_db_connection()
# (it establishes the package-level connection from .Renviron creds).
replikitdata::db_connect()
.con <- replikitdata::get_db_connection()
replikitdata::load_all_portfolios_from_db(con = .con, refresh_market_data = FALSE)
set_structure_as_of()   # stamp when the holdings snapshot was loaded
.prewarm_cvxr()         # one-time CVXR init so the first /rebalance is fast

# --- auth filter (runs before every route) ----------------------------------
# Bearer token issued by /login (signed, expiring). /login, /health and the docs
# are public; everything else needs a valid token. Fails closed (503) if the
# signing secret is not configured.
#* @filter auth
function(req, res) {
  if (.is_public_path(req$PATH_INFO)) return(plumber::forward())

  if (!nzchar(Sys.getenv("REPLIKIT_API_SECRET"))) {
    res$status <- 503L
    return(list(error = "auth_not_configured"))
  }
  if (is.null(.verify_token(.bearer_token(req$HTTP_AUTHORIZATION)))) {
    res$status <- 401L
    res$setHeader("WWW-Authenticate", "Bearer")
    return(list(error = "unauthorized"))
  }
  plumber::forward()
}

# --- endpoints (thin: parse -> handler -> status) ---------------------------

#* @get /health
function() list(status = "ok", as_of = format(Sys.time(), tz = "UTC"),
                data_as_of = data_as_of_str())

#* Rebuild the in-memory registry from current DB state (holdings/NAV/rules).
#* Call after positions update in Supabase, instead of restarting the server.
#* @post /refresh
#* @serializer unboxedJSON
function(req, res) {
  clear_registries()
  replikitdata::db_connect()   # refresh the DB connection in case it went stale
  replikitdata::load_all_portfolios_from_db(
    con = replikitdata::get_db_connection(), refresh_market_data = FALSE
  )
  set_structure_as_of()
  list(status = "refreshed", data_as_of = data_as_of_str())
}

#* Exchange DB username/password for a short-lived bearer token.
#* @post /login
#* @serializer unboxedJSON
function(req, res) {
  body <- jsonlite::fromJSON(req$postBody, simplifyVector = TRUE)
  if (!.validate_db_login(body$username %||% "", body$password %||% "")) {
    res$status <- 401L
    return(list(error = "invalid_credentials"))
  }
  list(
    token      = .issue_token(body$username),
    token_type = "Bearer",
    expires_in = .token_ttl_sec()
  )
}

#* @post /required-inputs
#* @serializer unboxedJSON
function(req, res) {
  out <- handle_required_inputs(jsonlite::fromJSON(req$postBody, simplifyVector = TRUE))
  res$status <- out$status
  out$body
}

#* @post /proposed-trade
#* @serializer unboxedJSON
function(req, res) {
  out <- handle_proposed_trade(jsonlite::fromJSON(req$postBody, simplifyVector = TRUE))
  res$status <- out$status
  out$body
}

#* @post /rebalance
#* @serializer unboxedJSON
function(req, res) {
  out <- handle_rebalance(jsonlite::fromJSON(req$postBody, simplifyVector = TRUE))
  res$status <- out$status
  out$body
}
