# dev/api/auth.R  (DRAFT)
#
# Login/token auth. Users authenticate with their OWN Postgres credentials
# (validated by a throwaway DB connection) and receive a short-lived signed
# token used as the bearer on subsequent calls. Authenticate-only: data is
# served from the shared warm registry, not per-user, so a successful login just
# proves the caller has a valid DB account.
#
# SOURCE ORDER: after handlers.R (uses %||%).
# Requires REPLIKIT_API_SECRET (HMAC signing key) in the environment. The server
# still needs its own PG_PASSWORD (service account) for the warm structural load;
# that is separate from these per-user logins.

# --- public paths / header parsing ------------------------------------------

# Served without a token: login, health, and the auto-generated docs.
.is_public_path <- function(path) {
  identical(path, "/login") ||
    identical(path, "/health") ||
    startsWith(path, "/__docs__") ||
    startsWith(path, "/__swagger__") ||
    identical(path, "/openapi.json")
}

.bearer_token <- function(auth_header) {
  sub("(?i)^Bearer\\s+", "", auth_header %||% "", perl = TRUE)
}

# --- DB credential validation -----------------------------------------------

# Supabase's pooler expects "<role>.<project-ref>". If the caller gave a bare
# role, append the project ref from the configured PGUSER default. Pass a
# username that already contains "." to bypass.
.pooler_user <- function(username) {
  if (grepl(".", username, fixed = TRUE)) return(username)
  ref <- sub("^[^.]+\\.", "", Sys.getenv("PGUSER", ""))
  if (nzchar(ref)) paste0(username, ".", ref) else username
}

# Open a throwaway connection with the user's own credentials to prove they can
# reach the DB, then close it. Does NOT touch the cached service connection used
# by the warm registry (that is what db_connect()/get_db_connection() manage).
.validate_db_login <- function(username, password) {
  if (!nzchar(username) || !nzchar(password)) return(FALSE)
  isTRUE(tryCatch({
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname   = Sys.getenv("PGDATABASE", "postgres"),
      host     = Sys.getenv("PG_HOST", "aws-1-us-east-1.pooler.supabase.com"),
      port     = as.integer(Sys.getenv("PGPORT", "6543")),
      user     = .pooler_user(username),
      password = password,
      sslmode  = Sys.getenv("PGSSLMODE", "require")
    )
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
    DBI::dbIsValid(con)
  }, error = function(e) FALSE))
}

# --- signed token (stateless, HMAC-SHA256) ----------------------------------

.token_ttl_sec <- function() {
  as.integer(Sys.getenv("REPLIKIT_TOKEN_TTL_SEC", "43200"))  # 12h default
}

.b64url_enc <- function(raw_or_chr) {
  s <- openssl::base64_encode(raw_or_chr)
  chartr("+/", "-_", sub("=+$", "", s))
}
.b64url_dec <- function(s) {
  s <- chartr("-_", "+/", s)
  pad <- (4 - nchar(s) %% 4) %% 4
  openssl::base64_decode(paste0(s, strrep("=", pad)))
}

.sign <- function(msg, secret) {
  .b64url_enc(openssl::sha256(charToRaw(msg), key = secret))  # HMAC when key set
}

# Constant-time compare of two equal-length strings.
.const_time_eq <- function(a, b) {
  ab <- charToRaw(a); bb <- charToRaw(b)
  if (length(ab) != length(bb)) return(FALSE)
  sum(bitwXor(as.integer(ab), as.integer(bb))) == 0L
}

.issue_token <- function(username) {
  secret <- Sys.getenv("REPLIKIT_API_SECRET")
  if (!nzchar(secret)) stop("REPLIKIT_API_SECRET not set")
  payload <- as.character(jsonlite::toJSON(
    list(sub = username, exp = as.integer(Sys.time()) + .token_ttl_sec()),
    auto_unbox = TRUE
  ))
  p <- .b64url_enc(charToRaw(payload))
  paste0(p, ".", .sign(p, secret))
}

# Returns the username if the token's signature is valid and it has not expired,
# else NULL.
.verify_token <- function(token) {
  secret <- Sys.getenv("REPLIKIT_API_SECRET")
  if (!nzchar(secret) || !nzchar(token)) return(NULL)
  parts <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(parts) != 2) return(NULL)
  if (!.const_time_eq(parts[2], .sign(parts[1], secret))) return(NULL)
  claims <- tryCatch(
    jsonlite::fromJSON(rawToChar(.b64url_dec(parts[1]))),
    error = function(e) NULL
  )
  if (is.null(claims) || is.null(claims$exp)) return(NULL)
  if (as.numeric(claims$exp) < as.numeric(Sys.time())) return(NULL)
  claims$sub
}
