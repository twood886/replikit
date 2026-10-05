# dev/api/run-api.R  (DRAFT)
#
# Launch the replikit API server. Blocks the session while running.
#   Rscript dev/api/run-api.R                 # from the repo root
# or, in an interactive session:
#   source("dev/api/run-api.R")
#
# Warm-starts the structural registry from Supabase (no Bloomberg), then serves.
# Port defaults to 8000; override with the REPLIKIT_API_PORT env var.
# Hit it from another session with dev/api/http-client.R.

library(plumber)

# Dev stage: run the current SOURCE of both packages so core engine changes take
# effect on a plain restart with no reinstall. Order matters — replikit first
# (replikitdata depends on it). Flip back to library() + installed for release.
devtools::load_all(".", quiet = TRUE)
devtools::load_all("../replikitdata", quiet = TRUE)

port <- as.integer(Sys.getenv("REPLIKIT_API_PORT", "8000"))

if (!nzchar(Sys.getenv("REPLIKIT_API_SECRET"))) {
  message(
    "WARNING: REPLIKIT_API_SECRET is not set — protected endpoints will return ",
    "503 (auth_not_configured) and /login cannot mint tokens. Set it in ",
    ".Renviron to a long random string (e.g. uuid::UUIDgenerate()). /login, ",
    "/health and the docs stay public."
  )
}

# pr() parses plumber.R, which runs the warm-start (load structure, open .con)
# as it sources the file, then registers the annotated endpoints.
message("Warming structural registry (Supabase, no Bloomberg)...")
api <- plumber::pr("dev/api/plumber.R")
message("Ready. Serving on http://127.0.0.1:", port)
plumber::pr_run(api, host = "127.0.0.1", port = port)
