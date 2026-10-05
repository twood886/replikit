# dev/api/request-provider.R  (DRAFT — not part of the package build yet)
#
# Assembles the per-request SecurityDataProvider from the market data the Excel
# client posted. No new provider class is needed: StaticDataProvider already
# serves in-memory client data, and CompositeDataProvider already layers
# server-side OTC-option overrides on top of a primary. We just wire them.
#
#   market_data (client, via BDP)  ->  StaticDataProvider   ] primary
#   OTC options  (server, Enfusion) ->  register_otc_option ] overrides
#
# The composite is set as the active provider for the request, then
# update_security_data(sec_id = involved) pushes prices/deltas/fields onto the
# already-hydrated Security objects. See NOTE on update_security_data scoping.

library(replikit)

#' Build the active provider for one request from posted market data.
#'
#' @param market_data data.frame with column `id` (lowercase bbid) plus one
#'   column per field mnemonic (at least `PX_LAST`; `OP006` for options; any
#'   rule bbfields). This is `market_data` from POST /proposed-trade, rectangular.
#' @param otc_specs Optional list of OTC-option override specs the server pulls
#'   from Supabase/Enfusion (NOT from the client). Each element:
#'   list(id=, underlying_id=, delta=, fields=list()). The underlying MUST be
#'   present in `market_data` (the client BDP'd it) so the composite can derive
#'   the option's price from the live underlying price.
#' @return A configured provider (StaticDataProvider, or CompositeDataProvider
#'   when there are OTC overrides). Caller passes it to
#'   set_security_data_provider().
build_request_provider <- function(market_data, otc_specs = list()) {
  stopifnot(is.data.frame(market_data), "id" %in% names(market_data))
  field_cols <- setdiff(names(market_data), "id")

  static <- replikit::StaticDataProvider$new()
  for (i in seq_len(nrow(market_data))) {
    row <- market_data[i, , drop = FALSE]
    id  <- tolower(row$id)

    px <- if ("PX_LAST" %in% field_cols) suppressWarnings(as.numeric(row$PX_LAST)) else NA_real_
    dl <- if ("OP006"   %in% field_cols) suppressWarnings(as.numeric(row$OP006))   else NA_real_

    # Everything except the canonical price/delta becomes a named rule field.
    extra <- field_cols[!field_cols %in% c("PX_LAST", "OP006")]
    fields <- stats::setNames(
      lapply(extra, function(f) row[[f]]),
      extra
    )

    static$add_security(
      sec_id = id,
      price  = ifelse(is.finite(px), px, NA_real_),
      delta  = ifelse(is.finite(dl), dl, NA_real_),
      fields = fields
    )
  }

  if (length(otc_specs) == 0) return(static)

  composite <- replikit::CompositeDataProvider$new(primary = static)
  for (spec in otc_specs) {
    composite$register_otc_option(
      sec_id        = tolower(spec$id),
      underlying_id = tolower(spec$underlying_id),
      delta         = spec$delta,
      fields        = spec$fields %||% list()
    )
  }
  composite
}

#' Price the involved securities from posted market data, then run the engine.
#'
#' @param involved_ids Character vector of the securities the request touches
#'   (base + derived holdings + trade securities) — the same set returned by
#'   /required-inputs, plus any OTC underlyings.
#' @return invisible(NULL); the Security objects in the registry are repriced.
apply_request_market_data <- function(market_data, involved_ids, otc_specs = list()) {
  provider <- build_request_provider(market_data, otc_specs)
  replikit::set_security_data_provider(provider)

  # Scope the repricing to this request's securities. Passing the whole registry
  # would error on any security the client did not send data for, since the
  # per-request StaticDataProvider only knows the posted rows. `involved_ids`
  # must already include the underlyings of any in-scope options.
  replikit::update_security_data(update_fields = TRUE, security_ids = involved_ids)
  invisible(NULL)
}
