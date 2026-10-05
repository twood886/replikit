#' Null/empty coalescing
#' Returns y if x is NULL, length 0, or NA; otherwise returns x.
#' @param x An object to check.
#' @param y A fallback value to return if x is NULL, length 0, or NA.
#' @export
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}


#' Test Whether an Instrument Type is an Option
#'
#' Securities carry instrument types from two vocabularies: the security data
#' provider (e.g. Bloomberg EX028, "Option") and the Enfusion-sourced
#' securities table ("Listed Option", "OTC Option"). Every option check must
#' accept both, so this predicate is the only place option types are listed.
#' @param type Character. Instrument type of a security.
#' @return Logical.
#' @export
.is_option_type <- function(type) {
  isTRUE(type %in% c("Option", "Listed Option", "OTC Option"))
}

#' Retrieve Registries from Package Namespace
#'
#' This function accesses the `registries` object from the namespace of the 
#' specified package (`yourpkg`). It assumes that the `registries` object 
#' exists within the package's namespace.
#'
#' @return The `registries` object from the namespace of the package `yourpkg`.
#' @examples
#' \dontrun{
#'   registries <- get_registries()
#' }
#' 
#' @export
get_registries <- function() {
  asNamespace("replikit")$registries
}

#' Get Data Fields Required by Registered SMA Rules
#'
#' Collects the unique Bloomberg field mnemonics declared by all rules in the
#' SMA rule registry.
#' @return Character vector of field mnemonics (NULL if no rules declare any).
#' @export
.rule_bbfields <- function() {
  rule_names <- ls(get_registries()$smarules)
  rules <- mget(
    rule_names,
    envir = get_registries()$smarules,
    inherits = TRUE
  )
  rules_bbfields_all <- sapply(rules, \(r) r$get_bbfields(), simplify = TRUE)
  unique(unlist(rules_bbfields_all, use.names = FALSE))
}

#' Update Data in Security Objects
#'
#' Refreshes price and delta for securities in the registry — and, by
#' default, the rule fields declared by registered SMA rules — using a single
#' batched request to the active security data provider. Prices and deltas
#' follow the same instrument-type rules as \code{Security$update_price()} and
#' \code{Security$update_delta()}: FixedIncome prices are 1 and non-Option
#' deltas are 1.
#'
#' @param update_fields Logical. If \code{TRUE} (default), rule fields are
#'  fetched in the same provider request and updated too. Set to \code{FALSE}
#'  when rule fields were just loaded (e.g. right after
#'  \code{load_all_portfolios_from_db()}, which already updates them).
#' @param security_ids Optional character vector of registry security ids to
#'  update. When \code{NULL} (default) every security in the registry is
#'  updated. Scope to a subset to refresh only the securities a single request
#'  touches (e.g. a per-request repricing in the API layer); the caller is
#'  responsible for including the underlyings of any in-scope options, whose
#'  prices in-scope options are derived from.
#' @export
update_security_data <- function(update_fields = TRUE, security_ids = NULL) {
  if (is.null(security_ids)) {
    security_ids <- ls(get_registries()$securities)
  }
  if (length(security_ids) == 0) return(invisible(NULL))

  rule_fields <- if (isTRUE(update_fields)) .rule_bbfields() else NULL
  bbdata <- get_security_data_provider()$get_fields(
    security_ids, c("PX_LAST", "OP006", rule_fields)
  )

  # Underlying securities are registry members, so options pick up fresh
  # underlying prices through their shared Security objects.
  for (id in security_ids) {
    security <- .security(id)
    type <- security$get_instrument_type()

    price <- if (identical(type, "FixedIncome")) 1 else bbdata[id, "PX_LAST"]
    delta <- if (.is_option_type(type)) bbdata[id, "OP006"] else 1
    if (!is.finite(delta)) delta <- 1

    security$set_price(price)
    security$set_delta(delta)
    for (f in rule_fields) {
      security$set_rule_data(f, bbdata[id, f])
    }
  }
  invisible(NULL)
}

#' Add Bloomberg Data for SMA Rules to Securities
#' @param sec_id Optional character vector of Security IDs to update. If NULL,
#'  all securities are updated.
#' @return TRUE (invisible)
#' @export
#' @include api-functions.R
#' @include class-security.R
update_bloomberg_fields <- function(sec_id = NULL) {
  rules_bbfields <- .rule_bbfields()
  if (is.null(rules_bbfields) || length(rules_bbfields) == 0) {
    return(invisible(TRUE))
  }
  if (is.null(sec_id)) {
    sec_id <- ls(get_registries()$securities)
  }
  invisible(lapply(sec_id, function(id) .security(id)))
  bbdata <- get_security_data_provider()$get_fields(sec_id, rules_bbfields)


  for (col in seq_len(ncol(bbdata))) {
    field <- colnames(bbdata)[col]
    for (row in seq_len(nrow(bbdata))) {
      id <- rownames(bbdata)[row]
      value <- bbdata[row, col]
      .security(id, create = FALSE)$set_rule_data(field, value)
    }
  }
  invisible(TRUE)
}

#' Convert Bloomberg Yellow Key to Security ID
#' @param yellow_key Character. Bloomberg Yellow Key (e.g., "AAPL US Equity").
#' @return Character. Corresponding Security ID.
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_match
#' @importFrom checkmate assert_character
#' @export
bbid_to_security_id <- function(id) {
  checkmate::assert_character(id)
  fraction_str <- "(?x)(-?\\d+(?:\\.\\d+)?)\\s*<\\s*(\\d+)\\s*/\\s*(\\d+)\\s*>"
  id_clean <- id %>%
    stringr::str_replace_all("<([A-Za-z]+)>", " \\1") %>%
    stringr::str_replace_all(
      fraction_str,
      function(m) {
        parts <- str_match(m, fraction_str)
        before <- as.numeric(parts[2])
        num <- as.numeric(parts[3])
        denom <- as.numeric(parts[4])
        format(before + num / denom, scientific = FALSE, trim = TRUE)
      }
    )
  id_clean
}


#' @title Get Tracking Portfolios
#' @description Get all SMAs that have a specified portfolio as any part of
#'   their base (including blended bases).
#' @param base_portfolio Character short name or Portfolio object
#' @returns list of SMA portfolios
#' @export
get_tracking_portfolios <- function(base_portfolio) {
  port_env <- tryCatch(
    registries$portfolios,
    error = function(e) {
      stop("Failed to access portfolio registry: ", conditionMessage(e))
    }
  )

  if (is.null(port_env)) return(NULL)

  if (checkmate::test_r6(base_portfolio, "Portfolio")) {
    base_portfolio_name <- base_portfolio$get_short_name()
  } else if (checkmate::test_character(base_portfolio, len = 1)) {
    base_portfolio_name <- base_portfolio
  } else {
    checkmate::assert_r6(base_portfolio, "Portfolio")
  }

  checkmate::assert_character(base_portfolio_name, len = 1)

  portfolio_names <- ls(port_env)
  portfolios <- sapply(
    portfolio_names,
    function(p) get(p, envir = port_env),
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  is_tracking <- vapply(
    portfolios,
    function(p) {
      tryCatch({
        base_list <- p$get_base_portfolios()
        if (is.null(base_list)) return(FALSE)
        base_names <- vapply(
          base_list, \(x) x$portfolio$get_short_name(), character(1)
        )
        base_portfolio_name %in% base_names
      }, error = function(e) FALSE)
    },
    logical(1)
  )

  portfolios[is_tracking]
}
