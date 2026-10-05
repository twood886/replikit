#' Retrieve or Create a Security Object
#'
#' This function retrieves a security object from the securities registry by its
#'  Bloomberg ID (bbid). If the security does not exist in the registry and
#'  `create` is set to `TRUE`, it attempts to create a new security object
#'  using Bloomberg data.
#'
#' @param sec_id A string representing the Bloomberg ID of the security.
#'  Must be a non-empty string.
#' @param create A logical value indicating whether to create a new security
#'  object if it does not already exist in the registry. Defaults to `TRUE`.
#' @param assign_to_registry A logical value indicating whether to assign
#'  the security object to the registry. Defaults to `TRUE`.
#'
#' @return If the security exists in the registry, it returns the corresponding
#'  security object. If the security does not exist and `create` is `FALSE`, it
#'  returns `NULL`. If `create` is `TRUE`, it creates a new security object and
#'  returns it. If `assign_to_registry` is `TRUE`, the new object is assigned to
#'  the registry.
#'
#' @details The function first validates the `bbid` parameter to ensure it is a
#'  valid string. It then checks if the security exists in the
#'  `registries$securities` environment. If the security does not exist and
#'  `create` is `TRUE`, it queries the active security data provider (see
#'  \code{\link{set_security_data_provider}}) for the security's data. If the
#'  provider indicates the security is not found, an error is raised.
#'  Otherwise, a new `Security` object is created and added to the registry.
#'
#' @examples
#' # Retrieve an existing security
#' security("AAPL US Equity")
#'
#' # Create a new security if it does not exist
#' security("MSFT US Equity", create = TRUE, assign_to_registry = TRUE)
#'
#' # Attempt to retrieve a security without creating it
#' security("GOOG US Equity", create = FALSE,  assign_to_registry = FALSE)
#'
#' @seealso \code{\link{Security}} for the Security class.
#'
#' @import checkmate
#' @export
.security <- function(sec_id, create = TRUE, assign_to_registry = TRUE) {
  env <- registries$securities
  # Fast path: an existing-security lookup with a valid single id (the
  # overwhelmingly common case in the hot rule-evaluation loop) returns before
  # the checkmate assertions below, which profiling showed dominate this call.
  if (is.character(sec_id) && length(sec_id) == 1L) {
    key <- tolower(sec_id)
    if (exists(key, envir = env, inherits = FALSE)) return(get(key, envir = env))
  }
  checkmate::assert_character(sec_id)
  checkmate::assert_logical(create)
  checkmate::assert_logical(assign_to_registry)
  sec_id <- tolower(sec_id)
  if (exists(sec_id, envir = env, inherits = FALSE)) {
    return(get(sec_id, envir = env))
  }
  if (!create) return(NULL)
  if (!get_security_data_provider()$security_exists(sec_id)) {
    stop("Security not found: ", sec_id)
  }
  security <- Security$new(sec_id)
  if (assign_to_registry) {
    assign(sec_id, security, envir = env)
  }
  invisible(security)
}

#' Create a Holding Object
#'
#' Constructs a new \code{\link{Holding}} object for a given security and
#' quantity. Validates inputs and delegates to \code{Holding$new()}.
#'
#' @param sec_id A string representing the security ID (ticker).
#' @param qty A numeric value representing the quantity of the holding.
#' @param swap A logical value indicating whether the holding is a swap.
#'   Defaults to \code{FALSE}.
#' @param custodian_acct_id (Optional) A string representing the custodian account ID.
#' @param trs_custodian_id (Optional) A string representing the TRS custodian ID.
#'
#' @return An object of class \code{Holding}.
#'
#' @seealso \code{\link{Holding}} for the Holding class.
#' @import checkmate
#' @export
.holding <- function(
  sec_id, qty, swap = FALSE, custodian_acct_id = NULL, trs_custodian_id = NULL
) {
  checkmate::assert_character(sec_id)
  sec_id <- tolower(sec_id)
  checkmate::assert_numeric(qty)
  checkmate::assert_flag(swap)
  checkmate::assert_character(custodian_acct_id, null.ok = TRUE)
  checkmate::assert_character(trs_custodian_id, null.ok = TRUE)
  Holding$new(sec_id, qty, swap, custodian_acct_id, trs_custodian_id)
}

#' Create or Retrieve a Position Object
#'
#' This function creates or updates a position in a specified portfolio.
#'  It ensures that the input parameters are valid and initializes a new
#'  position object.
#'
#' @param portfolio_name A string specifying the name of the portfolio.
#'  The portfolio must already exist.
#' @param sec_id A string representing the Bloomberg identifier (BBID) of the
#'  security. This will be converted to lowercase.
#' @param create A logical value indicating whether to create the position
#' @param assign_to_portfolio A logical value indicating whether to add the
#'  position to the portfolio. Defaults to `TRUE`.
#'
#' @return An object of class `Position` representing the created or updated
#'  position.
#'
#' @details The function validates the input parameters using assertion checks.
#'  If the security does not already exist, it will be created. The position is
#'  then initialized using the `Position$new` method.
#'
#' @examples
#' # Create a position with 100 shares of a security
#' .position("MyPortfolio", "AAPL US Equity")
#'
#' @seealso \code{\link{Position}} for the Position class.
#' @import checkmate
#'
#' @export
.position <- function(
  portfolio_name, sec_id, create = FALSE, assign_to_portfolio = FALSE
) {
  checkmate::assert_character(portfolio_name)
  checkmate::assert_string(sec_id)
  checkmate::assert_flag(create)
  checkmate::assert_flag(assign_to_portfolio)
  sec_id <- tolower(sec_id)
  portfolio <- .portfolio(portfolio_name, create = FALSE)
  position <- tryCatch(
    portfolio$get_position(sec_id),
    error = function(e) NULL
  )
  if (!is.null(position)) return(invisible(position))
  if (!create) stop("Position does not exist and create is set to FALSE")

  sec <- .security(sec_id, create = TRUE)
  position <- Position$new(portfolio_name, sec)
  if (assign_to_portfolio) {
    portfolio$add_position(position, overwrite = TRUE)
  }
  return(invisible(position))
}

#' Create or Retrieve a Portfolio Object
#'
#' This function creates a new portfolio object or retrieves an existing one
#' from the portfolio registry. If the portfolio does not exist and `create`
#' is set to `FALSE`, an error is raised.
#'
#' @param short_name A string representing the short name of the portfolio.
#'   Must be unique within the portfolio registry.
#' @param long_name A string representing the long name of the portfolio.
#'   Required if creating a new portfolio.
#' @param nav A numeric value representing the net asset value (NAV) of the
#'   portfolio. Defaults to 0.
#' @param positions A list of positions to initialize the portfolio with.
#'   Each position must inherit from the "Position" class. Defaults to an
#'   empty list.
#' @param create A logical value indicating whether to create the portfolio
#'   if it does not exist. Defaults to `FALSE`.
#' @param assign_to_registry A logical value indicating whether to assign the
#'  portfolio object to the registry. Defaults to `TRUE`.
#'
#' @return If the portfolio exists or is successfully created, the portfolio
#'   object is returned. Otherwise, an error is raised.
#'
#' @details The function checks for the existence of a portfolio in the
#'   `registries$portfolios` environment using the `short_name`. If the
#'   portfolio exists, it is retrieved and returned. If it does not exist
#'   and `create` is `TRUE`, a new portfolio is created. If `assign_to_registry`
#'   is true, the object is added to the registry.
#'   The function performs various assertions to ensure the
#'   validity of the input arguments.
#'
#' @examples
#' # Retrieve an existing portfolio
#' .portfolio("short_name")
#'
#' # Create a new portfolio
#' .portfolio(
#'  "short_name",
#'  "Long Name",
#'  nav = 1000,
#'  positions = list(),
#'  create = TRUE
#' )
#'
#' @seealso \code{\link{Portfolio}} for the Portfolio class.
#' @import checkmate
#'
#' @export
.portfolio <- function(
  short_name,
  long_name,
  nav = 0,
  positions = list(),
  create = FALSE,
  assign_to_registry = TRUE
) {
  checkmate::assert_character(short_name)

  env <- registries$portfolios
  if (exists(short_name, envir = env, inherits = FALSE)) {
    return(get(short_name, envir = env))
  }
  checkmate::assert_flag(create)
  checkmate::assert_flag(assign_to_registry)
  checkmate::assert_character(long_name)
  checkmate::assert_numeric(nav)

  if (!create) stop("Portfolio does not exist and create is set to FALSE")
  lapply(positions, \(p) checkmate::assert_r6(p, "Position"))
  portfolio <- Portfolio$new(
    long_name,
    short_name,
    nav,
    positions
  )
  if (assign_to_registry) assign(short_name, portfolio, envir = env)
  invisible(portfolio)
}

#' Create or Retrieve an SMA Object
#'
#' This function creates or retrieves an SMA (Separately Managed Account) object
#' If the SMA already exists in the registry, it is returned. Otherwise, a new
#' SMA object is created if the `create` parameter is set to `TRUE`.
#'
#' @param short_name A string representing the short name of the SMA.
#'  Must be unique.
#' @param long_name A string representing the long name of the SMA.
#' @param nav A numeric value representing the net asset value (NAV) of the SMA.
#'  Defaults to 0.
#' @param positions A list of `Position` objects representing the positions in
#'  the SMA. Defaults to an empty list.
#' @param base_portfolio Either a single character string naming the base
#'   portfolio, or a named numeric vector of blend weights that sum to 1
#'   (e.g. \code{c(ccmf = 0.8, atom_core = 0.2)}).
#' @param create A boolean indicating whether to create the SMA if it does
#'   not exist. Defaults to `FALSE`.
#' @param assign_to_registry A boolean indicating whether to assign the SMA
#'   object to the registry. Defaults to `TRUE`.
#'
#' @return An SMA object.
#' @details The function checks if the SMA with the given `short_name` exists
#' in the `registries$portfolios` environment. If it exists, the SMA is
#' retrieved and returned. If it does not exist and `create` is `TRUE`, a new
#' SMA object is created using the provided parameters and added to the
#' registry. If `create` is `FALSE` and the SMA does not exist, an error is
#' raised.
#'
#' @examples
#' # Retrieve an existing SMA
#' existing_sma <- .sma("short_name", "long_name",
#'                      base_portfolio = "base_portfolio")
#'
#' # Create a new SMA with a blended base portfolio (80/20)
#' new_sma <- .sma(
#'   "new_short_name", "New Long Name", nav = 1000000,
#'   base_portfolio = c(ccmf = 0.8, atom_core = 0.2), create = TRUE
#' )
#'
#' @seealso \code{\link{Portfolio}}, \code{\link{SMA}}
#' @import checkmate
#' @export
.sma <- function(
  short_name, long_name,
  nav = 0, positions = list(),
  base_portfolio, create = FALSE, assign_to_registry = TRUE
) {
  checkmate::assert_character(short_name)
  checkmate::assert_logical(create)
  env <- registries$portfolios
  if (exists(short_name, envir = env)) return(get(short_name, envir = env))
  if (!create) stop("SMA does not exist and create is set to FALSE")
  checkmate::assert_character(long_name)
  checkmate::assert_numeric(nav)
  lapply(
    positions,
    function(position) checkmate::assert_r6(position, "Position")
  )
  base_ptfl <- if (
    is.character(base_portfolio) && length(base_portfolio) == 1
  ) {
    .portfolio(base_portfolio, create = FALSE)
  } else {
    checkmate::assert_numeric(
      base_portfolio, min.len = 2, any.missing = FALSE
    )
    checkmate::assert_names(names(base_portfolio), type = "unique")
    if (abs(sum(base_portfolio) - 1) > 1e-6) {
      stop("base_portfolio weights must sum to 1.")
    }
    lapply(names(base_portfolio), function(nm) {
      list(
        portfolio = .portfolio(nm, create = FALSE),
        weight = base_portfolio[[nm]]
      )
    })
  }
  sma <- SMA$new(
    long_name,
    short_name,
    nav,
    positions,
    base_ptfl
  )
  if (assign_to_registry) {
    assign(short_name, sma, envir = env)
  }
  invisible(sma)
}

#' Create or Retrieve an SMA Rule
#'
#' This function creates or retrieves an SMA rule object associated with a
#'  specific SMA. The rule defines certain conditions or thresholds for the SMA
#'  and can be scoped to positions, portfolios, or count.
#'
#' @param sma_name A string representing the name of the SMA.
#'  Must be a valid SMA name.
#' @param rule_id An integer representing the unique identifier for the rule.
#'  Must be a positive integer.
#' @param rule_name A string representing the name of the rule.
#' @param scope One of "position", "portfolio", "count", "covered_options" or
#'  "aggregate_shares" indicating the scope of the rule.
#' @param bbfields A character vector of Bloomberg fields to be used in the
#'  rule's logic.
#' @param definition A function defining the rule's logic.
#'  Must be a valid function object. May be `NULL` if `scope` is "count".
#' @param max_threshold (Optional) A numeric value specifying the maximum
#'  threshold for the rule. If not provided, defaults to `Inf`.
#' @param min_threshold (Optional) A numeric value specifying the minimum
#'  threshold for the rule. If not provided, defaults to `-Inf`.
#' @param swap_only A logical value indicating whether the rule applies only to
#'  swaps. Defaults to `FALSE`.
#' @param side (Optional) A string indicating the side of the rule.
#'  Valid values are "long", "short", or "gross". Used for position count rules.
#' @param exclusions (Optional) A character vector of security IDs to be
#'  excluded from the rule.
#' @param restrict_calls,restrict_puts (Optional) Logical. For a
#'  \code{"covered_options"} rule, whether short calls / short puts are governed.
#'  Both default `TRUE`. Ignored for other scopes.
#' @param per_contract (Optional) Logical. For a \code{"covered_options"} rule,
#'  when `TRUE` coverage is measured on the short legs only (a long option does
#'  not offset a short one); when `FALSE` (default) coverage nets across legs.
#'  Ignored for other scopes.
#' @param field (Optional) Character. For an \code{"aggregate_shares"} rule,
#'  the field mnemonic holding each security's share reference (e.g.
#'  \code{"HS021"}); the firm-wide share count across all portfolios may not exceed
#'  `max_threshold` times this value. Defaults to the first entry of
#'  `bbfields`. Ignored for other scopes.
#' @param underlying (Optional) Logical. For an \code{"aggregate_shares"} rule,
#'  whether `field` is read from an option's underlying (default `TRUE`).
#'  Ignored for other scopes.
#'
#' @return An object of class `SMARule` representing the SMA rule.
#'
#' @details
#' The function first checks if the rule already exists in the `smarules`
#'  registry. If it exists, the existing rule is returned. Otherwise, a new
#'  rule is created and stored in the registry. The rule's scope determines
#'  whether it applies to individual positions, the entire portfolio, or count.
#'
#' @examples
#' # Example usage:
#' my_rule <- .sma_rule(
#'   sma_name = "example_sma",
#'   rule_name = "example_rule",
#'   scope = "position",
#'   definition = function(x) x > 0,
#'   max_threshold = 100,
#'   min_threshold = 10
#' )
#'
#' @seealso [SMARule()]
#'
#' @import checkmate
#' @export
.sma_rule <- function(
  sma_name,
  rule_id,
  rule_name,
  scope,
  definition = NULL,
  bbfields = NULL,
  max_threshold = Inf,
  min_threshold = -Inf,
  swap_only = FALSE,
  gross_exposure = FALSE,
  relative_to = "nav",
  divisor = NULL,
  exclusions = NULL,
  include = NULL,
  grandfather = FALSE,
  restrict_calls = TRUE,
  restrict_puts = TRUE,
  per_contract = FALSE,
  field = NULL,
  underlying = TRUE
) {
  checkmate::assert_character(sma_name, len = 1)
  sma <- .sma(sma_name, create = FALSE)
  checkmate::assert_r6(sma, "SMA")

  checkmate::assert_integer(rule_id, len = 1)
  key_name <- paste0(sma_name, "::", rule_id)
  env <- registries$smarules
  if (exists(key_name, envir = env)) return(get(key_name, envir = env))

  checkmate::assert_character(rule_name, len = 1)
  scope_types <- c(
    "position", "portfolio", "count", "covered_options", "aggregate_shares"
  )
  checkmate::assert_choice(scope, scope_types)

  # The covered-options rule is structural (option <-> underlying coupling), not
  # factor-based, so it carries no definition/threshold; count rules build their
  # own selection variables; the aggregate-shares rule reads a field directly.
  # Only the threshold rules require a definition.
  if (scope %in% c("position", "portfolio")) {
    checkmate::assert_function(definition)
  }

  checkmate::assert_numeric(max_threshold)
  checkmate::assert_numeric(min_threshold)

  checkmate::assert_choice(
    relative_to, c("nav", "gmv", "long_gmv", "short_gmv")
  )

  if (!is.null(divisor)) checkmate::assert_r6(divisor, "DivisorProvider")

  if (scope == "count") {
    checkmate::assert_choice(include, c("long_only", "short_only", "all"))
  }

  if (scope == "position") {
    smarule <- SMARulePosition$new(
      sma_name = sma_name,
      rule_id = rule_id,
      name = rule_name,
      scope = scope,
      bbfields = bbfields,
      definition = definition,
      max_threshold = max_threshold,
      min_threshold = min_threshold,
      swap_only = swap_only,
      gross_exposure = gross_exposure,
      relative_to = relative_to,
      divisor = divisor,
      exclusions = exclusions,
      include = include,
      grandfather = grandfather
    )
  }
  if (scope == "portfolio") {
    smarule <- SMARulePortfolio$new(
      sma_name = sma_name,
      rule_id = rule_id,
      name = rule_name,
      scope = scope,
      bbfields = bbfields,
      definition = definition,
      max_threshold = max_threshold,
      min_threshold = min_threshold,
      swap_only = swap_only,
      gross_exposure = gross_exposure,
      relative_to = relative_to,
      divisor = divisor,
      exclusions = exclusions,
      include = include,
      grandfather = grandfather
    )
  }
  if (scope == "count") {
    smarule <- SMARuleCount$new(
      sma_name = sma_name,
      rule_id = rule_id,
      name = rule_name,
      scope = scope,
      bbfields = bbfields,
      definition = definition,
      max_threshold = max_threshold,
      min_threshold = min_threshold,
      swap_only = swap_only,
      gross_exposure = gross_exposure,
      relative_to = relative_to,
      divisor = divisor,
      exclusions = exclusions,
      include = include,
      grandfather = grandfather
    )
  }
  if (scope == "covered_options") {
    smarule <- SMARuleCoveredOptions$new(
      sma_name = sma_name,
      rule_id = rule_id,
      name = rule_name,
      restrict_calls = restrict_calls,
      restrict_puts = restrict_puts,
      per_contract = per_contract,
      exclusions = exclusions
    )
  }
  if (scope == "aggregate_shares") {
    if (is.null(field)) field <- bbfields[1]
    if (is.null(field) || is.na(field) || !nzchar(field)) {
      stop("An 'aggregate_shares' rule needs a field (or a bbfields entry).")
    }
    smarule <- SMARuleAggregateShares$new(
      sma_name = sma_name,
      rule_id = rule_id,
      name = rule_name,
      field = field,
      max_threshold = max_threshold,
      underlying = underlying,
      exclusions = exclusions,
      grandfather = grandfather
    )
  }
  assign(key_name, smarule, envir = env)
  invisible(smarule)
}