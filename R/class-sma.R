#' @title SMA (R6 Object)
#'
#' @description R6 Class representing a seperately managed account.
#'  A seperately managed account is linked to a portfolio and contains
#'  Rules.
#'
#' @import R6
#' @include class-portfolio.R
#' @include class-position.R
#' @include class-security.R
#' @include utils.R
#' @include api-functions.R
#' @include class-tradeconstructor.R
#' @export
SMA <- R6::R6Class(   #nolint
  "SMA",
  inherit = Portfolio,
  private = list(
    base_portfolio_ = NULL  # list of list(portfolio = <Portfolio>, weight = <numeric>)
  ),
  public = list(
    #' @description
    #' Create a new SMA R6 object.
    #' @param long_name Character. SMA Long Name.
    #' @param short_name Character. SMA Short Name.
    #' @param nav Numeric. SMA Net Asset Value.
    #' @param positions Optional. SMA Positions. Default is NULL.
    #' @param base_portfolio A Portfolio object (weight = 1.0) or a list of
    #'   \code{list(portfolio = <Portfolio>, weight = <numeric>)} for blended bases.
    #' @return A new instance of the SMA class.
    initialize = function(
      long_name,
      short_name,
      nav,
      positions = NULL,
      base_portfolio = NULL
    ) {
      private$long_name_ <- long_name
      private$short_name_ <- short_name
      private$nav_ <- nav
      private$base_portfolio_ <- if (is.null(base_portfolio)) {
        NULL
      } else if (inherits(base_portfolio, "Portfolio")) {
        list(list(portfolio = base_portfolio, weight = 1.0))
      } else {
        base_portfolio  # already a list of (portfolio, weight) pairs
      }
      private$positions_ <- positions
      private$rules_ <- list()
      private$replacements_ <- list()
      private$trade_constructor <- SMAConstructor$new()
    },
    # Getters ------------------------------------------------------------------
    #' Get Base Portfolios
    #' @description Get all base portfolios with their blend weights as a list
    #'   of \code{list(portfolio, weight)} pairs.
    get_base_portfolios = function() private$base_portfolio_,
    #' Get Base Portfolio
    #' @description Get the primary (first) base portfolio. For single-base SMAs
    #'   this is the only base. For blended SMAs use \code{get_base_portfolios()}
    #'   to access all bases and weights.
    get_base_portfolio = function() {
      if (is.null(private$base_portfolio_)) return(NULL)
      private$base_portfolio_[[1]]$portfolio
    },
    #' Get Base Portfolio Position
    #' @description Get a position in the primary base portfolio.
    #' @param security_id Security ID
    get_base_portfolio_position = function(security_id = NULL) {
      self$get_base_portfolio()$get_position(security_id)
    },
    # Checkers -----------------------------------------------------------------
    #' Check Rule Compliance
    #' @description Check if the SMA is compliant with all its rules.
    #' @param verbose Logical. Whether to print compliance results. Defaults to
    #'  TRUE.
    #' @import checkmate
    #' @return A list of rule compliance results.
    check_rule_compliance = function(verbose = TRUE) {
      checkmate::assert_logical(verbose)
      rules <- self$get_rules()
      if (length(rules) == 0) return(list())
      positions <- self$get_position()

      ids <- vapply(positions, \(p) p$get_id(), character(1))
      qty <- vapply(positions, \(p) p$get_qty(), numeric(1))
      is_swap <- vapply(positions, \(p) p$get_swap(), logical(1))
      nav <- self$get_nav()

      results <- lapply(
        rules,
        \(r) r$check_compliance(ids = ids, qty = qty, nav = nav, is_swap = is_swap) #nolint
      )
      if (verbose) return(results)

      non_comply_results <- Filter(\(x) !x$pass, results)

      if (length(non_comply_results) == 0) {
        return(list("pass" = TRUE))
      } else {
        non_comply <- lapply(non_comply_results, \(x) x$non_comply)
        # Passive (grandfathered) breaches remain in non_compliant so existing
        # consumers still see them, but are also listed separately so the
        # compliance table can distinguish a tolerated drift from a hard
        # violation.
        passive_results <- Filter(\(x) isTRUE(x$passive), non_comply_results)
        passive_breach <- lapply(passive_results, \(x) x$non_comply)
        return(list(
          "pass" = FALSE,
          "non_compliant" = non_comply,
          "passive_breach" = passive_breach
        ))
      }
    },
    # Replicators --------------------------------------------------------------
    #' Replicate a trade from the base portfolio using Quantity
    #' @description
    #' Analyzes the effect of a trade in the base portfolio on the SMA,
    #' identifying the required trade.
    #' @param security_id The ID of the security traded in the base portfolio.
    #' @param base_trade_qty The quantity of the trade in the base portfolio.
    #' @param base_portfolio_name Character. Short name of the trading base
    #'   portfolio. Required for blended SMAs; defaults to the primary base
    #'   portfolio when NULL.
    replicate_trade_qty = function(
      security_id, base_trade_qty, base_portfolio_name = NULL
    ) {
      checkmate::assert_character(security_id, len = 1)
      checkmate::assert_numeric(base_trade_qty, len = 1)
      self$get_trade_constructor()$replicate_trade_qty(
        security_id = security_id,
        base_trade_qty = base_trade_qty,
        portfolio = self,
        base_portfolio_name = base_portfolio_name
      )
    }
  )
)