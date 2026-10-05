#' @title Portfolio (R6 Object)
#' @description
#' R6 Class representing a portfolio object.
#' @import R6
#' @importFrom dplyr filter
#' @include api-functions.R
#' @include utils.R
#' @include class-tradeconstructor.R
#' @include class-orderconstructor.R
#' @export
Portfolio <- R6::R6Class( #nolint
  "Portfolio",
  private = list(
    long_name_ = NULL,
    short_name_ = NULL,
    nav_ = NULL,
    positions_ = NULL,
    rules_ = list(),
    replacements_ = list(),
    order_constructor_ = NULL,
    trade_constructor = NULL
  ),
  public = list(
    #' @description
    #' Create New Portfolio R6 object
    #' @param long_name Portfolio Long Name
    #' @param short_name Portfolio Short Name
    #' @param nav NAV of portfolio
    #' @param positions list of position items
    initialize = function(
      long_name, short_name, nav, positions
    ) {
      private$long_name_ <- long_name
      private$short_name_ <- short_name
      private$nav_ <- nav
      private$positions_ <- positions
      private$rules_ <- list()
      private$replacements_ <- list()
      private$trade_constructor <- TradeConstructor$new()
    },
    # Getter Functions ---------------------------------------------------------
    #' @description Get Portfolio short name
    get_short_name = function() private$short_name_,
    #' @description Get Fund NAV
    get_nav = function() private$nav_,
    #' @description Get list of positions in portfolio
    #' @param id Ticker
    get_position = function(id = NULL) {
      positions <- private$positions_
      if (is.null(id)) return(positions)
      position_ids <- sapply(positions, \(x) x$get_id())
      if (!id %in% position_ids) stop("No position in portfolio with id")
      positions[[which(position_ids == id)]]
    },
    #' @description Get the Rules
    #' @return A list of rules
    get_rules = function() private$rules_,
    #' @description Get the Trade Constructor
    #' @return The portfolio constructor object
    get_trade_constructor = function() private$trade_constructor,
    #' @description Get replacement security (or securities) for a given
    #'  replaced security.
    #' @param replaced_security_id Security ID of the replaced security (in base ptfl) #nolint
    #' @return If \code{replaced_security_id} is NULL, the full named list of
    #'  replacements, each a \code{list(security, weight)} pair. Otherwise a
    #'  single \code{list(security, weight)} pair for that security, where
    #'  \code{security} is a character vector of one or more replacement IDs
    #'  and \code{weight} the matching split (summing to 1). Securities with
    #'  no replacement return
    #'  \code{list(security = replaced_security_id, weight = 1)}.
    get_replacement_security = function(replaced_security_id = NULL) {
      if (is.null(replaced_security_id)) return(private$replacements_)
      if (!replaced_security_id %in% names(private$replacements_)) {
        return(list(security = replaced_security_id, weight = 1))
      }
      private$replacements_[[replaced_security_id]]
    },
    #' @description Get the replaced (original) security for a given
    #'  replacement security.
    #' @param replacement_security_id Security ID of the replacement security (in SMA) #nolint
    get_replaced_security = function(replacement_security_id = NULL) {
      if (is.null(replacement_security_id)) return(names(private$replacements_))
      matches <- vapply(
        private$replacements_,
        \(r) replacement_security_id %in% r$security,
        logical(1)
      )
      if (!any(matches)) return(NULL)
      names(private$replacements_)[matches]
    },
    #' @description Get Max and Min Value of the security given all SMA Rules
    #' @param security_id Security ID
    #' @param position_only Logical. Return only position-based limits (default: FALSE) #nolint
    #' @param verbose Logical. Get verbose output (default: FALSE)
    get_security_position_limits = function(
      security_id = NULL,
      position_only = FALSE,
      verbose = FALSE
    ) {
      checkmate::assert_flag(verbose)
      checkmate::assert_flag(position_only)
      self$get_trade_constructor()$get_security_position_limits(
        portfolio = self,
        security_id = security_id,
        position_only = position_only,
        verbose = verbose
      )
    },
    #' @description Get Swap Flag for a given security
    #' @param security_id Security ID
    get_swap_flag_position_rules = function(
      security_id = NULL
    ) {
      private$trade_constructor$get_swap_flag_position_rules(self, security_id)
    },
    #'@description Get OrderConstructor
    get_order_constructor = function() private$order_constructor_,
    # Setter Functions ---------------------------------------------------------
    #' Add OrderConstructor
    #' @param pb_act_num list of PB and ISDA account numbers
    #' @param pb_act_sel formula for determining which PB account to use
    #' @param isda_act_sel formula for determining which ISDA account to use
    add_orderconstructor = function(pb_act_num, pb_act_sel, isda_act_sel) {
      checkmate::assert_list(pb_act_num)
      checkmate::assert_function(pb_act_sel)
      checkmate::assert_function(isda_act_sel)
      oc <- OrderConstructor$new(pb_act_num, pb_act_sel, isda_act_sel)
      private$order_constructor_ <- oc
      return(invisible(private$order_constructor_))
    },
    #' @description
    #' Add flow to portfolio
    #' @param flow flow amount
    add_flow = function(flow = 0) {
      private$nav_ <- private$nav_ + flow
      invisible(NULL)
    },
    #' @description
    #' Add Position to Portfolio
    #' @param position Position S6 Object
    #' @param overwrite Logical. Overwrite existing position if TRUE
    add_position = function(position, overwrite = FALSE) {
      checkmate::assert_r6(position, "Position")
      position_ids <- sapply(self$get_position(), \(x) x$get_id())
      existing_pos <- position$get_id() %in% position_ids
      if (isTRUE(existing_pos)) {
        if (overwrite) {
          idx <- which(position_ids == position$get_id())
          private$positions_[[idx]] <- position
        }
      } else {
        private$positions_ <- c(private$positions_, position)
      }
      invisible(TRUE)
    },
    #' @description Add Holding Object to Portfolio
    #' @param holding Holding S6 Object
    add_holding = function(holding) {
      checkmate::assert_r6(holding, "Holding")
      sec_id <- holding$get_security_id()
      position <- .position(self$get_short_name(), sec_id, TRUE, TRUE)
      position$add_holding(holding)
      invisible(TRUE)
    },
    #' Add Rule
    #' @description Create Rule and Add to Portfolio
    #' @param rule An object of class SMARule
    add_rule = function(rule) {
      checkmate::assert_r6(rule, "SMARule")
      private$rules_[[rule$get_name()]] <- rule
      invisible(rule)
    },
    #' Add Replacement
    #' @description Add one or more replacement securities for an original
    #'  security. When more than one replacement security is given,
    #'  \code{weight} controls how the original security's overflow (the
    #'  amount that can't be held directly, e.g. due to a rule limit) is
    #'  split across the replacements. Calling this again for the same
    #'  \code{original_security} replaces its prior replacement set.
    #' @param original_security The original Security id
    #' @param replacement_security Character vector of one or more
    #'  replacement Security ids
    #' @param weight Numeric vector the same length as
    #'  \code{replacement_security} giving each replacement's share of the
    #'  overflow; must sum to 1. Defaults to an equal split across
    #'  \code{replacement_security}.
    add_replacement = function(
      original_security = NULL, replacement_security = NULL, weight = NULL
    ) {
      if (is.null(original_security) | is.null(replacement_security)) {
        stop("Securities must be provided")
      }
      original_security <- tolower(original_security)
      replacement_security <- tolower(replacement_security)
      for (sec in replacement_security) .security(sec)

      if (is.null(weight)) {
        weight <- rep(1 / length(replacement_security), length(replacement_security)) #nolint
      }
      checkmate::assert_numeric(weight, len = length(replacement_security))
      if (abs(sum(weight) - 1) > 1e-8) {
        stop("Replacement weights must sum to 1")
      }

      private$replacements_[[original_security]] <- list(
        security = replacement_security,
        weight = weight
      )
      invisible(NULL)
    },
    # Updaters -----------------------------------------------------------------
    #' @description Set NAV
    #' @param nav Numeric NAV value
    set_nav = function(nav) {
      private$nav_ <- as.numeric(nav)
      invisible(self)
    },
    #' @description Clear all positions
    clear_positions = function() {
      private$positions_ <- list()
      invisible(self)
    },
    # Calculators --------------------------------------------------------------
    #' Rebalance Portfolio
    #' @description Calculate the trade quantity for a given security
    #' @param as.df Logical. Return as data frame (default: TRUE)
    rebalance = function(as.df = TRUE) {
      checkmate::assert_flag(as.df)
      rebal <- self$get_trade_constructor()$optimize_sma(self)
      current_ids <- vapply(self$get_position(), \(p) p$get_id(), character(1))
      current_sh <- vapply(self$get_position(), \(p) p$get_qty(), numeric(1))
      current_wt <- vapply(self$get_position(), \(p) p$get_mkt_pct_nav(), numeric(1))
      names(current_sh) <- current_ids
      sec_ids <- unique(c(names(rebal$target_weights), current_ids))
      swap_rules <- self$get_swap_flag_position_rules(sec_ids)

      if (!as.df) return(rebal)
      data.frame(
        security_id   = sec_ids,
        target_weights = tidyr::replace_na(rebal$target_weights[sec_ids], 0),
        final_weights = tidyr::replace_na(rebal$weights[sec_ids], 0),
        final_shares  = tidyr::replace_na(rebal$shares[sec_ids], 0),
        current_shares = tidyr::replace_na(current_sh[sec_ids], 0),
        trade = tidyr::replace_na(rebal$shares[sec_ids], 0) -
          tidyr::replace_na(current_sh[sec_ids], 0),
        swap = as.logical(swap_rules),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
  )
)