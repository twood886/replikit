#' @title SMA Rule (R6 Onject)
#'
#' @description R6 Class that encapsultes rules for SMAs.
#'
#' @import R6
#' @include class-sma.R
#'
#' @export
SMARule <- R6::R6Class( #nolint
  "SMARule",
  private = list(
    sma_name_ = NULL,
    rule_id_ = NULL,
    name_ = NULL,
    scope_ = NULL,
    bbfields_ = NULL,
    definition_ = NULL,
    max_threshold_ = NULL,
    min_threshold_ = NULL,
    swap_only_ = NULL,
    gross_exposure_ = NULL,
    relative_to_ = NULL,
    exclusions_ = NULL,
    divisor_ = NULL,
    include_ = NULL,
    grandfather_ = NULL
  ),
  public = list(
    #' @param sma_name Character
    #' @param rule_id Integer
    #' @param name Character
    #' @param scope Character
    #' @param bbfields Character vector
    #' @param definition Formula
    #' @param max_threshold numeric
    #' @param min_threshold numeirc
    #' @param swap_only logical
    #' @param gross_exposure logical
    #' @param relative_to Character
    #' @param exclusions Character vector
    #' @param divisor DivisorProvider object
    #' @param include Character: "all", "long_only", or "short_only"
    #' @param grandfather Logical. When TRUE the rule bounds trades relative to
    #'  the current position instead of imposing an absolute limit: an existing
    #'  breach may be held (or reduced) but never increased, and a rebalance
    #'  will not force a grandfathered position to the limit. Defaults to FALSE.
    initialize = function(
      sma_name = NULL,
      rule_id = NULL,
      name = NULL,
      scope = NULL,
      bbfields = NULL,
      definition = NULL,
      max_threshold = NULL,
      min_threshold = NULL,
      swap_only = FALSE,
      gross_exposure = FALSE,
      relative_to = "nav",
      exclusions = NULL,
      divisor = NULL,
      include = "all",
      grandfather = FALSE
    ) {
      private$sma_name_ <- sma_name
      private$rule_id_ <- rule_id
      private$name_ <- name
      private$scope_ <- scope
      private$bbfields_ <- bbfields
      private$definition_ <- definition
      private$max_threshold_ <- max_threshold
      private$min_threshold_ <- min_threshold
      private$swap_only_ <- swap_only
      private$gross_exposure_ <- gross_exposure
      private$relative_to_ <- relative_to
      private$exclusions_ <- exclusions
      if (!include %in% c("all", "long_only", "short_only")) {
        stop("include must be 'all', 'long_only', or 'short_only'")
      }
      private$include_ <- include
      private$grandfather_ <- isTRUE(grandfather)
      if (checkmate::test_r6(divisor, "DivisorProvider")) {
        private$divisor_ <- divisor
      } else {
        if (relative_to %in% c("nav", "gmv", "long_gmv", "short_gmv")) {
          private$divisor_ <- DivisorProvider$new(relative_to)
        } else {
          private$divisor_ <- DivisorProvider$new("nav")
        }
      }
    },
    #' Get SMA Name
    #' @description Get the name of the SMA
    get_sma_name = function() private$sma_name_,
    #' Get Rule Id
    #' @description Get the rule id of the SMA Rule
    get_rule_id = function() private$rule_id_,
    #' Get Id
    #' @description Get Id of SMA Rule
    get_id = function() paste0(private$sma_name_, "::", private$rule_id_),
    #' Get Name
    #' @description Get name of SMA Rule
    get_name = function() private$name_,
    #' Get Scope
    #' @description Get the scope of the SMA Rule
    get_scope = function() private$scope_,
    #' Get Bloomberg Fields
    #' @description Get the Bloomberg fields of the SMA Rule
    get_bbfields = function() private$bbfields_,
    #' Get Definition
    #' @description Get the definition of the SMA Rule
    get_definition = function() private$definition_,
    #' Get Max Threshold
    #' @description Get the threshold of the SMA Rule
    get_max_threshold = function() private$max_threshold_,
    #' Get Min Threshold
    #' @description Get the threshold of the SMA Rule
    get_min_threshold = function() private$min_threshold_,
    #' Get Swap Only Flag
    #' @description Get the swap only flag of the SMA Rule
    get_swap_only = function() private$swap_only_,
    #' Get Gross Exposure Flag
    #' @description Get the gross exposure flag of the SMA Rule
    get_gross_exposure = function() private$gross_exposure_,
    #' Get Relative To
    #' @description Get the relative to field of the SMA Rule
    #' @return Character
    get_relative_to = function() private$relative_to_,
    #' Get Exclusions
    #' @description Get the exclusions from the SMA Rule
    get_exclusions = function() private$exclusions_,
    #' Get Divisor
    #' @description Get the DivisorProvider object
    #' @return DivisorProvider object
    get_divisor = function() private$divisor_,
    #' Get Include
    #' @description Get the include filter ("all", "long_only", "short_only")
    #' @return Character
    get_include = function() private$include_,
    #' Get Grandfather Flag
    #' @description Whether the rule bounds trades relative to the current
    #'  position (TRUE) rather than imposing an absolute limit (FALSE).
    #' @return Logical
    get_grandfather = function() isTRUE(private$grandfather_),
    #' Apply the Rule Definition
    #' @description Apply the rule definition to a set of security IDs
    #' @param security_id Security ID
    #' @param nav Portfolio NAV
    apply_rule_definition = function(security_id, nav) {
      exp <- private$definition_(security_id, nav)
      names(exp) <- security_id
      excl <- self$get_exclusions()
      exp[excl[excl %in% names(exp)]] <- 0
      exp
    },
    #' Is Security Impacted
    #' @description Check if a security is impacted by the rule
    #' @param security_id Security ID
    #' @param nav Portfolio NAV
    #' @return Logical
    security_impacted = function(security_id, nav) {
      exp <- self$apply_rule_definition(security_id, nav)
      any(exp != 0)
    },
    #' Check Compliance Capacity
    #' @param security_id Security ID
    #' @param nav Portfolio NAV
    #' @return Numeric
    capacity = function(security_id, nav) {
      return(NULL)
    },
    #' Build Constraints
    #' @description Build any additional constraints for the optimization
    #' @param ctx ModelContext
    build_constraints = function(ctx) list(),
    #' Objective Terms
    #' @description Build any additional objective terms for the optimization
    #' @param ctx ModelContext
    objective_terms = function(ctx) list()
  )
)