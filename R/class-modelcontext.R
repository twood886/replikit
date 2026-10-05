#' @title Model Context for CVXR Optimization
#' @description R6 Class representing the context for optimization models,
#'  including variables and parameters.
#' @import R6
#' @import CVXR
#' @export
ModelContext <- R6::R6Class( #nolint
  "ModelContext",
  public = list(
    n = NULL,
    ids = NULL,
    price = NULL,
    nav = NULL,
    t_w = NULL,
    w_current = NULL,
    sgn = NULL,
    w = NULL,
    alpha = NULL,
    params = NULL,
    index_of = NULL,
    var_factory = NULL,
    #' @description Initialize the ModelContext object.
    #' @param n Number of securities
    #' @param ids Character vector of security IDs
    #' @param price Numeric vector of security prices
    #' @param nav Numeric NAV value
    #' @param t_w Numeric target weights
    #' @param sgn Numeric sign vector
    #' @param w CVXR Variable for weights
    #' @param alpha CVXR Parameter for risk aversion
    #' @param params List of CVXR Parameters
    #' @param index_of Function mapping security IDs to indices
    #' @param var_factory Variable factory for creating CVXR variables
    #' @param w_current Numeric vector of current portfolio weights (aligned to
    #'  \code{ids}); used by grandfathered rules to bound trades relative to the
    #'  existing position. Defaults to zeros when not supplied.
    initialize = function(
      n, ids, price, nav, t_w, sgn, w, alpha, params, index_of, var_factory,
      w_current = NULL
    ) {
      self$n <- n
      self$ids <- ids
      self$price <- price
      self$nav <- nav
      self$t_w <- t_w
      self$w_current <- if (is.null(w_current)) rep(0, n) else w_current
      self$sgn <- sgn
      self$w <- w
      self$alpha <- alpha
      self$params <- params
      self$index_of <- index_of
      self$var_factory <- var_factory
    },
    #' @description Get a portfolio metric as a CVXR expression
    #' @param kind Character string: "gmv", "long_gmv", or "short_gmv"
    #' @return List with 'var' (CVXR expression) and 'cons' (list of constraints)
    metric = function(kind) {
      if (kind == "gmv") {
        # GMV = sum of absolute weights
        # Use auxiliary variables: pos >= w, pos >= 0, neg >= -w, neg >= 0
        # Then GMV = sum(pos + neg)
        gmv_var <- self$var_factory$scalar("gmv")
        pos <- self$var_factory$vec("pos_gmv", self$n, "pos_gmv")
        neg <- self$var_factory$vec("neg_gmv", self$n, "neg_gmv")
        cons <- list(
          pos >= self$w,
          pos >= 0,
          neg >= -self$w,
          neg >= 0,
          gmv_var == CVXR::sum_entries(pos + neg)
        )
        return(list(var = gmv_var, cons = cons))
      }

      if (kind == "long_gmv") {
        # Long GMV = sum of positive weights
        # Use auxiliary variable: pos >= w, pos >= 0
        # Then Long GMV = sum(pos)
        long_gmv_var <- self$var_factory$scalar("long_gmv")
        pos <- self$var_factory$vec("pos_long_gmv", self$n, "pos_long_gmv")
        cons <- list(
          pos >= self$w,
          pos >= 0,
          long_gmv_var == CVXR::sum_entries(pos)
        )
        return(list(var = long_gmv_var, cons = cons))
      }

      if (kind == "short_gmv") {
        # Short GMV = sum of absolute negative weights
        # Use auxiliary variable: neg >= -w, neg >= 0
        # Then Short GMV = sum(neg)
        short_gmv_var <- self$var_factory$scalar("short_gmv")
        neg <- self$var_factory$vec("neg_short_gmv", self$n, "neg_short_gmv")
        cons <- list(
          neg >= -self$w,
          neg >= 0,
          short_gmv_var == CVXR::sum_entries(neg)
        )
        return(list(var = short_gmv_var, cons = cons))
      }

      stop("Unknown metric kind: ", kind, ". Must be 'gmv', 'long_gmv', or 'short_gmv'")
    }
  )
)