#' @title SMA Rule for Positions
#'
#' @description R6 Class that encapsulates rule for SMAs
#'
#' @import R6
#' @include class-smarule.R
#'
#' @export
SMARulePosition <- R6::R6Class( #nolint
  "SMARulePosition",
  inherit = SMARule,
  public = list(
    #' @description Check the rule against raw portfolio data
    #' @param ids Character vector of security IDs
    #' @param qty Numeric vector of quantities
    #' @param nav Numeric NAV value
    #' @param prices Optional numeric vector of prices. If NULL, fetched.
    #' @param tolerance Numeric tolerance level for compliance check
    #' @param is_swap Optional logical vector indicating if each security is
    #'  on swap
    #' @return List of security IDs that do not comply with the rule
    check_compliance = function(
      ids, qty, nav, prices = NULL, is_swap = NULL, tolerance = 1e-6
    ) {
      if (isTRUE(self$get_swap_only())) {
        need_swap <- self$check_swap_security(ids)
        is_swap <- if (is.null(is_swap)) {
          vapply(ids, \(x) FALSE, logical(1))
        } else {
          is_swap
        }
        non_comply <- ids[which(need_swap & !is_swap)]
        return(
          if (!length(non_comply)) {
            list("pass" = TRUE)
          } else {
            list("pass" = FALSE, "non_comply" = non_comply)
          }
        )
      }

      d <- self$get_divisor()
      max_t <- self$get_max_threshold()
      min_t <- self$get_min_threshold()

      f <- self$apply_rule_definition(ids, nav)
      f[!is.finite(f)] <- 0

      # Apply include filter
      include_filter <- self$get_include()
      if (include_filter == "long_only") {
        f[qty <= 0] <- 0
      } else if (include_filter == "short_only") {
        f[qty >= 0] <- 0
      }

      exp_i <- qty * as.numeric(f)
      if (isTRUE(self$get_gross_exposure())) exp_i <- abs(exp_i)
      denom <- d$value_from_data(ids, qty, nav, prices)

      violates_max <- is.finite(max_t) & (exp_i > max_t * denom + tolerance)
      violates_min <- !isTRUE(self$get_gross_exposure()) &
        is.finite(min_t) &
        (exp_i < min_t * denom - tolerance)

      non_comply <- ids[which(violates_max | violates_min)]
      if (!length(non_comply)) {
        list("pass" = TRUE)
      } else {
        # Grandfathered rules tolerate an existing breach (the optimizer only
        # prevents it from growing), so surface it as a passive breach rather
        # than a hard failure - still reported, but distinguishable for the
        # compliance table.
        list(
          "pass" = FALSE,
          "passive" = self$get_grandfather(),
          "violates_max" = any(violates_max),
          "violates_min" = any(violates_min),
          "non_comply" = non_comply,
          "divisor_kind" = d$kind,
          "divisor_value" = denom
        )
      }
    },
    #' @description Get the swap flag for a given security
    #' @param security_id Security ID
    check_swap_security = function(security_id) {
      if (!isTRUE(self$get_swap_only())) {
        return(vapply(security_id, \(x) FALSE, logical(1)))
      }
      swap <- as.logical(self$apply_rule_definition(security_id))
      names(swap) <- security_id
      swap
    },
    #' @description Get limits for securities based on raw portfolio data
    #' @param security_id Vector of security IDs to calculate limits for
    #' @param ids_all Character vector of all security IDs (must include
    #'  security_id). For new securities not in portfolio, include with qty=0.
    #' @param qty_all Numeric vector of quantities corresponding to ids_all
    #' @param nav Numeric NAV value
    #' @param prices_all Optional numeric vector of prices corresponding to
    #'  ids_all. If NULL, fetched via .security().
    #' @param f_all Optional numeric vector of rule values for ids_all.
    #'  If NULL, computed via apply_rule_definition.
    #' @return List of Max and Min Value for each security
    get_security_limits = function(
      security_id, ids_all, qty_all, nav, prices_all = NULL, f_all = NULL
    ) {
      d <- self$get_divisor()

      if (is.null(f_all)) {
        f_all <- as.numeric(self$apply_rule_definition(ids_all, nav))
        f_all[!is.finite(f_all)] <- 0
      }

      prices_all <- if (!is.null(prices_all)) {
        prices_all
      } else {
        vapply(ids_all, \(id) {
          p <- .security(id)$get_replication_price()
          if (!is.finite(p) || p <= 0) 1 else p
        }, numeric(1))
      }

      w_all <- qty_all * prices_all / nav

      # Apply include filter
      include_filter <- self$get_include()
      f_all_filtered <- f_all
      if (include_filter == "long_only") {
        f_all_filtered[qty_all <= 0] <- 0
      } else if (include_filter == "short_only") {
        f_all_filtered[qty_all >= 0] <- 0
      }

      denom_current <- d$value_from_data(ids_all, qty_all, nav, prices_all)
      denom_contrib_all <- d$contrib_vec(w_all)

      max_t <- self$get_max_threshold()
      if (isTRUE(self$get_gross_exposure())) {
        min_t <- -max_t
      } else {
        min_t <- self$get_min_threshold()
      }

      out <- lapply(
        seq_along(security_id),
        function(i) {
          sec <- security_id[i]
          idx <- which(ids_all == sec)

          # Security must be in ids_all (caller's responsibility)
          if (!length(idx)) {
            stop("Security '", sec, "' not found in ids_all")
          }

          p_sec <- prices_all[idx]
          f_sec <- f_all[idx]

          # For include filters, return unconstrained limits for excluded direction
          if (include_filter == "long_only") {
            # Shorts unconstrained, longs limited
            if (f_sec == 0 || !is.finite(f_sec)) {
              return(list(max = Inf, min = -Inf))
            }
          } else if (include_filter == "short_only") {
            # Longs unconstrained, shorts limited
            if (f_sec == 0 || !is.finite(f_sec)) {
              return(list(max = Inf, min = -Inf))
            }
          } else {
            # "all" - standard behavior
            if (f_sec == 0 || !is.finite(f_sec)) {
              return(list(max = Inf, min = -Inf))
            }
          }

          gamma_vals <- d$gamma(p_sec, nav)
          gamma_pos <- gamma_vals$gamma_pos
          gamma_neg <- gamma_vals$gamma_neg

          if (d$kind == "nav") {
            denom_excl <- 1
          } else {
            denom_sec <- denom_contrib_all[idx]
            denom_excl <- denom_current - denom_sec
          }

          max_limit <- denom_excl / (1 / max_t * f_sec - gamma_pos)
          min_limit <- -(denom_excl / (1 / min_t * -f_sec - gamma_neg))

          # Apply directional constraints based on include filter
          if (include_filter == "long_only") {
            min_limit <- -Inf  # Shorts unconstrained
          } else if (include_filter == "short_only") {
            max_limit <- Inf  # Longs unconstrained
          }

          list(max = max_limit, min = min_limit)
        }
      )
      names(out) <- security_id
      out
    },
    #' @description Build the constraints for the optimization model
    #' @param ctx Context object with optimization variables and parameters
    #' @param nav Portfolio NAV
    build_constraints = function(ctx, nav) {
      d <- self$get_divisor()
      max_t <- self$get_max_threshold()
      min_t <- self$get_min_threshold()

      f <- self$apply_rule_definition(ctx$ids, nav)

      if (is.logical(f)) {
        fnum <- ifelse(f, ctx$price / ctx$nav, 0)
      } else {
        fnum <- as.numeric(f)
        fnum[!is.finite(fnum)] <- 0
      }

      gamma <- ifelse(
        ctx$price > 0 & is.finite(ctx$price),
        fnum / (ctx$price / ctx$nav),
        0
      )

      idx <- which(abs(gamma) > 1e-12)
      if (!length(idx)) return(list())

      # Directional filters select names by target-weight sign (a known
      # constant), keeping the exposure term affine in w. Splitting the
      # optimization variable with pos()/neg() instead would (a) break DCP -
      # a convex pos()/neg() under a >= floor, or under a <= cap with
      # negative gamma, is non-convex - and (b) diverge from
      # check_compliance(), which selects names by position sign and
      # constrains the signed exposure gamma * w (= f * qty).
      include_filter <- self$get_include()
      if (include_filter == "long_only") {
        idx <- idx[ctx$t_w[idx] > 0]
      } else if (include_filter == "short_only") {
        idx <- idx[ctx$t_w[idx] < 0]
      }
      if (!length(idx)) return(list())

      # Grandfathered rules bound each name relative to its current exposure
      # instead of absolutely: the effective threshold is stretched to
      # whichever is looser - the rule limit or the name's current exposure
      # ratio (current per-name exposure / current divisor value). An existing
      # breach can thus be held (or reduced) but never increased, and a
      # rebalance won't force it toward the limit. A fresh name (w_current 0)
      # keeps the original limit, so it still can't be opened in breach.
      w_cur <- ctx$w_current
      if (is.null(w_cur)) w_cur <- rep(0, length(ctx$ids))
      grandfather <- self$get_grandfather()
      divisor_cur <- if (d$kind == "nav") {
        1
      } else {
        dv <- sum(d$contrib_vec(w_cur))
        if (!is.finite(dv) || dv <= 0) 1 else dv
      }
      cur_ratio <- gamma[idx] * w_cur[idx] / divisor_cur
      cur_ratio[!is.finite(cur_ratio)] <- 0
      max_eff <- if (grandfather) {
        pmax(max_t, cur_ratio)
      } else {
        rep(max_t, length(idx))
      }
      min_eff <- if (grandfather) {
        pmin(min_t, cur_ratio)
      } else {
        rep(min_t, length(idx))
      }

      if (d$kind == "nav") {
        cons <- list()
        if (is.finite(max_t)) {
          cons <- c(cons, list(gamma[idx] * ctx$w[idx] <= max_eff))
        }
        if (is.finite(min_t)) {
          cons <- c(cons, list(gamma[idx] * ctx$w[idx] >= min_eff))
        }
        return(cons)
      }

      dres <- d$expr(ctx)
      cons <- dres$cons
      if (is.finite(max_t)) {
        cons <- c(cons, list(gamma[idx] * ctx$w[idx] <= max_eff * dres$expr))
      }
      if (is.finite(min_t)) {
        cons <- c(cons, list(gamma[idx] * ctx$w[idx] >= min_eff * dres$expr))
      }
      cons
    }
  )
)