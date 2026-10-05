#' @title Trade Constructor Parent Class
#' @import CVXR
#' @import R6
#' @include class-portfolio.R
#' @include class-smarule.R
#' @include class-smaruleposition.R
#' @include class-sma.R
#' @include class-security.R
#' @export
TradeConstructor <- R6::R6Class( #nolint
  "TradeConstructor",
  public = list(
    #' @description Calculate target quantities for the trade constructor.
    #'  For grandfathered rules the returned interval is widened to include the
    #'  security's current share count, so an existing breach may be held or
    #'  reduced but never increased (and never force-traded to the limit).
    #' @param portfolio An object of class Portfolio
    #' @param security_id A character vector of security IDs
    #' @param position_only Logical, if TRUE only consider position rules
    #' @param verbose Logical. Get verbose output (default: FALSE)
    get_security_position_limits = function(
      portfolio,
      security_id = NULL,
      position_only = FALSE,
      verbose = FALSE
    ) {
      if (is.null(security_id)) stop("Security ID must be supplied")
      rules <- Filter(\(r) !r$get_swap_only(), portfolio$get_rules())

      if (position_only) {
        rules <- Filter(\(r) r$get_scope() == "position", rules)
      }

      # Registry keys are lowercase (.security lowercases on registration),
      # so compare in lowercase or already-registered ids passed in mixed
      # case get needlessly re-fetched from the provider.
      new_securities <- setdiff(
        tolower(security_id), ls(get_registries()$securities)
      )
      if (length(new_securities) > 0) {
        lapply(new_securities, .security)
        # Assuming update_bloomberg_fields can take a vector of securities
        update_bloomberg_fields(new_securities)
      }

      nav <- portfolio$get_nav()

      ids_pos <- vapply(portfolio$get_position(), \(p) p$get_id(), character(1))
      qty_pos <- vapply(portfolio$get_position(), \(p) p$get_qty(), numeric(1))

      new_ids <- setdiff(security_id, ids_pos)
      ids_all <- c(ids_pos, new_ids)
      qty_all <- c(qty_pos, rep(0, length(new_ids)))
      prices_all <- vapply(
        ids_all, \(id) .security(id)$get_replication_price(), numeric(1)
      )
      prices_all[!is.finite(prices_all) | prices_all <= 0] <- 1

      limits_all <- list()
      for (sec in security_id) {
        cur_q <- qty_all[match(sec, ids_all)]
        for (r in rules) {
          limit <- r$get_security_limits(sec, ids_all, qty_all, nav, prices_all)
          lmax <- limit[[sec]]$max
          lmin <- limit[[sec]]$min
          # Grandfathered rules never force a trade: widen the limit interval
          # to include the current share count, so an existing breach can be
          # held (or reduced) but not increased. Mirrors the ratio stretch the
          # same rules apply in build_constraints; in the single-name share
          # domain (rest of the book fixed) "current point stays feasible" is
          # the exact equivalent. A fresh name (cur_q 0) keeps the absolute
          # limits, so it still can't be opened in breach.
          if (isTRUE(r$get_grandfather()) && is.finite(cur_q)) {
            lmax <- max(lmax, cur_q)
            lmin <- min(lmin, cur_q)
          }
          limits_all[[sec]][[r$get_name()]] <- list(max = lmax, min = lmin)
        }
      }

      if (isTRUE(verbose)) return(limits_all)

      limits <- lapply(
        security_id,
        \(sec) {
          sec_limit <- limits_all[[sec]]
          if (length(sec_limit) == 0) return(list(max = Inf, min = -Inf))
          max_limit <- min(sapply(sec_limit, \(x) x$max), na.rm = TRUE)
          min_limit <- max(sapply(sec_limit, \(x) x$min), na.rm = TRUE)
          list(max = max_limit, min = min_limit)
        }
      )
      names(limits) <- security_id
      limits
    },
    #' @description Identify securities that are part of any swap rule
    #' @param portfolio An object of class Portfolio
    #' @param security_id A character vector of security IDs
    get_swap_flag_position_rules = function(portfolio, security_id = NULL) {
      if (is.null(security_id)) stop("Security ID must be supplied")
      rules <- portfolio$get_rules()
      swaps <- lapply(rules, \(rule) rule$check_swap_security(security_id))
      swap_flag <- lapply(
        security_id,
        \(sec) any(vapply(swaps, \(swap) swap[[sec]], logical(1)))
      )
      names(swap_flag) <- security_id
      swap_flag
    },
    #' @description Build the Optimzation Model Context (called by optimize_sma)
    #' @param ids Character vector of security IDs
    #' @param price_vec Numeric vector of security prices
    #' @param nav Numeric, portfolio NAV
    #' @param t_w Numeric vector of target weights (+/-/0)
    #' @param params List of parameters (lambda_alpha, tau_rel, etc.)
    #' @param w_current Numeric vector of current portfolio weights (aligned to
    #'  \code{ids}); used by grandfathered rules. Defaults to zeros.
    make_model_context = function(
      ids, price_vec, nav, t_w, params, w_current = NULL
    ) {
      n  <- length(ids)
      vf <- VariableFactory$new()
      w  <- CVXR::Variable(n, name = "w")
      alpha <- CVXR::Variable(1, name = "alpha")
      index_of <- function(id) match(id, ids)
      ModelContext$new(
        n = n,
        ids = ids,
        price = price_vec,
        nav = nav,
        t_w = t_w,
        sgn = sign(t_w),
        w = w,
        alpha = alpha,
        params = params,
        index_of = index_of,
        var_factory = vf,
        w_current = w_current
      )
    },
    #' Main optimization using CVXR
    #' @param portfolio An object of class Portfolio
    #' @param lambda_alpha Regularization parameter for alpha
    #' @param tau_rel Small denominator to stabilize relative error
    #' @param beta_free Extra weight for "unaffected" names
    #' @param alpha_min Minimum value for alpha
    #' @param alpha_max Maximum value for alpha
    #' @param verbose Print progress
    #' @importFrom stats setNames
    optimize_sma = function(
      portfolio,
      lambda_alpha   = 10,
      tau_rel        = 1e-3,
      beta_free      = 5.0,
      alpha_min      = 1,
      alpha_max      = 1
    ) {
      tgt_qty <- self$calc_target_quantities(portfolio)
      current_pos <- private$.extract_qty(portfolio$get_position())
      replacements <- portfolio$get_replacement_security()
      s_ids <- names(replacements)
      t_ids <- if (length(replacements)) {
        unique(unlist(lapply(replacements, `[[`, "security"), use.names = FALSE)) #nolint
      } else {
        character(0)
      }

      sec_ids <- unique(c(names(tgt_qty), names(current_pos), t_ids))
      n <- length(sec_ids)

      price_vec <- vapply(
        sec_ids, \(s) .security(s)$get_replication_price(), numeric(1)
      )
      price_vec[!is.finite(price_vec) | price_vec <= 0] <- 1
      nav <- portfolio$get_nav()

      tgt_qty <- vapply(sec_ids, \(s) tgt_qty[s] %||% 0, numeric(1))
      t_w <- (price_vec * tgt_qty) / nav
      sgn <- sign(t_w)

      # Current SMA weights (aligned to sec_ids), for grandfathered rules that
      # bound trades relative to the existing position rather than absolutely.
      cur_qty <- vapply(sec_ids, \(s) current_pos[s] %||% 0, numeric(1))
      w_current <- (price_vec * cur_qty) / nav

      # Context ----------------------------------------------------------------
      params <- list(
        lambda_alpha = lambda_alpha,
        tau_rel = tau_rel,
        beta_free = beta_free,
        alpha_min = alpha_min,
        alpha_max = alpha_max
      )
      ctx <- self$make_model_context(
        sec_ids, price_vec, nav, t_w, params, w_current = w_current
      )
      w <- ctx$w
      alpha <- ctx$alpha

      # --- Base constraints (global box on alpha) -----------------------------
      t_w_nz <- t_w[abs(t_w) > 1e-12]
      cons <- list(alpha >= alpha_min, alpha <= alpha_max)
      # Zero-target names that are not overflow targets -> clamp to 0
      zero_target_ids <- setdiff(sec_ids, c(names(t_w_nz), t_ids))
      zero_not_targets <- match(zero_target_ids, sec_ids)
      if (length(zero_not_targets)) {
        cons <- c(cons, list(w[zero_not_targets] == 0))
      }

      # --- Let rules contribute constraints -----------------------------------
      rules <- portfolio$get_rules()
      if (length(replacements) > 0) {
        rules <- c(rules, list(OverflowRule$new(replacements)))
      }
      # Position-count rules add boolean selection variables. Combined with
      # this problem's quadratic tracking-error objective below, solving them
      # jointly is a mixed-integer QP that none of our available solvers
      # (ECOS_BB, HIGHS) support - only commercial solvers (GUROBI/XPRESS/
      # CPLEX) do. So their constraints are built and solved separately, in
      # a phase-1 MILP selection step below.
      count_rules <- Filter(\(r) r$get_scope() == "count", rules)
      other_rules <- Filter(\(r) r$get_scope() != "count", rules)

      other_cons <- unlist(
        lapply(other_rules, \(r) r$build_constraints(ctx, nav)),
        recursive = FALSE
      )
      cons <- c(cons, other_cons)

      # --- Objective ----------------------------------------------------------
      denom <- pmax(abs(t_w), tau_rel)
      base_err <- (w - alpha * t_w) / denom

      affected <- (sec_ids %in% s_ids) | (sec_ids %in% t_ids)
      free_idx <- which(!affected)

      term_base <- CVXR::sum_squares(base_err)
      term_free <- if (length(free_idx)) {
        beta_free * CVXR::sum_squares(base_err[free_idx])
      } else {
        0
      }
      net_tgt <- sum(t_w)
      rule_terms <- unlist(
        lapply(rules, \(r) r$objective_terms(ctx)),
        recursive = FALSE
      )
      rule_obj_sum <- if (length(rule_terms)) Reduce(`+`, rule_terms) else 0

      objective <- CVXR::Minimize(
        term_base
        + term_free
        + rule_obj_sum
        + lambda_alpha * CVXR::square(alpha - 1)
        #+ 10 * CVXR::square(CVXR::sum_entries(w) - net_tgt)
      )

      # --- Phase 1: resolve position-count selection (MILP) --------------------
      if (length(count_rules) > 0) {
        count_cons <- unlist(
          lapply(count_rules, \(r) r$build_constraints(ctx, nav)),
          recursive = FALSE
        )
        l1_objective <- CVXR::Minimize(CVXR::sum_entries(abs(base_err)))
        prob1 <- CVXR::Problem(l1_objective, c(cons, count_cons))
        CVXR::psolve(prob1, solver = "HIGHS")
        status1 <- CVXR::status(prob1)
        if (!(status1 %in% c("optimal", "optimal_inaccurate", "solved"))) {
          stop(sprintf(
            "Position-count selection (phase 1) failed with status: %s",
            status1
          ))
        }

        # Translate the solved selection into fixed continuous bounds on w,
        # instead of re-imposing the boolean constraints themselves - phase 2
        # below must stay a pure continuous QP (no integer variables) to be
        # solvable by OSQP/CLARABEL.
        fix_cons <- list()
        for (r in count_rules) {
          sel <- r$get_last_selection()
          idx <- match(sel$ids, sec_ids)
          if (!is.null(sel$z_long)) {
            z_val <- round(as.numeric(CVXR::value(sel$z_long)))
            excl <- idx[z_val < 0.5]
            if (length(excl)) fix_cons <- c(fix_cons, list(w[excl] <= 0))
          }
          if (!is.null(sel$z_short)) {
            z_val <- round(as.numeric(CVXR::value(sel$z_short)))
            excl <- idx[z_val < 0.5]
            if (length(excl)) fix_cons <- c(fix_cons, list(w[excl] >= 0))
          }
        }
        cons <- c(cons, fix_cons)
      }

      # --- Phase 2 (or only phase, when there's no count rule): continuous QP -
      prob <- CVXR::Problem(objective, cons)
      # --- Solve --------------------------------------------------------------
      opt_value <- {
        osqp_value <- tryCatch(
          CVXR::psolve(
            prob,
            solver = "OSQP",
            eps_abs = 1e-8,
            eps_rel = 1e-8,
            max_iter = 100000000,
            polish = TRUE
          ),
          error = function(e) NULL
        )
        # OSQP doesn't raise an R error when it merely fails to converge
        # (e.g. status "user_limit") - it just leaves prob's status set to
        # that outcome - so the fallback below must be status-driven, not
        # tryCatch-driven.
        # ECOS only supports LP/SOC cones; portfolios with enough rules can
        # produce a problem that also needs PowCone3D (e.g. rule-heavy GMV
        # divisor constraints combined with the scalar alpha penalty), which
        # ECOS rejects outright. CLARABEL supports SOC and power cones.
        if (is.null(osqp_value) ||
              !(CVXR::status(prob) %in% c("optimal", "optimal_inaccurate", "solved"))) { #nolint
          CVXR::psolve(
            prob,
            solver = "CLARABEL",
            tol_feas = 1e-8,
            tol_gap_abs = 1e-8,
            tol_gap_rel = 1e-8
          ) #nolint
        } else {
          osqp_value
        }
      }
      prob_status <- CVXR::status(prob)
      if (!(prob_status %in% c("optimal", "optimal_inaccurate", "solved"))) {
        stop(sprintf("Optimization failed with status: %s", prob_status))
      }

      w_hat     <- setNames(as.numeric(CVXR::value(w)), sec_ids)
      alpha_hat <- as.numeric(CVXR::value(alpha))
      sh        <- setNames((w_hat * nav) / price_vec,  sec_ids)
      sh_final  <- sh
      sh_final[sh > 0] <- floor(sh[sh > 0])
      sh_final[sh < 0] <- ceiling(sh[sh < 0])

      sf <- setNames(rep(NA_real_, n), sec_ids)
      nz <- which(abs(t_w) > 1e-12)
      sf[nz] <- w_hat[nz] / t_w[nz]

      list(
        shares          = sh_final,
        weights         = w_hat,
        target_shares   = tgt_qty,
        target_weights  = t_w,
        alpha_hat       = alpha_hat,
        scaling_factors = sf,
        objective_value = opt_value,
        status          = prob_status
      )
    }


  ),
  private = list(
    .extract_qty = function(positions) {
      if (length(positions) == 0) return(numeric(0))
      ids <- vapply(positions, function(x) x$get_id(), character(1))
      qty <- vapply(positions, function(x) x$get_qty(), numeric(1))
      # Clean up non-finite values
      if (any(!is.finite(qty))) {
        qty[!is.finite(qty)] <- 0
      }
      setNames(qty, ids)
    }
  )
)




#' @title SMA Trade Constructor Class
#' @import dplyr
#' @import purrr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na
#' @importFrom R6 R6Class
#' @include class-security.R
#' @include class-portfolio.R
#' @include class-smarule.R
#' @include class-smaruleposition.R
#' @export
SMAConstructor <- R6::R6Class( #nolint
  "SMAConstructor",
  inherit = TradeConstructor,
  public = list(
    #' @description Get the scale quantity for a security in the SMA
    #' @param base_portfolio Base portfolio object
    #' @param sma_portfolio SMA portfolio object
    #' @param base_security_id Security ID in the base portfolio
    get_scale_qty = function(base_portfolio, sma_portfolio, base_security_id) {
      base_pos_qty <- tryCatch(
        {base_portfolio$get_position(base_security_id)$get_qty()},
        error = function(e) 0
      )
      nav_ratio <- sma_portfolio$get_nav() / base_portfolio$get_nav()
      base_pos_qty * nav_ratio
    },
    #' @description Get the scale ratio for the SMA
    #' @param base_portfolio Base portfolio object
    #' @param sma_portfolio SMA portfolio object
    get_scale_ratio = function(base_portfolio, sma_portfolio) {
      base_nav <- base_portfolio$get_nav()
      sma_nav  <- sma_portfolio$get_nav()
      if (base_nav == 0) return(0)
      sma_nav / base_nav
    },
    #' Calculate target quantities for the SMA based on scaled base portfolio(s)
    #' @param sma SMA object
    calc_target_quantities = function(sma) {
      sma_nav <- sma$get_nav()
      base_list <- sma$get_base_portfolios()
      target_quantities <- numeric(0)

      for (item in base_list) {
        base_nav <- item$portfolio$get_nav()
        if (base_nav == 0) next
        scale_ratio <- sma_nav / base_nav
        contrib <- item$weight * 
          private$.extract_qty(item$portfolio$get_position()) * 
          scale_ratio
        new_ids <- setdiff(names(contrib), names(target_quantities))
        if (length(new_ids) > 0) target_quantities[new_ids] <- 0
        target_quantities[names(contrib)] <- target_quantities[names(contrib)] + contrib
      }

      target_quantities[!is.finite(target_quantities)] <- 0
      target_quantities
    },
    #' @description Replicate a trade from the base portfolio to the SMA
    #' based on trade quantity
    #' @param security_id Security ID of the traded security in the base
    #'  portfolio
    #' @param base_trade_qty Trade quantity in the base portfolio
    #' @param portfolio SMA portfolio object
    #' @param base_portfolio_name Short name of the trading base portfolio.
    #'  Required for SMAs with blended bases; defaults to the primary base
    #'  portfolio when NULL.
    #' @return A list with trade details and calculations. In addition to the
    #'  fields from \code{.unconst_to_const_shares} it includes the trade
    #'  decomposition (both on an \emph{unconstrained} basis, since the rule
    #'  clamp applies to the total and cannot be cleanly attributed to either
    #'  part): \code{marginal_shares}, the scaled replication of just this base
    #'  trade (\code{weight * base_trade_qty * sma_nav / base_nav}); and
    #'  \code{drift_shares}, the correction of the SMA's pre-existing under/over-
    #'  allocation. \code{marginal_shares + drift_shares} is the unconstrained
    #'  total trade; \code{trade_shares} is what the rules actually allow.
    replicate_trade_qty = function(
      security_id,
      base_trade_qty,
      portfolio,
      base_portfolio_name = NULL
    ) {
      checkmate::assert_character(security_id, len = 1)
      checkmate::assert_numeric(base_trade_qty, len = 1)
      checkmate::assert_r6(portfolio, "SMA")

      base_list <- portfolio$get_base_portfolios()
      trading_idx <- private$.get_base_idx(base_list, base_portfolio_name)

      sma_nav <- portfolio$get_nav()
      unconstrained_target_qty <- 0
      marginal_qty <- 0
      for (i in seq_along(base_list)) {
        item <- base_list[[i]]
        base_nav <- item$portfolio$get_nav()
        if (base_nav == 0) next
        base_qty <- tryCatch(
          {item$portfolio$get_position(security_id)$get_qty()},
          error = function(e) 0
        )
        if (i == trading_idx) {
          base_qty <- base_qty + base_trade_qty
          # Marginal = the delta this trade adds to the SMA's scaled target.
          marginal_qty <- item$weight * base_trade_qty * sma_nav / base_nav
        }
        unconstrained_target_qty <- unconstrained_target_qty +
          item$weight * base_qty * sma_nav / base_nav
      }
      res <- private$.unconst_to_const_shares(
        portfolio,
        security_id,
        unconstrained_target_qty
      )
      # Drift = the rest of the unconstrained trade, i.e. correcting the SMA's
      # pre-existing mis-allocation (0 if it was already on target).
      res$marginal_shares <- marginal_qty
      res$drift_shares <-
        res$unconstrained_target_shares - res$current_shares - marginal_qty
      res
    },

    #' @description Replicate a trade from the base portfolio to the SMA
    #' based on trade as percentage of base portfolio NAV
    #' @param security_id Security ID of the traded security in the base
    #'  portfolio
    #' @param base_trade_pct Trade percentage in the base portfolio
    #' @param portfolio SMA portfolio object
    #' @param base_portfolio_name Short name of the trading base portfolio.
    #'  Required for SMAs with blended bases; defaults to the primary base
    #'  portfolio when NULL.
    #' @return A list with trade details and calculations
    replicate_trade_pct = function(
      security_id,
      base_trade_pct,
      portfolio,
      base_portfolio_name = NULL
    ) {
      checkmate::assert_character(security_id, len = 1)
      checkmate::assert_numeric(base_trade_pct, len = 1)
      checkmate::assert_r6(portfolio, "SMA")
      security <- .security(security_id, create = TRUE)

      base_list <- portfolio$get_base_portfolios()
      trading_idx <- private$.get_base_idx(base_list, base_portfolio_name)

      sma_nav <- portfolio$get_nav()
      unconstrained_target_qty <- 0
      for (i in seq_along(base_list)) {
        item <- base_list[[i]]
        base_nav <- item$portfolio$get_nav()
        if (base_nav == 0) next
        base_qty <- tryCatch(
          {item$portfolio$get_position(security_id)$get_qty()},
          error = function(e) 0
        )
        if (i == trading_idx) {
          base_qty <- base_qty +
            base_trade_pct * base_nav / security$get_price()
        }
        unconstrained_target_qty <- unconstrained_target_qty +
          item$weight * base_qty * sma_nav / base_nav
      }
      private$.unconst_to_const_shares(
        portfolio,
        security_id,
        unconstrained_target_qty
      )
    },

    #' @description Replicate a set of base-portfolio trades into this SMA,
    #'  \emph{replacement-aware}. When a rule prevents the SMA from holding a
    #'  replaced ("source") security in full, the un-holdable weight (the
    #'  overflow) is routed into its replacement ("target") securities split by
    #'  weight - the same routing the rebalance optimizer applies via
    #'  \code{OverflowRule}. This is what \code{replicate_trade_qty} (a pure
    #'  per-security marginal) cannot do: it would liquidate a replacement
    #'  position because the base doesn't hold it. Handles blended bases and
    #'  computes overflow from the post-trade base holdings.
    #' @param portfolio SMA portfolio object.
    #' @param base_trades Named numeric of trade quantities keyed by (lowercase)
    #'  security id, applied to the trading base. Absent securities = 0.
    #' @param base_portfolio_name Short name of the trading base (for blended
    #'  bases); defaults to the primary base.
    #' @return Named list keyed by security id, each element the fields of
    #'  \code{.unconst_to_const_shares} plus \code{marginal_shares}. Covers the
    #'  traded securities and any replacement targets that receive overflow.
    replicate_base_trades = function(
      portfolio, base_trades, base_portfolio_name = NULL
    ) {
      checkmate::assert_r6(portfolio, "SMA")
      if (is.null(base_trades)) base_trades <- numeric(0)

      base_list   <- portfolio$get_base_portfolios()
      trading_idx <- private$.get_base_idx(base_list, base_portfolio_name)
      sma_nav     <- portfolio$get_nav()

      # safe trade-qty lookup (named numeric returns NA for an absent name)
      bt <- function(sec) {
        v <- base_trades[sec]
        if (length(v) == 0 || is.na(v)) 0 else unname(v)
      }
      # Scaled SMA target qty for `sec` = each base's holding (plus the trade on
      # the trading base) scaled by weight * sma_nav / base_nav.
      unconstrained_target <- function(sec) {
        tot <- 0
        for (i in seq_along(base_list)) {
          item <- base_list[[i]]
          base_nav <- item$portfolio$get_nav()
          if (base_nav == 0) next
          bq <- tryCatch(
            item$portfolio$get_position(sec)$get_qty(), error = function(e) 0
          )
          if (i == trading_idx) bq <- bq + bt(sec)
          tot <- tot + item$weight * bq * sma_nav / base_nav
        }
        tot
      }
      # Unconstrained marginal of a direct trade in `sec` (trading base only).
      marginal_of <- function(sec) {
        tq <- bt(sec)
        if (tq == 0) return(0)
        item <- base_list[[trading_idx]]
        bn <- item$portfolio$get_nav()
        if (bn == 0) return(0)
        item$weight * tq * sma_nav / bn
      }

      reps    <- portfolio$get_replacement_security()   # source -> {security,weight}
      sources <- names(reps)

      # Replication price = the exposure basis (|delta| * underlying). Overflow
      # must be routed in EXPOSURE space, not raw shares: the source and target
      # have different prices, so N shares of a $20 source replace to a different
      # share count of a $25 target. This mirrors the optimizer's weight-space
      # OverflowRule. Fallback 1 keeps a missing price from zeroing the routing.
      repl_price <- function(sec) {
        p <- tryCatch(.security(sec)$get_replication_price(), error = function(e) NA_real_)
        if (!is.finite(p) || p <= 0) 1 else p
      }

      # Overflow of a source, as EXPOSURE = (un-holdable shares) * repl_price.
      overflow_cache <- new.env(parent = emptyenv())
      get_overflow_exposure <- function(src) {
        if (is.null(overflow_cache[[src]])) {
          u <- unconstrained_target(src)
          c <- private$.unconst_to_const_shares(
            portfolio, src, u
          )$constrained_target_shares
          overflow_cache[[src]] <- (u - c) * repl_price(src)
        }
        overflow_cache[[src]]
      }

      # Emit the traded securities plus the replacement targets of any traded
      # source (so a base trade in a restricted name shows its replacement buy).
      traded  <- names(base_trades)
      routed  <- unlist(
        lapply(intersect(traded, sources), function(s) reps[[s]]$security),
        use.names = FALSE
      )
      process <- unique(c(traded, routed))

      out <- list()
      for (s in process) {
        u_s <- unconstrained_target(s)
        # Exposure routed INTO s from every source that names s as a target,
        # converted back to s's shares at s's own replication price.
        incoming_exposure <- 0
        for (src in sources) {
          tgt <- reps[[src]]$security
          k <- match(s, tgt)
          if (!is.na(k)) {
            incoming_exposure <- incoming_exposure +
              reps[[src]]$weight[k] * get_overflow_exposure(src)
          }
        }
        incoming_shares <- incoming_exposure / repl_price(s)
        res <- private$.unconst_to_const_shares(portfolio, s, u_s + incoming_shares)
        res$marginal_shares <- marginal_of(s)
        out[[s]] <- res
      }
      out
    }
  ),
  private = list(
    .get_base_idx = function(base_list, base_portfolio_name) {
      if (!is.null(base_portfolio_name)) {
        trading_idx <- which(
          vapply(base_list,
            \(x) x$portfolio$get_short_name(),
            character(1)
          ) == base_portfolio_name
        )
        if (length(trading_idx) == 0) {
          stop(sprintf(
            "Base portfolio '%s' not found in this SMA's blend.",
            base_portfolio_name
          ))
        }
      } else {
        trading_idx <- 1L
      }
      trading_idx
    },
    .unconst_to_const_shares = function(
      portfolio,
      security_id,
      unconstrained_target_qty
    ) {
      limits_as_shares <- self$get_security_position_limits(
        portfolio = portfolio,
        security_id = security_id,
        position_only = FALSE, # Use FALSE to consider all rules
        verbose = TRUE
      )[[security_id]]

      impacted_limits <- list()
      for (rule_name in names(limits_as_shares)) {
        limit <- limits_as_shares[[rule_name]]
        if (!is.infinite(limit$max) & !is.na(limit$max)) {
          impacted_limits[[rule_name]][["max"]] <- limit$max
        }
        if (!is.infinite(limit$min) & !is.na(limit$min)) {
          impacted_limits[[rule_name]][["min"]] <- limit$min
        }
      }

      limits_max <- unlist(sapply(impacted_limits, \(l) l$max))
      limits_min <- unlist(sapply(impacted_limits, \(l) l$min))

      limit_max <- if (length(limits_max) == 0) {
        Inf
      } else {
        min(limits_max, na.rm = TRUE)
      }

      limit_min <- if (length(limits_min) == 0) {
        -Inf
      } else {
        max(limits_min, na.rm = TRUE)
      }

      constrained_target_qty <- pmin(pmax(
        unconstrained_target_qty, limit_min
      ), limit_max)
      # Round to whole shares
      final_qty_rounded <- if (constrained_target_qty > 0) {
        floor(constrained_target_qty)
      } else {
        ceiling(constrained_target_qty)
      }


      if (constrained_target_qty == unconstrained_target_qty) {
        limiting_rule <- NA_character_
      } else if (constrained_target_qty == limit_min) {
        limiting_rule <- limits_min[which(limits_min == limit_min)]
      } else if (constrained_target_qty == limit_max) {
        limiting_rule <- limits_max[which(limits_max == limit_max)]
      }

      sma_pre_qty <- tryCatch(
        {portfolio$get_position(security_id)$get_qty()},
        error = function(e) 0
      )
      trade_qty <- final_qty_rounded - sma_pre_qty

      list(
        security_id = security_id,
        trade_shares = trade_qty,
        unconstrained_target_shares = unconstrained_target_qty,
        constrained_target_shares = constrained_target_qty,
        min_allowed_shares = limit_min,
        max_allowed_shares = limit_max,
        limiting_rule = limiting_rule,
        current_shares = sma_pre_qty,
        final_shares = final_qty_rounded,
        rule_limits = impacted_limits
      )
    }
  )
)