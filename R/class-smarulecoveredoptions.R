#' @title SMA Rule: Covered Options (no naked short options)
#'
#' @description
#' A cross-security rule that forbids uncovered (naked) short option positions.
#' Unlike the threshold/divisor rules, this one couples an option's position to
#' its underlying's position, grouped by underlying:
#'
#' \itemize{
#'   \item short \strong{call} shares may not exceed the \strong{long} shares
#'     held in the underlying (a short call is covered only by long stock);
#'   \item short \strong{put} shares may not exceed the \strong{short} shares
#'     held in the underlying (a short put is covered only by short stock).
#' }
#'
#' Quantities are treated as \emph{physical shares} (the stored option qty is
#' already contracts x 100), so no multiplier is applied. Physical shares for
#' any security are recovered as \code{w * nav / price}, where \code{price} is
#' the replication price \code{|delta| * underlying_price}; the \code{|delta|}
#' cancels, leaving the true share count and keeping the constraint linear
#' (DCP-affine) in \code{w}.
#'
#' Whether a leg is covered depends on the underlying's \emph{side}. As with the
#' directional \code{include} filters, the side is taken from a known constant
#' (the underlying's target weight sign) rather than the decision variable, so
#' the branch selection stays affine. When the underlying is on the wrong side
#' for coverage (or absent), the covering capacity is zero and the short leg is
#' forced to zero, i.e. an uncovered short is disallowed entirely.
#'
#' @import R6
#' @import CVXR
#' @include class-smarule.R
#' @include class-security.R
#'
#' @export
SMARuleCoveredOptions <- R6::R6Class( #nolint
  "SMARuleCoveredOptions",
  inherit = SMARule,
  private = list(
    restrict_calls_ = TRUE,
    restrict_puts_ = TRUE,
    per_contract_ = FALSE,

    # Split the ids in the book into underlying groups. Returns a named list
    # keyed by (lowercased) underlying id, each element a list with integer
    # index vectors: call_idx, put_idx (option legs) and und_idx (the position
    # of the underlying equity in `ids`, or NA_integer_ if not in the book).
    .group_by_underlying = function(ids) {
      lc <- tolower(ids)
      groups <- list()
      for (i in seq_along(ids)) {
        sec <- tryCatch(.security(ids[i]), error = function(e) NULL)
        if (is.null(sec)) next
        if (!.is_option_type(sec$get_instrument_type())) next
        u_id <- tolower(sec$get_underlying_security()$get_id())
        is_call <- sec$get_delta() >= 0
        g <- groups[[u_id]]
        if (is.null(g)) g <- list(call_idx = integer(0), put_idx = integer(0))
        if (is_call) {
          g$call_idx <- c(g$call_idx, i)
        } else {
          g$put_idx <- c(g$put_idx, i)
        }
        groups[[u_id]] <- g
      }
      # Attach the underlying equity index for each group.
      for (u_id in names(groups)) {
        u_idx <- match(u_id, lc)
        groups[[u_id]]$und_idx <- if (is.na(u_idx)) NA_integer_ else u_idx
      }
      groups
    }
  ),
  public = list(
    #' @description Create a covered-options rule.
    #' @param sma_name Character
    #' @param rule_id Integer
    #' @param name Character
    #' @param restrict_calls Logical. Govern short calls (default TRUE).
    #' @param restrict_puts Logical. Govern short puts (default TRUE).
    #' @param per_contract Logical. When FALSE (default) coverage nets across an
    #'  underlying's option legs, so a long option frees up capacity for a short
    #'  one. When TRUE, coverage is measured strictly on the \emph{short} legs
    #'  only (selected by target-weight sign): a long call cannot offset a naked
    #'  short call. Both modes stay linear in \code{w}.
    #' @param exclusions Character vector of security IDs exempt from the rule.
    #' @param ... Passed to \code{SMARule$new()}.
    initialize = function(
      sma_name = NULL, rule_id = NULL, name = NULL,
      restrict_calls = TRUE, restrict_puts = TRUE, per_contract = FALSE,
      exclusions = NULL, ...
    ) {
      super$initialize(
        sma_name = sma_name, rule_id = rule_id, name = name,
        scope = "position", exclusions = exclusions, ...
      )
      private$restrict_calls_ <- isTRUE(restrict_calls)
      private$restrict_puts_ <- isTRUE(restrict_puts)
      private$per_contract_ <- isTRUE(per_contract)
    },

    #' @description Whether short calls are governed by the rule.
    get_restrict_calls = function() private$restrict_calls_,
    #' @description Whether short puts are governed by the rule.
    get_restrict_puts = function() private$restrict_puts_,
    #' @description Whether coverage is measured per-contract (no netting).
    get_per_contract = function() private$per_contract_,

    #' @description This rule imposes no swap requirement.
    #' @param security_id Security ID(s)
    check_swap_security = function(security_id) {
      stats::setNames(rep(FALSE, length(security_id)), security_id)
    },

    #' @description Build optimizer constraints coupling each option leg to its
    #'  underlying. All expressions are linear in \code{ctx$w}.
    #' @param ctx ModelContext
    #' @param nav Portfolio NAV
    build_constraints = function(ctx, nav) {
      groups <- private$.group_by_underlying(ctx$ids)
      if (!length(groups)) return(list())

      # Physical-share coefficient for each id: shares_i = coef_i * w_i.
      coef <- ctx$nav / ctx$price
      coef[!is.finite(coef)] <- 0
      excl <- self$get_exclusions()
      lc_ids <- tolower(ctx$ids)

      # Per-contract coverage counts only the short legs (target-weight sign is
      # a known constant, so the selected set is fixed and the sum stays affine);
      # net coverage counts every leg, letting a long option offset a short one.
      leg_set <- function(idx) {
        idx <- idx[!(lc_ids[idx] %in% excl)]
        if (private$per_contract_) idx <- idx[ctx$t_w[idx] < 0]
        idx
      }
      share_sum <- function(idx) {
        idx <- leg_set(idx)
        if (!length(idx)) return(NULL)
        CVXR::sum_entries(coef[idx] * ctx$w[idx])
      }

      cons <- list()
      for (u_id in names(groups)) {
        if (u_id %in% excl) next
        g <- groups[[u_id]]

        # Underlying side and its (variable) share expression.
        if (is.na(g$und_idx)) {
          side <- 0
          und_shares <- 0
        } else {
          side <- sign(ctx$t_w[g$und_idx])
          und_shares <- coef[g$und_idx] * ctx$w[g$und_idx]
        }

        # Calls: short calls need long stock.
        if (private$restrict_calls_) {
          sc <- share_sum(g$call_idx)
          if (!is.null(sc)) {
            if (side > 0) {
              # covered: sum(call shares) >= -long_underlying_shares
              cons <- c(cons, list(sc + und_shares >= 0))
            } else {
              # no long stock to cover -> no net short calls
              cons <- c(cons, list(sc >= 0))
            }
          }
        }

        # Puts: short puts need short stock.
        if (private$restrict_puts_) {
          sp <- share_sum(g$put_idx)
          if (!is.null(sp)) {
            if (side < 0) {
              # covered: sum(put shares) >= short_underlying_shares (<= 0)
              cons <- c(cons, list(sp - und_shares >= 0))
            } else {
              # no short stock to cover -> no net short puts
              cons <- c(cons, list(sp >= 0))
            }
          }
        }
      }
      cons
    },

    #' @description Per-security share limits for the single-name trade-matching
    #'  path. The rest of the book (\code{qty_all}) is held fixed, matching how
    #'  the other rules' limits treat the current portfolio.
    #' @param security_id Vector of security IDs to compute limits for.
    #' @param ids_all Character vector of all security IDs in the book.
    #' @param qty_all Numeric vector of physical shares aligned to \code{ids_all}.
    #' @param nav Numeric NAV (unused; kept for signature compatibility).
    #' @param prices_all Optional; unused (limits are in shares).
    #' @param f_all Optional; unused.
    #' @return Named list of \code{list(max, min)} per security.
    get_security_limits = function(
      security_id, ids_all, qty_all, nav, prices_all = NULL, f_all = NULL
    ) {
      lc_all <- tolower(ids_all)
      excl <- self$get_exclusions()
      groups <- private$.group_by_underlying(ids_all)

      # Coverable share sum for a set of legs: per-contract counts only the
      # short (negative-qty) legs, so a long option never adds capacity; net
      # counts every leg.
      cov_sum <- function(idx) {
        idx <- idx[!(lc_all[idx] %in% excl)]
        if (!length(idx)) return(0)
        q <- qty_all[idx]
        if (private$per_contract_) sum(q[q < 0]) else sum(q)
      }

      out <- lapply(security_id, function(sec) {
        lc <- tolower(sec)
        if (lc %in% excl) return(list(max = Inf, min = -Inf))
        idx <- match(lc, lc_all)
        if (is.na(idx)) return(list(max = Inf, min = -Inf))

        s <- .security(sec)
        # ---- Option leg: bounded by its underlying's covering capacity -------
        if (.is_option_type(s$get_instrument_type())) {
          u_id <- tolower(s$get_underlying_security()$get_id())
          g <- groups[[u_id]]
          if (is.null(g)) return(list(max = Inf, min = -Inf))
          u_qty <- if (is.na(g$und_idx)) 0 else qty_all[g$und_idx]
          is_call <- s$get_delta() >= 0

          if (is_call && private$restrict_calls_) {
            other <- cov_sum(setdiff(g$call_idx, idx))
            floor_qty <- if (u_qty > 0) -u_qty - other else -other
            return(list(max = Inf, min = floor_qty))
          }
          if (!is_call && private$restrict_puts_) {
            other <- cov_sum(setdiff(g$put_idx, idx))
            floor_qty <- if (u_qty < 0) u_qty - other else -other
            return(list(max = Inf, min = floor_qty))
          }
          return(list(max = Inf, min = -Inf))
        }

        # ---- Underlying equity: must cover the existing short option legs ----
        g <- groups[[lc]]
        if (is.null(g)) return(list(max = Inf, min = -Inf))
        sum_calls <- cov_sum(g$call_idx)
        sum_puts <- cov_sum(g$put_idx)
        # Long enough to cover short calls; short enough to cover short puts.
        min_q <- if (private$restrict_calls_ && sum_calls < 0) -sum_calls else -Inf
        max_q <- if (private$restrict_puts_ && sum_puts < 0) sum_puts else Inf
        list(max = max_q, min = min_q)
      })
      names(out) <- security_id
      out
    },

    #' @description Post-hoc compliance check over raw share quantities.
    #' @param ids Character vector of security IDs.
    #' @param qty Numeric vector of physical shares.
    #' @param nav Numeric NAV (unused).
    #' @param prices Optional; unused.
    #' @param tolerance Numeric share tolerance.
    #' @param ... Unused.
    #' @return list(pass = TRUE) or list(pass = FALSE, non_comply = ...).
    check_compliance = function(
      ids, qty, nav, prices = NULL, tolerance = 1e-6, ...
    ) {
      excl <- self$get_exclusions()
      groups <- private$.group_by_underlying(ids)
      lc_ids <- tolower(ids)
      non_comply <- character(0)

      for (u_id in names(groups)) {
        if (u_id %in% excl) next
        g <- groups[[u_id]]
        call_idx <- g$call_idx[!(lc_ids[g$call_idx] %in% excl)]
        put_idx  <- g$put_idx[!(lc_ids[g$put_idx] %in% excl)]
        u_qty <- if (is.na(g$und_idx)) 0 else qty[g$und_idx]

        # Short share count: per-contract sums only the short legs; net lets a
        # long leg offset (floored at 0 so a net-long book needs no cover).
        short_shares <- function(idx) {
          q <- qty[idx]
          if (private$per_contract_) -sum(q[q < 0]) else max(0, -sum(q))
        }

        if (private$restrict_calls_ && length(call_idx)) {
          long_und <- max(0, u_qty)
          if (short_shares(call_idx) > long_und + tolerance) {
            non_comply <- c(non_comply, ids[call_idx])
          }
        }
        if (private$restrict_puts_ && length(put_idx)) {
          short_und <- max(0, -u_qty)
          if (short_shares(put_idx) > short_und + tolerance) {
            non_comply <- c(non_comply, ids[put_idx])
          }
        }
      }

      if (!length(non_comply)) {
        list(pass = TRUE)
      } else {
        list(pass = FALSE, non_comply = unique(non_comply))
      }
    }
  )
)
