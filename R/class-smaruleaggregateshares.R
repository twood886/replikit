#' @title SMA Rule: Aggregate Shares Across All Portfolios
#'
#' @description
#' A firm-level share-count cap. The shares of a security held across
#' \emph{every} portfolio in the registry (base funds and SMAs alike) may not
#' exceed \code{max_threshold} times a per-security reference field (e.g.
#' Bloomberg \code{HS021}):
#'
#' \deqn{\sum_{k \in portfolios} qty_k(i) \le max\_threshold \times field(i)}
#'
#' Each SMA carries its own copy of the rule. When this SMA is optimized, the
#' shares currently held in every \emph{other} portfolio are read from the
#' portfolio registry and subtracted from the cap, leaving this SMA the
#' remaining headroom. That headroom is floored at zero: if the other
#' portfolios already exhaust the cap, this SMA may not hold the name long, but
#' is never forced short to repair a breach it did not create.
#'
#' Only SMAs carry rules, so the cap binds SMAs only. A base fund that on its
#' own exceeds the cap leaves every SMA with zero headroom in that name.
#'
#' Quantities are physical shares (the stored option qty is already contracts x
#' 100). Physical shares for any security are recovered in the optimizer as
#' \code{w * nav / price}, which keeps the constraint linear (DCP-affine) in
#' \code{w}. Aggregation is keyed by security id, not by underlying, so an
#' option leg is capped against its own id; with \code{underlying = TRUE}
#' (default) the reference field is read from the option's underlying, since
#' options carry no share-count fundamentals of their own.
#'
#' The cap is one-sided: only long share counts are bounded, so a short
#' position always satisfies the rule. A security with no usable field value
#' (missing, non-finite or non-positive) is left unconstrained.
#'
#' @section Simultaneous rebalances:
#' The other portfolios' shares are the registry's \emph{current} holdings.
#' Rebalances computed for several SMAs from the same holdings snapshot each
#' see the same headroom and can jointly overshoot the firm cap; apply each
#' SMA's trades before optimizing the next, or rely on
#' \code{check_compliance()} to surface the resulting breach.
#'
#' @import R6
#' @import CVXR
#' @include class-smarule.R
#' @include class-security.R
#'
#' @export
SMARuleAggregateShares <- R6::R6Class( #nolint
  "SMARuleAggregateShares",
  inherit = SMARule,
  private = list(
    field_ = NULL,
    underlying_ = TRUE,

    # Per-id share cap: max_threshold * field. Inf (unconstrained) for excluded
    # ids, ids without a Security, or ids whose field value is unusable.
    .caps = function(ids) {
      excl <- self$get_exclusions()
      max_t <- self$get_max_threshold()
      caps <- vapply(ids, function(id) {
        if (!is.finite(max_t)) return(Inf)
        if (tolower(id) %in% excl) return(Inf)
        sec <- tryCatch(.security(id), error = function(e) NULL)
        if (is.null(sec)) return(Inf)
        val <- tryCatch(
          sec$get_rule_data(private$field_, underlying = private$underlying_),
          error = function(e) NA_real_
        )
        val <- suppressWarnings(as.numeric(val))
        if (length(val) != 1 || !is.finite(val) || val <= 0) return(Inf)
        max_t * val
      }, numeric(1))
      names(caps) <- ids
      caps
    }
  ),
  public = list(
    #' @description Create an aggregate-shares rule.
    #' @param sma_name Character. Short name of the SMA the rule belongs to
    #'  (used to exclude that SMA when summing the others' holdings).
    #' @param rule_id Integer
    #' @param name Character
    #' @param field Character. Field mnemonic holding the per-security share
    #'  reference (e.g. \code{"HS021"}). Registered as the rule's
    #'  \code{bbfields} so it is fetched with the other rule fields.
    #' @param max_threshold Numeric. Fraction of \code{field} the firm-wide
    #'  share count may not exceed (e.g. \code{0.20}).
    #' @param underlying Logical. Read \code{field} from an option's underlying
    #'  (default \code{TRUE}).
    #' @param exclusions Character vector of security IDs exempt from the rule.
    #' @param grandfather Logical. When \code{TRUE} this SMA may hold (but not
    #'  increase) a position that already breaches the firm cap.
    #' @param ... Passed to \code{SMARule$new()}.
    initialize = function(
      sma_name = NULL, rule_id = NULL, name = NULL,
      field = "HS021", max_threshold = 0.20, underlying = TRUE,
      exclusions = NULL, grandfather = FALSE, ...
    ) {
      checkmate::assert_string(field)
      checkmate::assert_number(max_threshold)
      checkmate::assert_flag(underlying)
      super$initialize(
        sma_name = sma_name, rule_id = rule_id, name = name,
        scope = "aggregate_shares", bbfields = field,
        max_threshold = max_threshold, min_threshold = -Inf,
        relative_to = "nav", exclusions = exclusions,
        grandfather = grandfather, ...
      )
      private$field_ <- field
      private$underlying_ <- underlying
    },

    #' @description The reference field mnemonic.
    get_field = function() private$field_,
    #' @description Whether the field is read from an option's underlying.
    get_underlying = function() private$underlying_,

    #' @description Shares of each id currently held across every \emph{other}
    #'  portfolio in the registry, base funds included (this rule's own SMA
    #'  excluded).
    #' @param ids Character vector of security IDs.
    #' @return Named numeric vector aligned to \code{ids} (0 when unheld).
    get_other_portfolio_shares = function(ids) {
      out <- stats::setNames(rep(0, length(ids)), ids)
      if (!length(ids)) return(out)
      lc <- tolower(ids)
      reg <- get_registries()$portfolios
      self_name <- self$get_sma_name()
      self_name <- if (is.null(self_name)) "" else tolower(self_name)
      for (nm in ls(reg, all.names = TRUE)) {
        p <- get(nm, envir = reg)
        if (!inherits(p, "Portfolio")) next
        if (identical(tolower(p$get_short_name()), self_name)) next
        for (pos in p$get_position()) {
          j <- match(tolower(pos$get_id()), lc)
          if (!is.na(j)) out[j] <- out[j] + pos$get_qty()
        }
      }
      out
    },

    #' @description Firm-wide share cap per id (\code{max_threshold * field});
    #'  \code{Inf} where the rule does not bind.
    #' @param ids Character vector of security IDs.
    get_share_caps = function(ids) private$.caps(ids),

    #' @description This rule imposes no swap requirement.
    #' @param security_id Security ID(s)
    check_swap_security = function(security_id) {
      stats::setNames(rep(FALSE, length(security_id)), security_id)
    },

    #' @description Whether the rule binds a security (it has a finite cap).
    #' @param security_id Security ID(s)
    #' @param nav Unused.
    security_impacted = function(security_id, nav = NULL) {
      any(is.finite(private$.caps(security_id)))
    },

    #' @description Build optimizer constraints: for each capped id, this SMA's
    #'  share count (\code{w * nav / price}) may not exceed the headroom left
    #'  by the other portfolios. Linear in \code{ctx$w}.
    #' @param ctx ModelContext
    #' @param nav Portfolio NAV
    build_constraints = function(ctx, nav) {
      caps <- private$.caps(ctx$ids)
      coef <- ctx$nav / ctx$price
      coef[!is.finite(coef) | coef <= 0] <- 0
      idx <- which(is.finite(caps) & coef > 0)
      if (!length(idx)) return(list())

      other <- self$get_other_portfolio_shares(ctx$ids)
      headroom <- pmax(caps - other, 0)

      # Grandfathered: an existing breach may be held but not increased. The
      # headroom stretches to this SMA's current share count when that is the
      # looser bound. A fresh name (0 current) keeps the absolute headroom.
      if (self$get_grandfather()) {
        w_cur <- ctx$w_current
        if (is.null(w_cur)) w_cur <- rep(0, length(ctx$ids))
        own_cur <- coef * w_cur
        own_cur[!is.finite(own_cur)] <- 0
        headroom <- pmax(headroom, own_cur)
      }

      list(coef[idx] * ctx$w[idx] <= headroom[idx])
    },

    #' @description Per-security share limits for the single-name trade path:
    #'  max is the headroom left by the other portfolios (floored at 0), min is
    #'  unbounded. Grandfather widening is applied by the caller.
    #' @param security_id Vector of security IDs to compute limits for.
    #' @param ids_all Character vector of all security IDs in the book (unused).
    #' @param qty_all Numeric vector of shares aligned to \code{ids_all} (unused).
    #' @param nav Numeric NAV (unused).
    #' @param prices_all Optional; unused (limits are in shares).
    #' @param f_all Optional; unused.
    #' @return Named list of \code{list(max, min)} per security.
    get_security_limits = function(
      security_id, ids_all, qty_all, nav, prices_all = NULL, f_all = NULL
    ) {
      caps <- private$.caps(security_id)
      other <- self$get_other_portfolio_shares(security_id)
      out <- lapply(seq_along(security_id), function(i) {
        if (!is.finite(caps[i])) return(list(max = Inf, min = -Inf))
        list(max = max(caps[i] - other[i], 0), min = -Inf)
      })
      names(out) <- security_id
      out
    },

    #' @description Post-hoc compliance check. A name fails when the firm-wide
    #'  share count (this SMA plus the others) exceeds its cap and this SMA
    #'  holds a long position in it, i.e. contributes to the breach.
    #' @param ids Character vector of security IDs.
    #' @param qty Numeric vector of physical shares held in this SMA.
    #' @param nav Numeric NAV (unused).
    #' @param prices Optional; unused.
    #' @param tolerance Numeric share tolerance.
    #' @param ... Unused.
    #' @return \code{list(pass = TRUE)} or a failure list with
    #'  \code{non_comply}, the firm-wide \code{aggregate_shares} and
    #'  \code{cap_shares} for the failing names, and \code{passive} set for a
    #'  grandfathered rule.
    check_compliance = function(
      ids, qty, nav, prices = NULL, tolerance = 1e-6, ...
    ) {
      if (!length(ids)) return(list(pass = TRUE))
      caps <- private$.caps(ids)
      other <- self$get_other_portfolio_shares(ids)
      total <- qty + other
      viol <- is.finite(caps) & qty > tolerance & total > caps + tolerance

      if (!any(viol)) {
        list(pass = TRUE)
      } else {
        list(
          pass             = FALSE,
          passive          = self$get_grandfather(),
          violates_max     = TRUE,
          violates_min     = FALSE,
          non_comply       = ids[viol],
          aggregate_shares = stats::setNames(total[viol], ids[viol]),
          cap_shares       = stats::setNames(caps[viol], ids[viol])
        )
      }
    }
  )
)
