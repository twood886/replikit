#' OverflowRule
#' @export
OverflowRule <- R6::R6Class(
  "OverflowRule",
  private = list(
    replacements_ = NULL
  ),
  public = list(
    #' @description Create a new OverflowRule R6 object.
    #' @param replacements Named list of replacements. Each name is a source
    #' security ID, and each element is a list with \code{security} (a
    #' character vector of target security IDs) and \code{weight} (a numeric
    #' vector of the same length giving each target's fixed share of the
    #' source's overflow; must sum to 1).
    initialize = function(replacements) {
      private$replacements_ <- replacements
    },
    #' @description Get the scope of the rule.
    #' @return Character. Always "portfolio"; used by the optimizer to route
    #' this rule away from the position-count (MILP) phase.
    get_scope = function() "portfolio",
    #' @description Build CVXR constraints for the rule
    #' @param ctx Context object with optimization variables and parameters
    #' @param nav Numeric portfolio NAV. Accepted for a uniform rule interface
    #' but unused; overflow constraints are expressed purely in weight space.
    build_constraints = function(ctx, nav = NULL) {
      cons <- list()
      ids <- ctx$ids
      t_w <- ctx$t_w
      w <- ctx$w
      a <- ctx$alpha

      if (!length(private$replacements_)) return(cons)

      # Accumulate every target's total incoming overflow across ALL sources
      # before emitting constraints. A target fed by more than one source
      # (e.g. a call and a put on the same underlying, both replaced by that
      # underlying's equity) must get a SINGLE aggregated equality. Emitting a
      # separate fixed-share equality per (source, target) pair pins the
      # target's weight to two different values at once, which is infeasible
      # whenever another rule (e.g. "No OTC Options") forces the sources to
      # zero and thereby fixes each overflow to a constant.
      target_overflow <- list() # keyed by stringified target index in `ids`

      for (src in names(private$replacements_)) {
        i <- match(src, ids)
        entry <- private$replacements_[[src]]
        tgt_ids <- as.character(entry$security)
        tgt_weight <- as.numeric(entry$weight)
        js <- match(tgt_ids, ids)
        keep <- !is.na(js)
        js <- js[keep]
        tgt_weight <- tgt_weight[keep]
        if (is.na(i) || !length(js)) next
        # Targets absent from this optimization's universe drop out; rescale
        # the remaining weights so the present targets still absorb all of
        # the source's overflow.
        tgt_weight <- tgt_weight / sum(tgt_weight)

        # Source clamp: a replacement only ever reduces the source toward
        # zero, never increases it.
        cons <- c(
          cons,
          list(
            if (t_w[i] >= 0) {
              w[i] <= a * t_w[i]
            } else {
              w[i] >= a * t_w[i]
            }
          )
        )

        overflow <- a * t_w[i] - w[i]
        for (k in seq_along(js)) {
          key <- as.character(js[k])
          contrib <- tgt_weight[k] * overflow
          target_overflow[[key]] <- if (is.null(target_overflow[[key]])) {
            contrib
          } else {
            target_overflow[[key]] + contrib
          }
        }
      }

      # One equality per target: its weight moves off its own base target by
      # exactly the total overflow routed into it. No direction clamp on the
      # target - the aggregated overflow can legitimately be negative (e.g.
      # the net delta of a long call + long put basket), and the equality
      # already fully determines the target's weight.
      for (key in names(target_overflow)) {
        j <- as.integer(key)
        cons <- c(cons, list((w[j] - a * t_w[j]) == target_overflow[[key]]))
      }

      cons
    },
    #' @description Objective terms contributed by this rule (Dummy)
    #' @param ctx Context object with optimization variables and parameters
    #' @return An empty list
    objective_terms = function(ctx) list()
  )
)