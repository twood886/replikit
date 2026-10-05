# Isolate which rule makes an SMA's optimize_sma() infeasible.
# Mirrors the constraint setup in TradeConstructor$optimize_sma() up to the
# solve, then (1) checks feasibility with all rules, (2) with base only, and
# (3) dropping each rule in turn. A rule whose removal flips the status to
# "optimal"/"solved" is (one of) the conflicting constraints.
#
# Usage:
#   sma <- .sma("qube")            # or .portfolio("atom_core"), etc.
#   diagnose_infeasible(sma)

diagnose_infeasible <- function(portfolio,
                                alpha_min = 1, alpha_max = 1, tau_rel = 1e-3) {
  tc  <- portfolio$get_trade_constructor()
  nav <- portfolio$get_nav()

  tgt_qty <- tc$calc_target_quantities(portfolio)

  pos <- portfolio$get_position()
  current_pos <- if (!length(pos)) {
    numeric(0)
  } else {
    q <- vapply(pos, function(p) p$get_qty(), numeric(1))
    q[!is.finite(q)] <- 0
    setNames(q, vapply(pos, function(p) p$get_id(), character(1)))
  }

  replacements <- portfolio$get_replacement_security()
  t_ids <- if (length(replacements)) {
    unique(unlist(lapply(replacements, `[[`, "security"), use.names = FALSE))
  } else {
    character(0)
  }

  sec_ids <- unique(c(names(tgt_qty), names(current_pos), t_ids))

  price_vec <- vapply(
    sec_ids, function(s) .security(s)$get_replication_price(), numeric(1)
  )
  price_vec[!is.finite(price_vec) | price_vec <= 0] <- 1

  tgt_qty_v <- vapply(
    sec_ids, function(s) if (s %in% names(tgt_qty)) tgt_qty[[s]] else 0, numeric(1)
  )
  t_w <- (price_vec * tgt_qty_v) / nav

  params <- list(lambda_alpha = 10, tau_rel = tau_rel, beta_free = 5,
                 alpha_min = alpha_min, alpha_max = alpha_max)
  ctx <- tc$make_model_context(sec_ids, price_vec, nav, t_w, params)
  w <- ctx$w; alpha <- ctx$alpha

  # ---- base constraints (same as optimize_sma) ----
  t_w_nz <- t_w[abs(t_w) > 1e-12]
  base_cons <- list(alpha >= alpha_min, alpha <= alpha_max)
  zero_target_ids  <- setdiff(sec_ids, c(names(t_w_nz), t_ids))
  zero_not_targets <- match(zero_target_ids, sec_ids)
  if (length(zero_not_targets)) {
    base_cons <- c(base_cons, list(w[zero_not_targets] == 0))
  }

  # ---- rules (add OverflowRule for replacements; skip MILP count rules) ----
  rules <- portfolio$get_rules()
  if (length(replacements) > 0) rules <- c(rules, list(OverflowRule$new(replacements)))
  rules <- Filter(function(r) r$get_scope() != "count", rules)

  rname <- function(r) tryCatch(r$get_name(), error = function(e) class(r)[1])
  rule_names <- vapply(rules, rname, character(1))
  rule_cons  <- lapply(rules, function(r) {
    tryCatch(r$build_constraints(ctx, nav),
             error = function(e) structure(list(), build_error = conditionMessage(e)))
  })

  # flag any rule that failed to even build its constraints (e.g. non-DCP)
  for (k in seq_along(rules)) {
    be <- attr(rule_cons[[k]], "build_error")
    if (!is.null(be)) cat(sprintf("BUILD ERROR [%s]: %s\n", rule_names[k], be))
  }

  feas <- function(cons) {
    p <- CVXR::Problem(CVXR::Minimize(0), cons)
    tryCatch(CVXR::psolve(p, solver = "CLARABEL"), error = function(e) NULL)
    CVXR::status(p)
  }

  full <- c(base_cons, unlist(rule_cons, recursive = FALSE))
  cat(sprintf("\n%-28s %s\n", "FULL (base + all rules):", feas(full)))
  cat(sprintf("%-28s %s\n\n", "BASE only:", feas(base_cons)))

  for (k in seq_along(rules)) {
    cons_k <- c(base_cons, unlist(rule_cons[-k], recursive = FALSE))
    cat(sprintf("drop %-24s %s\n", paste0("[", rule_names[k], "]"), feas(cons_k)))
  }
  invisible(NULL)
}
