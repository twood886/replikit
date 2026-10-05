# Covered-options rule: short calls must be backed by long underlying shares,
# short puts by short underlying shares. Quantities are physical shares (the
# option qty is already contracts x 100). Covers netting vs per-contract modes
# across check_compliance and get_security_limits.

make_covered_provider <- function() {
  provider <- StaticDataProvider$new()
  provider$add_security(
    "aapl us equity",
    description = "Apple Inc", instrument_type = "Equity", price = 200
  )
  # Two calls (delta > 0) and one put (delta < 0) on the same underlying.
  provider$add_security(
    "aapl c1 equity",
    description = "AAPL Call 1", instrument_type = "Listed Option",
    price = 100, delta = 0.5, underlying_id = "aapl us"
  )
  provider$add_security(
    "aapl c2 equity",
    description = "AAPL Call 2", instrument_type = "Listed Option",
    price = 80, delta = 0.4, underlying_id = "aapl us"
  )
  provider$add_security(
    "aapl p1 equity",
    description = "AAPL Put 1", instrument_type = "Listed Option",
    price = 60, delta = -0.3, underlying_id = "aapl us"
  )
  provider
}

with_covered_provider <- function(env = parent.frame()) {
  pkg_state <- asNamespace("replikit")$.pkg_state
  old_provider <- pkg_state$security_data_provider
  provider <- make_covered_provider()
  set_security_data_provider(provider)

  reg <- get_registries()$securities
  rm(list = ls(reg), envir = reg)
  # Register the securities so option -> underlying links resolve.
  invisible(lapply(
    c("aapl us equity", "aapl c1 equity", "aapl c2 equity", "aapl p1 equity"),
    .security
  ))

  withr::defer(
    {
      pkg_state$security_data_provider <- old_provider
      rm(list = ls(reg), envir = reg)
    },
    envir = env
  )
  provider
}

new_rule <- function(per_contract = FALSE,
                     restrict_calls = TRUE, restrict_puts = TRUE) {
  SMARuleCoveredOptions$new(
    sma_name = "TestSMA", rule_id = 1L, name = "No naked options",
    restrict_calls = restrict_calls, restrict_puts = restrict_puts,
    per_contract = per_contract
  )
}

# ---- check_compliance -------------------------------------------------------

test_that("covered short call passes; naked short call fails", {
  with_covered_provider()
  rule <- new_rule()

  ok <- rule$check_compliance(
    ids = c("aapl us equity", "aapl c1 equity"), qty = c(300, -200), nav = 1e6
  )
  expect_true(ok$pass)

  bad <- rule$check_compliance(
    ids = c("aapl us equity", "aapl c1 equity"), qty = c(100, -200), nav = 1e6
  )
  expect_false(bad$pass)
  expect_true("aapl c1 equity" %in% bad$non_comply)
})

test_that("short put covered by short stock passes; against long stock fails", {
  with_covered_provider()
  rule <- new_rule()

  ok <- rule$check_compliance(
    ids = c("aapl us equity", "aapl p1 equity"), qty = c(-300, -200), nav = 1e6
  )
  expect_true(ok$pass)

  bad <- rule$check_compliance(
    ids = c("aapl us equity", "aapl p1 equity"), qty = c(300, -200), nav = 1e6
  )
  expect_false(bad$pass)
  expect_true("aapl p1 equity" %in% bad$non_comply)
})

test_that("netting vs per-contract diverge when a long call offsets a short", {
  with_covered_provider()
  ids <- c("aapl us equity", "aapl c1 equity", "aapl c2 equity")
  qty <- c(100, -200, 300) # net calls +100 (long); short leg alone -200

  expect_true(new_rule(per_contract = FALSE)$check_compliance(ids, qty, 1e6)$pass)

  pc <- new_rule(per_contract = TRUE)$check_compliance(ids, qty, 1e6)
  expect_false(pc$pass)
  expect_true("aapl c1 equity" %in% pc$non_comply)
})

test_that("restrict_puts = FALSE leaves naked short puts alone", {
  with_covered_provider()
  rule <- new_rule(restrict_puts = FALSE)
  res <- rule$check_compliance(
    ids = c("aapl us equity", "aapl p1 equity"), qty = c(300, -200), nav = 1e6
  )
  expect_true(res$pass)
})

# ---- get_security_limits ----------------------------------------------------

test_that("call floor equals negative of long underlying shares", {
  with_covered_provider()
  rule <- new_rule()
  lim <- rule$get_security_limits(
    security_id = "aapl c1 equity",
    ids_all = c("aapl us equity", "aapl c1 equity"),
    qty_all = c(300, 0), nav = 1e6
  )
  expect_equal(lim[["aapl c1 equity"]]$min, -300)
  expect_equal(lim[["aapl c1 equity"]]$max, Inf)
})

test_that("underlying must stay long enough to cover existing short calls", {
  with_covered_provider()
  rule <- new_rule()
  lim <- rule$get_security_limits(
    security_id = "aapl us equity",
    ids_all = c("aapl us equity", "aapl c1 equity"),
    qty_all = c(0, -150), nav = 1e6
  )
  expect_equal(lim[["aapl us equity"]]$min, 150)
  expect_equal(lim[["aapl us equity"]]$max, Inf)
})

test_that("a long call adds short-call capacity only under netting", {
  with_covered_provider()
  ids_all <- c("aapl us equity", "aapl c1 equity", "aapl c2 equity")
  qty_all <- c(100, 0, 300) # c2 long 300 shares

  net <- new_rule(per_contract = FALSE)$get_security_limits(
    "aapl c1 equity", ids_all, qty_all, 1e6
  )
  expect_equal(net[["aapl c1 equity"]]$min, -400) # -100 (stock) - 300 (long c2)

  pc <- new_rule(per_contract = TRUE)$get_security_limits(
    "aapl c1 equity", ids_all, qty_all, 1e6
  )
  expect_equal(pc[["aapl c1 equity"]]$min, -100) # long c2 does not count
})

# ---- build_constraints (smoke) ----------------------------------------------

test_that("build_constraints emits a coupling for a short call on long stock", {
  with_covered_provider()
  rule <- new_rule()
  tc <- TradeConstructor$new()
  ids <- c("aapl us equity", "aapl c1 equity")
  price_vec <- c(200, 100) # equity price; call replication price = 0.5 * 200
  nav <- 1e6
  t_w <- c(300 * 200 / nav, -200 * 100 / nav) # long stock, short call
  ctx <- tc$make_model_context(ids, price_vec, nav, t_w, params = list())

  cons <- rule$build_constraints(ctx, nav)
  expect_true(length(cons) >= 1)

  # No options in the book -> no constraints.
  ids2 <- c("aapl us equity")
  ctx2 <- tc$make_model_context(ids2, c(200), nav, c(0.06), params = list())
  expect_equal(length(rule$build_constraints(ctx2, nav)), 0)
})
