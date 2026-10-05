# Aggregate-shares rule: the physical shares of a security held across ALL
# portfolios in the registry (base funds and SMAs) may not exceed max_threshold
# x a per-security field (HS021). Each SMA sees every other portfolio's current
# holdings as consumed headroom. Covers the registry aggregation,
# check_compliance, get_security_limits, the single-name trade path, the
# optimizer path (incl. grandfathering) and the .sma_rule factory.

# Swap in a provider and give the test clean securities / portfolios / rules
# registries, restoring them afterwards.
with_clean_registry <- function(provider, env = parent.frame()) {
  pkg_state <- asNamespace("replikit")$.pkg_state
  old_provider <- pkg_state$security_data_provider
  set_security_data_provider(provider)

  sec_reg <- get_registries()$securities
  saved_secs <- mget(ls(sec_reg), envir = sec_reg)
  rm(list = ls(sec_reg), envir = sec_reg)

  port_reg <- get_registries()$portfolios
  ports_before <- ls(port_reg)
  rule_reg <- get_registries()$smarules
  rules_before <- ls(rule_reg)

  withr::defer(
    {
      pkg_state$security_data_provider <- old_provider
      rm(list = ls(sec_reg), envir = sec_reg)
      list2env(saved_secs, envir = sec_reg)
      new_ports <- setdiff(ls(port_reg), ports_before)
      if (length(new_ports)) rm(list = new_ports, envir = port_reg)
      new_rules <- setdiff(ls(rule_reg), rules_before)
      if (length(new_rules)) rm(list = new_rules, envir = rule_reg)
    },
    envir = env
  )
}

# aaa: HS021 = 10,000 -> firm cap 2,000 shares at 20%.
# bbb: no HS021 -> unconstrained.
# aaa c1: call on aaa (delta 0.5), no field of its own.
make_provider <- function() {
  provider <- StaticDataProvider$new()
  provider$add_security(
    "aaa us equity", instrument_type = "Equity", price = 100,
    fields = list(HS021 = 10000)
  )
  provider$add_security(
    "bbb us equity", instrument_type = "Equity", price = 50
  )
  provider$add_security(
    "aaa c1 equity", description = "AAA Call", instrument_type = "Listed Option",
    price = 50, delta = 0.5, underlying_id = "aaa us"
  )
  provider
}

# Registers the securities, a base portfolio ("agg_base", holding `base_aaa`
# shares of aaa), the SMA under test ("agg_sma") and a second SMA ("agg_other")
# holding `other_aaa` shares of aaa. Both the base and the other SMA count
# against agg_sma's headroom.
setup_book <- function(other_aaa = 1500, base_aaa = 0, set_field = TRUE) {
  .security("aaa us equity")
  .security("bbb us equity")
  .security("aaa c1 equity")
  if (set_field) .security("aaa us equity")$set_rule_data("HS021", 10000)

  base <- .portfolio(
    "agg_base", "Aggregate Base",
    nav = 1e6, positions = list(), create = TRUE
  )
  if (base_aaa != 0) base$add_holding(.holding("aaa us equity", base_aaa))
  sma <- .sma(
    "agg_sma", "Aggregate SMA",
    nav = 1e6, positions = list(), base_portfolio = "agg_base", create = TRUE
  )
  other <- .sma(
    "agg_other", "Aggregate Other SMA",
    nav = 1e6, positions = list(), base_portfolio = "agg_base", create = TRUE
  )
  if (other_aaa != 0) other$add_holding(.holding("aaa us equity", other_aaa))
  list(base = base, sma = sma, other = other)
}

new_rule <- function(sma_name = "agg_sma", grandfather = FALSE,
                     underlying = TRUE, exclusions = NULL,
                     max_threshold = 0.20) {
  SMARuleAggregateShares$new(
    sma_name = sma_name, rule_id = 1L, name = "20pct HS021",
    field = "HS021", max_threshold = max_threshold, underlying = underlying,
    exclusions = exclusions, grandfather = grandfather
  )
}

ids2 <- c("aaa us equity", "bbb us equity")

# ---- registry aggregation ---------------------------------------------------

test_that("other-portfolio shares include the base fund and exclude self", {
  with_clean_registry(make_provider())
  b <- setup_book(other_aaa = 1500, base_aaa = 100)
  b$sma$add_holding(.holding("aaa us equity", 400))

  rule <- new_rule()
  other <- rule$get_other_portfolio_shares(ids2)
  expect_equal(unname(other), c(1600, 0)) # other SMA 1500 + base 100

  # Seen from the other SMA, this SMA's 400 plus the base's 100 count.
  from_other <- new_rule(sma_name = "agg_other")$get_other_portfolio_shares(ids2)
  expect_equal(unname(from_other), c(500, 0))

  caps <- rule$get_share_caps(ids2)
  expect_equal(unname(caps), c(2000, Inf))
  expect_equal(rule$get_bbfields(), "HS021")
  expect_equal(rule$get_scope(), "aggregate_shares")
})

# ---- check_compliance -------------------------------------------------------

test_that("compliance passes within the firm cap and fails beyond it", {
  with_clean_registry(make_provider())
  setup_book(other_aaa = 1500)
  rule <- new_rule()

  # 400 + 1500 = 1900 <= 2000; bbb has no field so any size passes.
  ok <- rule$check_compliance(ids = ids2, qty = c(400, 5000), nav = 1e6)
  expect_true(ok$pass)

  # 600 + 1500 = 2100 > 2000.
  bad <- rule$check_compliance(ids = ids2, qty = c(600, 5000), nav = 1e6)
  expect_false(bad$pass)
  expect_equal(bad$non_comply, "aaa us equity")
  expect_equal(unname(bad$aggregate_shares), 2100)
  expect_equal(unname(bad$cap_shares), 2000)
  expect_false(bad$passive)

  # Grandfathered -> passive breach.
  gf <- new_rule(grandfather = TRUE)$check_compliance(ids2, c(600, 0), 1e6)
  expect_false(gf$pass)
  expect_true(gf$passive)
})

test_that("base-fund shares count toward the firm total", {
  with_clean_registry(make_provider())
  setup_book(other_aaa = 1000, base_aaa = 900) # others total 1900
  rule <- new_rule()
  expect_true(rule$check_compliance(ids2, c(100, 0), 1e6)$pass)  # 2000
  expect_false(rule$check_compliance(ids2, c(101, 0), 1e6)$pass) # 2001
})

test_that("a short or flat position never fails, even if the others are over", {
  with_clean_registry(make_provider())
  setup_book(other_aaa = 2500) # others alone exceed the 2000 cap
  rule <- new_rule()
  expect_true(rule$check_compliance(ids2, c(-100, 0), 1e6)$pass)
  expect_true(rule$check_compliance(ids2, c(0, 0), 1e6)$pass)
  expect_false(rule$check_compliance(ids2, c(10, 0), 1e6)$pass)
})

# ---- get_security_limits ----------------------------------------------------

test_that("limits are the headroom left by the other portfolios, floored at 0", {
  with_clean_registry(make_provider())
  setup_book(other_aaa = 1500)
  rule <- new_rule()

  lim <- rule$get_security_limits(ids2, ids2, c(0, 0), 1e6)
  expect_equal(lim[["aaa us equity"]]$max, 500) # 2000 - 1500
  expect_equal(lim[["aaa us equity"]]$min, -Inf)
  expect_equal(lim[["bbb us equity"]]$max, Inf)

  # Excluded name is unconstrained.
  ex <- new_rule(exclusions = "aaa us equity")$get_security_limits(
    ids2, ids2, c(0, 0), 1e6
  )
  expect_equal(ex[["aaa us equity"]]$max, Inf)
})

test_that("headroom floors at zero when the base fund alone exhausts the cap", {
  with_clean_registry(make_provider())
  setup_book(other_aaa = 0, base_aaa = 2500)
  lim <- new_rule()$get_security_limits("aaa us equity", ids2, c(0, 0), 1e6)
  expect_equal(lim[["aaa us equity"]]$max, 0)
})

test_that("an option reads the field from its underlying", {
  with_clean_registry(make_provider())
  setup_book(other_aaa = 0)
  expect_equal(
    unname(new_rule(underlying = TRUE)$get_share_caps("aaa c1 equity")), 2000
  )
  expect_equal(
    unname(new_rule(underlying = FALSE)$get_share_caps("aaa c1 equity")), Inf
  )
})

# ---- portfolio-level paths --------------------------------------------------

test_that("SMA compliance and single-name limits go through the rule", {
  with_clean_registry(make_provider())
  b <- setup_book(other_aaa = 1500)
  b$sma$add_holding(.holding("aaa us equity", 600)) # 600 + 1500 > 2000
  b$sma$add_rule(new_rule())

  res <- b$sma$check_rule_compliance(verbose = FALSE)
  expect_false(res$pass)
  expect_equal(unname(unlist(res$non_compliant)), "aaa us equity")

  lim <- b$sma$get_security_position_limits("aaa us equity")[["aaa us equity"]]
  expect_equal(lim$max, 500) # absolute headroom: forces a sell-down
  expect_equal(lim$min, -Inf)
})

test_that("grandfathered single-name limits stretch to the current holding", {
  with_clean_registry(make_provider())
  b <- setup_book(other_aaa = 1500)
  b$sma$add_holding(.holding("aaa us equity", 600))
  b$sma$add_rule(new_rule(grandfather = TRUE))

  lim <- b$sma$get_security_position_limits("aaa us equity")[["aaa us equity"]]
  expect_equal(lim$max, 600) # hold allowed, no increase

  res <- b$sma$check_rule_compliance(verbose = FALSE)
  expect_false(res$pass)
  expect_equal(unname(unlist(res$passive_breach)), "aaa us equity")
})

test_that("replicate_trade_qty clamps the SMA to the firm headroom", {
  with_clean_registry(make_provider())
  # Base 400 + other SMA 1500 = 1900 -> headroom 100 for this SMA.
  b <- setup_book(other_aaa = 1500, base_aaa = 400)
  b$sma$add_holding(.holding("aaa us equity", 50))
  b$sma$add_rule(new_rule())

  # Base buys 100 -> unconstrained SMA target 500; headroom is 100.
  res <- b$sma$replicate_trade_qty("aaa us equity", 100)
  expect_equal(res$constrained_target_shares, 100)
  expect_equal(res$trade_shares, 50)
  expect_equal(names(res$limiting_rule), "20pct HS021")
})

# ---- optimizer path ---------------------------------------------------------

test_that("build_constraints emits one vector constraint over capped names", {
  with_clean_registry(make_provider())
  setup_book(other_aaa = 1500)
  rule <- new_rule()
  tc <- TradeConstructor$new()
  nav <- 1e6
  price_vec <- c(100, 50)
  t_w <- c(400 * 100 / nav, 1000 * 50 / nav)
  ctx <- tc$make_model_context(ids2, price_vec, nav, t_w, params = list())

  cons <- rule$build_constraints(ctx, nav)
  expect_length(cons, 1)

  # Nothing capped (excluded) -> no constraints.
  ex <- new_rule(exclusions = "aaa us equity")
  expect_length(ex$build_constraints(ctx, nav), 0)
})

test_that("rebalance respects the firm headroom", {
  with_clean_registry(make_provider())
  # Base 400 + other SMA 1500 = 1900 -> headroom 100 for this SMA.
  b <- setup_book(other_aaa = 1500, base_aaa = 400)
  b$base$add_holding(.holding("bbb us equity", 1000))

  # Control: without the rule the SMA tracks the base one-for-one.
  free <- b$sma$rebalance()
  expect_equal(free$final_shares[free$security_id == "aaa us equity"], 400,
               tolerance = 5e-3)

  b$sma$add_rule(new_rule())
  rb <- b$sma$rebalance()
  aaa <- rb$final_shares[rb$security_id == "aaa us equity"]
  bbb <- rb$final_shares[rb$security_id == "bbb us equity"]
  expect_lt(abs(aaa - 100), 5)   # 2000 - 1900 headroom
  expect_lt(abs(bbb - 1000), 5)  # unconstrained name untouched
})

test_that("a grandfathered rebalance holds an over-cap position", {
  with_clean_registry(make_provider())
  # Base 1000 + other SMA 1500 = 2500: the others alone exceed the cap, so
  # this SMA's absolute headroom is 0. Its unconstrained target is 1000.
  b <- setup_book(other_aaa = 1500, base_aaa = 1000)
  b$sma$add_holding(.holding("aaa us equity", 900))

  b$sma$add_rule(new_rule(grandfather = TRUE))
  rb <- b$sma$rebalance()
  aaa <- rb$final_shares[rb$security_id == "aaa us equity"]
  expect_lt(abs(aaa - 900), 5) # held, not increased toward 1000, not sold

  # Without grandfather the optimizer sells down to the (zero) headroom.
  b$sma$add_rule(new_rule(grandfather = FALSE)) # same name -> replaces
  rb2 <- b$sma$rebalance()
  aaa2 <- rb2$final_shares[rb2$security_id == "aaa us equity"]
  expect_lt(abs(aaa2), 5)
})

# ---- .sma_rule factory and field fetch --------------------------------------

test_that(".sma_rule builds the rule from bbfields and fields get fetched", {
  with_clean_registry(make_provider())
  setup_book(other_aaa = 0, set_field = FALSE)

  r <- .sma_rule(
    sma_name = "agg_sma", rule_id = 7L, rule_name = "agg via factory",
    scope = "aggregate_shares", bbfields = "HS021", max_threshold = 0.20
  )
  expect_true(inherits(r, "SMARuleAggregateShares"))
  expect_equal(r$get_field(), "HS021")
  expect_true(r$get_underlying())

  # Field not yet on the security -> unconstrained until fetched.
  expect_equal(unname(r$get_share_caps("aaa us equity")), Inf)
  update_bloomberg_fields()
  expect_equal(unname(r$get_share_caps("aaa us equity")), 2000)

  # Scope without a field to read is rejected.
  expect_error(
    .sma_rule(
      sma_name = "agg_sma", rule_id = 8L, rule_name = "no field",
      scope = "aggregate_shares", max_threshold = 0.20
    ),
    "needs a field"
  )
})
