# Grandfathered rules on the single-name trade path: the per-security limit
# interval from get_security_position_limits is widened to include the current
# share count, so an existing breach can be held (or reduced) but never
# increased — and a trade is never forced to sell the position down to the
# absolute limit. Mirrors the ratio stretch the same rules apply in
# build_constraints on the optimizer path.

# Swap in a provider and give the test a clean securities registry, restoring
# both (and any portfolios it registers) afterwards.
with_clean_registry <- function(provider, env = parent.frame()) {
  pkg_state <- asNamespace("replikit")$.pkg_state
  old_provider <- pkg_state$security_data_provider
  set_security_data_provider(provider)

  sec_reg <- get_registries()$securities
  saved_secs <- mget(ls(sec_reg), envir = sec_reg)
  rm(list = ls(sec_reg), envir = sec_reg)

  port_reg <- get_registries()$portfolios
  ports_before <- ls(port_reg)

  withr::defer(
    {
      pkg_state$security_data_provider <- old_provider
      rm(list = ls(sec_reg), envir = sec_reg)
      list2env(saved_secs, envir = sec_reg)
      new_ports <- setdiff(ls(port_reg), ports_before)
      if (length(new_ports)) rm(list = new_ports, envir = port_reg)
    },
    envir = env
  )
}

make_equity_provider <- function() {
  provider <- StaticDataProvider$new()
  provider$add_security(
    "aaa us equity", instrument_type = "Equity", price = 100
  )
  provider$add_security(
    "bbb us equity", instrument_type = "Equity", price = 50
  )
  provider
}

# Position rule on NAV. The definition returns each security's per-share NAV
# weight (replication price / nav), so the absolute share limits are
# max_t * nav / price and min_t * nav / price. With nav 1e6:
#   aaa (price 100): max  500, min -400
#   bbb (price  50): max 1000, min -800
new_nav_rule <- function(sma_name, grandfather, name = "5pct nav") {
  SMARulePosition$new(
    sma_name = sma_name,
    name = name,
    scope = "position",
    definition = function(ids, nav) {
      vapply(ids, \(id) .security(id)$get_replication_price(), numeric(1)) / nav
    },
    max_threshold = 0.05,
    min_threshold = -0.04,
    grandfather = grandfather
  )
}

test_that("grandfathered limits stretch to the current breach only", {
  with_clean_registry(make_equity_provider())
  .security("aaa us equity")
  .security("bbb us equity")

  port <- .portfolio(
    "gf_port_max", "Grandfather Max Breach",
    nav = 1e6, positions = list(), create = TRUE
  )
  # 800 shares @ 100 = 8% of NAV: in breach of the 5% cap (absolute 500 sh).
  port$add_holding(.holding("aaa us equity", 800))
  port$add_rule(new_nav_rule("gf_port_max", grandfather = TRUE))

  lim <- port$get_security_position_limits("aaa us equity")[["aaa us equity"]]
  expect_equal(lim$max, 800)  # stretched to current: hold allowed, no increase
  expect_equal(lim$min, -400) # min side untouched (current is above it)

  # A name not currently held keeps the absolute limits: a fresh position
  # still can't be opened in breach.
  lim_new <- port$get_security_position_limits(
    "bbb us equity"
  )[["bbb us equity"]]
  expect_equal(lim_new$max, 1000)
  expect_equal(lim_new$min, -800)
})

test_that("non-grandfathered limits still clamp a breach absolutely", {
  with_clean_registry(make_equity_provider())
  .security("aaa us equity")

  port <- .portfolio(
    "gf_port_ctrl", "Grandfather Control",
    nav = 1e6, positions = list(), create = TRUE
  )
  port$add_holding(.holding("aaa us equity", 800))
  port$add_rule(new_nav_rule("gf_port_ctrl", grandfather = FALSE))

  lim <- port$get_security_position_limits("aaa us equity")[["aaa us equity"]]
  expect_equal(lim$max, 500)
  expect_equal(lim$min, -400)
})

test_that("grandfathered limits stretch on the short side too", {
  with_clean_registry(make_equity_provider())
  .security("aaa us equity")

  port <- .portfolio(
    "gf_port_min", "Grandfather Min Breach",
    nav = 1e6, positions = list(), create = TRUE
  )
  # -900 shares @ 100 = -9% of NAV: in breach of the -4% floor (abs. -400).
  port$add_holding(.holding("aaa us equity", -900))
  port$add_rule(new_nav_rule("gf_port_min", grandfather = TRUE))

  lim <- port$get_security_position_limits("aaa us equity")[["aaa us equity"]]
  expect_equal(lim$min, -900) # stretched to current short
  expect_equal(lim$max, 500)  # max side untouched
})

test_that("replicate_trade_qty holds a grandfathered breach", {
  with_clean_registry(make_equity_provider())
  .security("aaa us equity")

  base <- .portfolio(
    "gf_base", "Grandfather Base",
    nav = 1e6, positions = list(), create = TRUE
  )
  base$add_holding(.holding("aaa us equity", 1600))

  sma <- .sma(
    "gf_sma", "Grandfather SMA",
    nav = 1e6, positions = list(), base_portfolio = "gf_base", create = TRUE
  )
  sma$add_holding(.holding("aaa us equity", 800)) # 8% — grandfathered breach
  sma$add_rule(new_nav_rule("gf_sma", grandfather = TRUE))

  # Base buys 100: unconstrained SMA target = 1700 shares. The stretched cap
  # (current 800) blocks the increase -> hold at 800, trade 0, rule named.
  res <- sma$replicate_trade_qty("aaa us equity", 100)
  expect_equal(res$constrained_target_shares, 800)
  expect_equal(res$trade_shares, 0)
  expect_equal(names(res$limiting_rule), "5pct nav")

  # Base sells 1000: unconstrained target = 600 shares. Reducing the breach
  # is allowed and passes through unclamped (no limiting rule).
  res2 <- sma$replicate_trade_qty("aaa us equity", -1000)
  expect_equal(res2$constrained_target_shares, 600)
  expect_equal(res2$trade_shares, -200)
  expect_true(is.na(res2$limiting_rule))
})

test_that("without grandfather, replicate_trade_qty force-sells a breach", {
  with_clean_registry(make_equity_provider())
  .security("aaa us equity")

  base <- .portfolio(
    "gf_base_ctrl", "Grandfather Base Control",
    nav = 1e6, positions = list(), create = TRUE
  )
  base$add_holding(.holding("aaa us equity", 1600))

  sma <- .sma(
    "gf_sma_ctrl", "Grandfather SMA Control",
    nav = 1e6, positions = list(),
    base_portfolio = "gf_base_ctrl", create = TRUE
  )
  sma$add_holding(.holding("aaa us equity", 800))
  sma$add_rule(new_nav_rule("gf_sma_ctrl", grandfather = FALSE))

  res <- sma$replicate_trade_qty("aaa us equity", 100)
  expect_equal(res$constrained_target_shares, 500) # absolute clamp
  expect_equal(res$trade_shares, -300)             # forced sell-down
})
