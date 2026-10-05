library(replikit)
library(replikitdata)

db_connect()
Rblpapi::blpConnect()
set_security_data_provider(BloombergDataProvider$new())
update_db_data()

t1 <- Sys.time()
portfolios <- load_all_portfolios_from_db()
t2 <- Sys.time()
print(t2 - t1)

lighthouse <- .portfolio("lighthouse")

qube <- .portfolio("qube")
qube$check_rule_compliance(verbose = TRUE)


qube_rules <- qube$get_rules()
qube_rule_names <- vapply(qube_rules, \(r) r$get_name(), character(1))
qube_adv_rule <- qube_rules[[4]]
qube_adv_rule$get_share_caps("www us equity")
qube_adv_rule$capacity("www us equity")


lighthouse_rebalance <- lighthouse$rebalance()


fmap <- .portfolio("fmap")
test <- fmap$rebalance()


portfolios <- sapply(
  ls(registries$portfolios),
  function(p) .portfolio(p)
)

compliance <- sapply(
  portfolios,
  function(p) {
    if ("SMA" %in% class(p)) {
      return(p$check_rule_compliance(update_bbfields = FALSE, verbose = FALSE))
    }
    list(pass = TRUE)
  },
  simplify = FALSE
)


compliance_table <- function(portfolios) {
  compliance <- suppressWarnings(sapply(
    portfolios,
    function(p) {
      if ("SMA" %in% class(p)) {
        return(p$check_rule_compliance(update_bbfields = FALSE, verbose = FALSE))
      }
      list(pass = TRUE)
    },
    simplify = FALSE
  ))

  max_name_length <- max(nchar(names(compliance)))

  for (i in 1:length(compliance)) {
    cat(rep("_", 80), sep = ""); cat("\n")
    name <- names(compliance)[[i]]
    space_padding <- paste(
      rep(" ", max_name_length - nchar(name)), collapse = ""
    )
    cat(name); cat(" : "); cat(space_padding)
    compliant <- compliance[[i]]$pass
    if (isTRUE(compliant)) {
      cat("All Compliant\n")
    }
    if (isFALSE(compliant)) {
      cat("Not Compliant\n")
      issues <- compliance[[i]]$non_compliant
      for (k in 1:length(issues)) {
        cat(paste0("  *", names(issues)[[k]], "\n"))
      }
    }
  }
}


compliance_table(portfolios)

owl <- sapply(
  portfolios,
  \(p) p$get_position("owl us equity")$get_qty()
)

nav <- sapply(
  portfolios,
  \(p) p$get_nav()
)
ccmf_owl_opt <- 1662200 * 12.25 / nav[1]

trade <- nav * ccmf_owl_opt / 1225


test <- function(base_name, security_id, amount, verbose = FALSE) {
  smas <- get_tracking_portfolios(base_portfolio = base_name)
  update_bloomberg_fields()
  trades <- sapply(
    smas,
    \(sma) sma$replicate_trade(security_id, amount, FALSE),
    simplify = FALSE,
    USE.NAMES = TRUE)
  final_shares <- c(
    setNames(amount, base_name),
    vapply(trades, \(t) t[["trade_shares"]], numeric(1))
  )
  if (isFALSE(verbose)) return(final_shares)
  list(final_shares = final_shares, detail = trades)
}
