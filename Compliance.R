library(replikit)
library(replikitdata)
library(replikitviz)

compliance_table <- function(portfolios) {
  compliance <- suppressWarnings(sapply(
    portfolios,
    function(p) {
      if ("SMA" %in% class(p)) {
        return(p$check_rule_compliance(verbose = FALSE
        ))
      }
      list(pass = TRUE)
    },
    simplify = FALSE
  ))

  max_name_length <- max(nchar(names(compliance)))

  for (i in seq_along(compliance)) {
    cat(rep("_", 80), sep = "")
    cat("\n")
    name <- names(compliance)[[i]]
    space_padding <- paste(
      rep(" ", max_name_length - nchar(name)), collapse = ""
    )
    cat(name)
    cat(" : ")
    cat(space_padding)
    compliant <- compliance[[i]]$pass
    if (isTRUE(compliant)) {
      cat("All Compliant\n")
    }
    if (isFALSE(compliant)) {
      cat("Not Compliant\n")
      issues <- compliance[[i]]$non_compliant
      for (k in seq_along(issues)) {
        cat(paste0("  *", names(issues)[[k]], "\n"))
      }
    }
  }
}



Rblpapi::blpConnect()

db_connect()
set_security_data_provider(BloombergDataProvider$new())
update_db_data()
portfolios <- load_all_portfolios_from_db()
compliance_table(portfolios)
update_security_data()


ccmf <- .portfolio("ccmf", create = FALSE)
positions <- ccmf$get_position()
sapply(positions, function(p) p$get_security()$get_id())



profvis::profvis({
  library(replikit)
  library(replikitdata)
  db_connect()
  Rblpapi::blpConnect()
  set_security_data_provider(BloombergDataProvider$new())
  #update_db_data()
  portfolios <- load_all_portfolios_from_db()
  update_security_data()
  rebal <- rebalance("ccmf", sma = "all", update_security_data = FALSE)
})


profvis::profvis({
  library(replikit)
  library(replikitdata)
  Rblpapi::blpConnect()
  db_connect()
  set_security_data_provider(BloombergDataProvider$new())
  #portfolios <- load_all_portfolios_from_db()
  test <- replicate_trade_qty("ccmf", "nvda us equity", 1000000, sma = "all")
})

profvis::profvis({
  library(replikit)
  library(replikitdata)
  Rblpapi::blpConnect()
  db_connect()
  update_db_data()
})
