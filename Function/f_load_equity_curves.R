f_load_equity_curves <- function() {
  # Naive Strat
  loaded_name <- load(here("output", "Equity_curve_naive_2018-01-01_to_2022-12-31.rda"))
  naive_strat<- get(loaded_name)
  cat("Loaded: naive_stratR\n")
  # Regression Strat
  loaded_name <- load(here("output", "Equity_curve_regression_2018-01-01_to_2022-12-31.rda"))
  regression_strat<- get(loaded_name)
  cat("Loaded: regression_strat\n")
  
  # Combine them together
  all_curves <- merge(naive_strat, regression_strat, by = "Date", all = TRUE)
  names(all_curves) <- c("Date", "Naive", "Regression")
  
  return(all_curves)
  
}