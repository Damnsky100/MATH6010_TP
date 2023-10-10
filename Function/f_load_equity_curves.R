f_load_equity_curves <- function() {
  ### Cette fonction retrouve les courbes d'équités qui sont sauvegardées en .RDA et les combines ensembles.
  ###  Elle ne contient pas d'input puisqu'elle retrouve les fichier .rda dans le dossier "output".  
  
  #  OUTPUTS
  #   output: [data.frame] (N x C) Data.frame contenant toutes les courbes d'équité.
  
  
  # Naive Strat
  loaded_name <- load(here("output", "Equity_curve_naive_2018-01-01_to_2022-12-31.rda"))
  naive_strat <- get(loaded_name)
  cat("Loaded: naive_strat\n")
  
  # Regression Strat
  loaded_name <- load(here("output", "Equity_curve_regression_2018-01-01_to_2022-12-31.rda"))
  regression_strat <- get(loaded_name)
  cat("Loaded: regression_strat\n")
  
  # Combine them together
  all_curves <- merge(naive_strat, regression_strat, by = "Date", all = TRUE)
  names(all_curves) <- c("Date", "Naive", "Regression")
  
  return(all_curves)
}