f_load_trades <- function() {
  ### Cette fonction retrouve les transactions qui sont sauvegardées en .RDA et les combines ensembles.
  ###  Elle ne contient pas d'input puisqu'elle retrouve les fichier .rda dans le dossier "output".  
  
  #  OUTPUTS
  #   output: [liste] Une liste ou chaque element est tout les trades de chaque stratégie.
  
  # Empty list
  all_trades_list <- list()
  
  # Naive Strat
  loaded_name <- load(here("output", "df_all_trades_naive_2018-01-01_to_2022-12-31.rda"))
  naive_trades <- get(loaded_name)
  cat("Loaded: naive_trades\n")
  all_trades_list$naive_trades <- naive_trades
  
  # Regression Strat
  loaded_name <- load(here("output", "df_all_trades_regression_2018-01-01_to_2022-12-31.rda"))
  regression_trades <- get(loaded_name)
  cat("Loaded: regression_trades\n")
  all_trades_list$regression_trades <- regression_trades
  
  return(all_trades_list)
}