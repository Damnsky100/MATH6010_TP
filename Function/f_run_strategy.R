f_run_strategy <- function(start_date, end_date, list_ratios_technicals, strategy) {
  ### Cette execute la strategie choisit pour une période déterminée et crée ensuite la courbe d'équité.
  
  #  Inputs
  #   start_date: [Date] la date de début de la période..
  #   end_date: [Date] la date de fin de la période.
  #   list_ratios_technicals: [List] Liste qui contient les informations techniques pour chaques paires.
  #   strategy: [Character] La strategie choisit (Naive, Regression, Classification, Both)
  
  #  OUTPUTS
  #   list_trading: [List] Une liste contenant le trading pour chaque paires.
  #   df_all_trades: [Data.frame] Un data.frame avec tout les trades de la strategy (.rda et .xslx)
  
  # Starting Date
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Small function to crop data
  crop_data <- function(xts_obj, start_date, end_date) {
    tmp <- window(xts_obj, start = start_date, end = end_date)
    return(tmp)
  }
  
  # Crop the vol data
  list_ratios_technicals <- lapply(list_ratios_technicals, 
                                   crop_data, 
                                   start_date, end_date)
  
  
  # Naive Strategy
  if (strategy == "naive") {
    # Create the backtest 
    list_trading <- lapply(list_ratios_technicals, 
                           function(i) f_trading_naive(i, 10, (100/45), 0.05))
  } 
  
  # Regression Strategy
  if (strategy == "regression") {
    list_trading <- lapply(list_ratios_technicals, 
                           function(i) f_trading_regression(i, 10, (100/45), 0.05))
  }
  
  
  # Retrieve all the trades
  list_trades <- lapply(list_trading, 
                        function(i) f_extract_trades(i))
  
  # Merge the trades from every pair
  df_all_trades <- do.call(rbind, list_trades)
  colnames(df_all_trades)[11] <- "profitable"
  
  # Save trades
  name_file <- paste0("df_all_trades_", strategy,"_",start_date,"_to_", end_date,".rda")
  save(df_all_trades, file = here('Output', name_file))
  cat(paste("Trades saved under:", name_file, "\n"))
  
  name_file_xlsx <- paste0("df_all_trades_", strategy,"_",start_date,"_to_", end_date,".xlsx")
  write_xlsx(df_all_trades, path = here('Output', name_file_xlsx))
  cat(paste("Trades saved under:", name_file_xlsx, "\n"))
  
  
  ### Create equity curve ###
  
  # Retrieve the equity curve from each pair in the list
  equity_curves <- lapply(list_trading, function(x) x$equity_curve)
  
  # Sum the curves
  total_equity_curve <- Reduce("+", equity_curves)
  
  # Convert xts to dataframe
  df_total_equity_curve <- as.data.frame(total_equity_curve)
  
  # Add the date index as a new column
  df_total_equity_curve$Date <- index(total_equity_curve)
  
  # Save equity curve
  name_file <- paste0("Equity_curve_", strategy,"_",start_date,"_to_", end_date,".rda")
  save(df_total_equity_curve, file = here('Output', name_file))
  cat(paste("Equity_curve saved under:", name_file, "\n"))
  
  name_file_xlsx <- paste0("Equity_curve_", strategy,"_",start_date,"_to_", end_date,".xlsx")
  write_xlsx(df_total_equity_curve, path = here('Output', name_file_xlsx))
  cat(paste("Equity_curve saved under:", name_file_xlsx, "\n"))
  
  return(list_trading)
}