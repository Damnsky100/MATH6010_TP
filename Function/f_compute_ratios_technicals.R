f_compute_ratios_technicals <- function(stocks_symbols, rolling_MA, rolling_SD, df_vol, df_price) {
  ### Cette fonction crée les indicateurs techniques requis pour le trading pour chaque paire.
  ###  Elle joint aussi les prix prédits par le modèle de regression.
  
  #  Inputs
  #   stocks_symbols: [Character] Le ticker du stock.
  #   rolling_MA: [Numeric] Le lookback pour la moyenne mobile.
  #   rolling_SD: [numeric] Le lookback pour l'écart type.

  #  OUTPUTS
  #   list_ratios_technicals: [Liste] Une liste contenant les indicateurs techniques 
  #   et prédictions pour chaque stock.
  

  # df_vol <- f_get_vol_data()              # Volatility
  # df_price <- f_get_price_data()          # Price 


  # Apply the clean_vol_data function to the list of tickers
  list_vols <- lapply(stocks_symbols, 
                    function(ticker) f_clean_vol_data(df_vol, 30, ticker))
  names(list_vols) <- stocks_symbols    
  
  # Apply the clean_price_data to the list of tickers
  list_prices <- lapply(stocks_symbols, 
                      function(ticker) f_clean_price_data(df_price, ticker))
  names(list_prices) <- stocks_symbols 


 
  # Generate all possible pairs 
  all_pairs <- f_generate_pairs(stocks_symbols)
  
  # Create and save every volatility ratio and price ratio
  list_volatility_ratios <- lapply(all_pairs, 
                                 f_compute_vol_ratios, list_vols, list_prices)

  # Rename every list element with the pair name
  names(list_volatility_ratios) <- all_pairs

  # Clean the xts_objects within the list
  # Some stocks did not exits at certain point in time and it creates NAs
  list_ratios <- lapply(list_volatility_ratios, function(x) {
  return(na.omit(x))
  })

  # Apply function to add technicals indicators
  list_ratios_technicals <- lapply(list_ratios, 
                                 function(i) f_add_technicals(i, 100, 2))

  
  # Add the price prediction from the regression
  f_run_price_regressions(stocks_symbols)
  
  # Load the predictions
  df <- load(here("Clean_Data", "pricePrediction.rda"))
  df_price_prediction <- get(df[1])
  unlist_all_pairs <- unlist(all_pairs)
  
  # Compute all the future price ratios
  df_price_ratio_predict <- f_compute_predict_price_ratios(unlist_all_pairs, df_price_prediction)
  
  # Iterate over the list and join the columns from price prediction
  list_ratios_technicals <- lapply(names(list_ratios_technicals), function(x) {
    
    # Check if the column exists in the dataframe
    
    if(x %in% colnames(df_price_ratio_predict)) {
      
      # Extract column from dataframe based on xts object's name
      col_data <- df_price_ratio_predict[, x, drop=FALSE]
      
      # Convert the column to xts and merge
      price_prediction <- xts(col_data[, x], order.by=as.Date(rownames(col_data)))
      merged_xts <- merge(list_ratios_technicals[[x]], price_prediction)
      
      return(merged_xts)
      
    } else {
      return(list_ratios_technicals[[x]])  # Return original xts if no matching column
    }
  })
  
  list_ratios_technicals <- lapply(list_ratios_technicals, na.locf)
  
  return(list_ratios_technicals)
}
