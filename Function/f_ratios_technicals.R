f_compute_ratios_technicals <- function(stocks_symbols, rolling_MA, rolling_SD) {
  

  df_vol <- f_get_vol_data()              # Volatility
  df_price <- f_get_price_data()          # Price 


  # Apply the clean_vol_data function to the list of tickers
  list_vols <- lapply(stocks_symbols, 
                    function(ticker) f_clean_vol_data(df_vol, 30, ticker))
  names(list_vols) <- stocks_symbols    # Name the list
  # Apply the clean_price_data to the list of tickers
  list_prices <- lapply(stocks_symbols, 
                      function(ticker) f_clean_price_data(df_price, ticker))
  names(list_prices) <- stocks_symbols # Name the list


  ##### CREATE THE RATIOS ######
  
  all_pairs <- f_generate_pairs(stocks_symbols)
  
  # Create and save every volatility and price ratio
  list_volatility_ratios <- lapply(all_pairs, 
                                 f_compute_vol_ratios, 
                                 list_vols, list_prices)

  # Rename every list element with the pair name
  names(list_volatility_ratios) <- all_pairs

  # Clean the xts_objects within the list
  # Some stocks did not exits at certain point in time and it creates NAs
  list_ratios <- lapply(list_volatility_ratios, function(x) {
  # For each xts object, remove rows with NAs
  return(na.omit(x))
  })

  # Apply function to add technicals 
  list_ratios_technicals <- lapply(list_ratios, 
                                 function(i) f_add_technicals(i, 100, 2))

  
  df <- load(here("Clean_Data", "pricePrediction.rda"))
  df_price_prediction <- get(df[1])
  z <- unlist(all_pairs)
  df_price_ratio_predict <- f_compute_predict_price_ratios(z, df_price_prediction)
  
  
  # Iterate over the list and join the columns
  list_ratios_technicals <- lapply(names(list_ratios_technicals), function(x) {
    # Check if the column exists in the dataframe
    if(x %in% colnames(df_price_ratio_predict)) {
      # Extract column from dataframe based on xts object's name
      col_data <- df_price_ratio_predict[, x, drop=FALSE]
      
      # Convert the column to xts
      price_prediction <- xts(col_data[, x], order.by=as.Date(rownames(col_data)))
      
      # Merge with the xts object
      merged_xts <- merge(list_ratios_technicals[[x]], price_prediction)
      
      return(merged_xts)
    } else {
      return(list_ratios_technicals[[x]])  # Return original xts if no matching column
    }
  })
  
  list_ratios_technicals <- lapply(list_ratios_technicals, na.locf)
  
}