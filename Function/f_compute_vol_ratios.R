f_compute_vol_ratios <- function(unique_pair, list_vols, list_prices) {
  ### Cette fonction crée les ratios de volatilités ainsi que les ratio de prix.
  
  #  Inputs
  #   stock_pairs: [character] (2 x 1) le nom de la paire.
  #   list_vols: [list] (length of N) liste contenant les de données de vol (daily) 
  #   list_prices: [list] (length of N) liste contenant les de données de prix (daily)

  #  OUTPUTS
  #   merged_data: [list] (length of N) liste contenant les ratios pour chaque paires.
  
  # Store each ticker
  ticker_A <- unlist(strsplit(unique_pair, "/"))[1]  # Numerator
  ticker_B <- unlist(strsplit(unique_pair, "/"))[2]  # Denominator
  
  # Store the daily implied vol. of both stock in the pair
  vol_A <- list_vols[[ticker_A]] 
  vol_B <- list_vols[[ticker_B]]
  aligned_vol <- merge.xts(vol_A, vol_B, fill= NA)
  
  # Create ratio 
  vol_ratio <- aligned_vol[, 1] / aligned_vol[, 2]
  colnames(vol_ratio) <- paste("vol_ratio")
  
  # Retrieve the prices from the list_price
  price_A <- list_prices[[ticker_A]]
  price_B <- list_prices[[ticker_B]]
  aligned_price <- merge.xts(price_A, price_B, fill= NA)

  #Create ratio 
  price_ratio <- aligned_price[, 1] / aligned_price[, 2]
  colnames(price_A) <- paste(ticker_A, "_Price", sep="")
  colnames(price_B) <- paste(ticker_B, "_Price", sep="")
  colnames(price_ratio) <- paste("price_ratio")
  
  # Merge ratio with the prices
  merged_data <- merge.xts(vol_ratio, price_A, price_B, price_ratio)

  return(merged_data)
}

f_compute_predict_price_ratios <- function(unique_pairs, df_price_predict) {
  ### Cette fonction crée les ratios de prix prédit dans 10 jours.
  
  #  Inputs
  #   stock_pairs: [character] la liste de combo possible.
  #   df_price_predict: [data.frame] (N x C) contenant les prix prédits.
  
  #  OUTPUTS
  #   predict_ratio: [data.frame] (N x C) dataframe contenant les ratios prédits.
  
  for (i in seq_along(unique_pairs)) {
    tickers <- strsplit(unique_pairs[i], "/")[[1]]  # Split the pair into individual tickers
    ticker_A <- tickers[1]  # Numerator
    ticker_B <- tickers[2]  # Denominator
    ratio_colname <- unique_pairs[i]
    df_price_predict[[ratio_colname]] <- df_price_predict[[ticker_A]] / df_price_predict[[ticker_B]]
  }
  
  return(df_price_predict)
}