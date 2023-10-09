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