f_clean_price_data <- function(df_price, ticker) {
  
  ### Cette fonction nettoie les données de prix des constituants du Dow Jones choisit.
  
  #  Inputs
  #   df_price: [data.frame] (N x C) de données journalière de prix des 
  #             constituants du Dow Jones choisit.
  #   ticker: [character] Choisir le ticker du stock. 
  
  #  OUTPUTS
  #   xts_price: [xts_object] (N x C) de données journalière de prix des 
  #               constituants choisit.
  
  # Retrieve the appropriate ticker
  df_price <- df_price[df_price$ticker == ticker, ]
  # Drop ticker column
  df_price <- df_price[, !colnames(df_price) %in% "ticker"]
  df_price$date <- as.Date(df_price$date, format = "%Y-%m-%d")
  
  # Save as xts_object
  xts_price <- as.xts(df_price[, !names(df_price) %in% "date"], order.by = df_price$date)
  # Fill the NAs with the previous value
  xts_price <- apply(xts_price, 2, na.locf)
  # Save as xts
  xts_price <- as.xts(xts_price)
  
  return(xts_price)
}
