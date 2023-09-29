f_clean_vol_data <- function(df_vol, days_to_expiry, ticker) {
  
  ### Cette fonction nettoie les données de volatilité servant à la création des 
  ### ratios de volatilité sur constituants du Dow Jones choisit.
  
  #  Inputs
  #   df_vol: [data.frame] (N x C) de données journalière de volatilité des 
  #          constituants du Dow Jones choisit.
  #   days :  [scalar] Choisir les nombres de jours avant expiration pour les options.
  #   ticker: [character] Choisir le ticker du stock. 
  
  #  OUTPUTS
  #   output: [xts_object] (N x C) de données journalière de volatilité des 
  #          constituants choisit.
  
  # Retrieve the appropriate ticker
  df_vol <- df_vol[df_vol$ticker == ticker, ]
  # Drop ticker column
  df_vol <- df_vol[, !colnames(df_vol) %in% "ticker"]
  df_vol$date <- as.Date(df_vol$date, format = "%Y-%m-%d")
  
  # Save as xts_object
  xts_vol <- xts(df_vol[, !names(df_vol) %in% "date"], order.by = df_vol$date)  # order by date
  # Absolute values to combine put and call
  xts_vol[, "delta"] <- abs(xts_vol[, "delta"]) 
  # Keep only 30 days implied vol and 0.4 to 0.6 delta
  xts_vol <- xts_vol[xts_vol[, "days"] == days_to_expiry & (xts_vol[, "delta"] %in% c(40, 45, 50, 55, 60)), ]
  # Take the average of the moneyness and save it
  output <- aggregate(xts_vol[,"impl_volatility"], by=index(xts_vol), FUN=mean)
  # Fill the NAs with the previous value
  output <- apply(output, 2, na.locf)
  # save as xts
  output <- as.xts(output)
  
  return(output)
}
