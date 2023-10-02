library("xts")
library("here")

f_get_price_data <- function() {
  
  ### Cette fonction nettoie les données de prix des constituants du Dow Jones choisit. 
  ### La fonction ne prend pas d'entrée car les données sont directement tirées du répertoire Raw_Data.
  ### Ces données sont :
  ###    1) data_price.csv : Données de prix des constituants du Dow Jones choisit
  ###       [Source : OptionMetrics. "Securities - Secutirty Prices WRDS". https://wrds-www.wharton.upenn.edu/pages/get-data/optionmetrics/ivy-db-us/securities/security-prices/. 2023.
  
  #  OUTPUTS
  #   xts_price: [xts_object] (N x C) de prix journalier des constituants choisit

  # Import data from csv
  df_price <- read.csv(here("Raw_Data", "data_price.csv"))      
  df_price <- df_price[, c("ticker", "date", "close")]
  
  return(df_price)
} 
  
  
