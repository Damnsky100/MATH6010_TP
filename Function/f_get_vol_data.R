library("xts")
library("here")

f_get_vol_data <- function() {
  
  ### Cette fonction retrouve les données servant à la création des 
  ### ratios de volatilité sur constituants du Dow Jones choisit. La fonction ne prend pas
  ### d'entrée car les données sont directement tirées du le répertoire Raw_Data
  ### Ces données sont :
  ###    1) data_vol.csv : Données de volatilité sur les constituants du Dow Jones choisit
  ###       [Source : OptionMetrics. "Ivy DB US - Volatility Surface" WRDS. https://wrds-www.wharton.upenn.edu/pages/get-data/optionmetrics/. 2023.
  
  #  Inputs
  #   days :  [scalar] Choisir les nombres de jours avant expiration pour les options.
  #   ticker: [character] Choisir le ticker du stock 
  
  #  OUTPUTS
  #   df_vol: [data.frame] (N x C) de données journalière de volatilité des 
  #          constituants du Dow Jones choisit.
  
  # Import data from csv
  df_vol   <- read.csv(here("Raw_Data", "data_vol.csv"))        # volatility

  # Retain needed columns
  df_vol <- df_vol[, c("ticker", "date", "days", "delta", "impl_volatility")]
  
  return(df_vol)
}

