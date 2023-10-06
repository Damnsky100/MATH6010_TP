f_clean_data_clustering <- function() {
  
  ### Cette fonction nettoie les données servant de base au clustering 
  ### effectuer sur les constituants NASDAQ 1000. La fonction ne prend pas
  ### d'entrée car les données sont directement tirées du le répertoire Raw_Data.
  ### Ces données sont :
  ###    1) NASDAQ_consituents_fundamentals.xlsx : Données fondamentales sur les 
  ###       constituants du NASDAQ 100 [Source : Refinitiv. "Worldscope - Fundamentals Annual" WRDS. https://wrds-www.wharton.upenn.edu/pages/get-data/thomson-reuters/worldscope/fundamentals-annual/ 2023.
  
  ### La fonction n'a pas non plus de sortie directe. Elle enregistre les données
  ### nettoyées dans un fichier texte dans le répertoire CLean_Data.
  
  
  # Importer les données
  df_fundamentals <- read.xlsx(here("Raw_Data", "NASDAQ100_constituents_fundamentals.xlsx"))
  
  # Garder seulement les données de 2007
  df_fundamentals <- df_fundamentals[df_fundamentals$Year_ == 2007, ]
  
  # Enlever les doublons (compagnie que les données sont là 2 fois)
  df_fundamentals <- df_fundamentals[rowSums(is.na(df_fundamentals)) < (ncol(df_fundamentals) / 2),]
  
  # Associer les NA aux moyennes des colonnes
  numeric_columns <- sapply(df_fundamentals, is.numeric)
  df_fundamentals[numeric_columns] <- lapply(df_fundamentals[numeric_columns], function(x) {x[is.na(x)] <- mean(x, na.rm = TRUE); return(x)})
  df_NASDAQ100_fundamentals_clean <- df_fundamentals
  
  save(df_NASDAQ100_fundamentals_clean, file = here('Clean_Data', "df_NASDAQ100_fundamentals_clean.rda"))
  
}

f_clean_NASDAQ100_constituents <- function() {
  
  ### Cette fonction nettoie les données donnant les informations sur les
  ### constituants du NASDAQ 100 en 2007. La fonction ne prend pas d'entrée car 
  ### les données sont directement tirées du le répertoire Raw_Data.
  ### Ces données sont :
  ###    1) NASDAQ100_constituents.csv : Données de constituants du NASDAQ 100 
  ###       en 2007. [Source : S&P Global. "Compustat Daily Updates - Index Constituents" WRDS. https://wrds-www.wharton.upenn.edu/pages/get-data/compustat-capital-iq-standard-poors/compustat/north-america-daily/index-constituents/. 2023.]
  
  ### La fonction n'a pas non plus de sortie directe. Elle écrit les code CUSIP
  ### de chacun des constiuants directement dans un fichier texte dans le 
  ### répertoire CLean_Data.
  
  # Load data
  NASDAQ100_constituents <- read.csv(here("Raw_Data", "NASDAQ100_constituents.csv"))
  
  # Clean data
  NASDAQ100_constituents$thru[NASDAQ100_constituents$thru == ''] <- format(as.Date(Sys.Date()), format = "%Y-%m-%d")
  NASDAQ100_constituents <- subset(NASDAQ100_constituents, thru > 2009, select = -c(gvkey,gvkeyx,tic))
  
  # Save data as .txt
  write.table(NASDAQ100_constituents$co_cusip, file = here('Clean_Data', 'NASDAQ100_constituents_2007.txt'), quote = FALSE, row.names = FALSE, col.names = FALSE)
}


f_get_vol_data <- function() {
  
  ### Cette fonction retrouve les données de volatilités des constituants du Dow Jones choisit.
  ### La fonction ne prend pas d'entrée car les données sont directement tirées du répertoire Raw_Data.
  ### Ces données sont :
  ###    1) data_vol.rda : Données de volatilité sur les constituants du Dow Jones choisit
  ###       [Source : OptionMetrics. "Ivy DB US - Volatility Surface" WRDS. https://wrds-www.wharton.upenn.edu/pages/get-data/optionmetrics/. 2023.
  
  #  OUTPUTS
  #   df_vol: [data.frame] (N x C) de données journalière de volatilité des 
  #          constituants du Dow Jones choisit.
  
  # Import data from rda
  data <- read.csv(here("Raw_Data", "data_volatility.csv"))
  
  # Retain specific columns
  data_volatility_clean <- data[, c("ticker", "date", "days", "delta", "impl_volatility")]
  
  save(data_volatility_clean, file = here('Clean_Data', "data_volatility_clean.rda"))
  return(data_volatility_clean)
}


f_get_price_data <- function() {
  
  ### Cette fonction retrouve les données de volatilités des constituants du Dow Jones choisit.
  ### La fonction ne prend pas d'entrée car les données sont directement tirées du répertoire Raw_Data.
  ### Ces données sont :
  ###    1) data_price.rda : Données de prix des constituants du Dow Jones choisit
  ###       [Source : OptionMetrics. "Securities - Secutirty Prices WRDS". https://wrds-www.wharton.upenn.edu/pages/get-data/optionmetrics/ivy-db-us/securities/security-prices/. 2023.
  
  #  OUTPUTS
  #   xts_price: [xts_object] (N x C) de prix journalier des constituants choisit
  
  # Import data from rda
  price_data <- load(here("Raw_Data", "stock_data.rda"))
  df_price <- get(Price_data[1])
  df_price <- df_price[, c("date", "ticker", "close")]
  
  return(df_price)
} 


f_clean_vol_data <- function(df_vol, days_to_expiry, ticker) {
  
  ### Cette fonction nettoie les données de volatilité servant à la création des 
  ### ratios de volatilité pour un stock choisit.
  
  #  Inputs
  #   df_vol: [data.frame] (N x C) de données journalière de volatilité pour les constituants
  #   days_to_expiry : [scalar] Choisir les nombres de jours avant expiration pour les options.
  #   ticker: [character] Choisir le ticker du constituant. 
  
  #  OUTPUTS
  #   xts_vol: [xts_object] (N x C) de données journalière de volatilité pour le stock choisit.
  
  # Retrieve information for the corresponding ticker
  df_vol <- df_vol[df_vol$ticker == ticker, ]
  df_vol <- df_vol[, !colnames(df_vol) %in% "ticker"]       # Drop ticker column
  df_vol$date <- as.Date(df_vol$date, format = "%Y-%m-%d")  # Set proper date format
  
  # Save as xts_object
  xts_vol <- xts(df_vol[, !names(df_vol) %in% "date"], 
                 order.by = df_vol$date)                    # Order by date
  # Absolute values of deltas to combine put and call
  xts_vol[, "delta"] <- abs(xts_vol[, "delta"]) 
  # Keep only 30 days implied vol and 0.4 to 0.6 delta
  xts_vol <- xts_vol[xts_vol[, "days"] == days_to_expiry & (xts_vol[, "delta"] %in% c(40, 45, 50, 55, 60)), ]
  # Take the average of the moneyness and save it
  xts_vol <- aggregate(xts_vol[,"impl_volatility"], 
                      by=index(xts_vol), FUN=mean)
  xts_vol <- apply(xts_vol, 2, na.locf)                       # Fill the NAs with the previous value
  xts_vol <- as.xts(xts_vol)                                  # Save as xts
  index(xts_vol) <- as.Date(index(xts_vol))                   # Set index as date
  return(xts_vol)
}


f_clean_price_data <- function(df_price, ticker) {
  
  ### Cette fonction nettoie les données de prix des constituants du Dow Jones choisit.
  
  #  Inputs
  #   df_price: [data.frame] (N x C) de données journalière de prix des 
  #             constituants du Dow Jones choisit.
  #   ticker: [character] Choisir le ticker du constituant.  
  
  #  OUTPUTS
  #   xts_price: [xts_object] (N x C) de données journalière de prix des 
  #               constituants choisit.
  
  # Remove the rownames
  rownames(df_price) <- NULL
  # Retrieve the appropriate ticker
  df_price <- df_price[df_price$ticker == ticker, ] # Retrieve only the current ticker
  df_price$date <- as.Date(df_price$date, 
                           format = "%Y-%m-%d")     # Set proper date format
  # Save as xts_object with closing price as the column and date as index
  xts_price <- xts(x = df_price$close, 
                   order.by = df_price$date)
  index(xts_price) <- as.Date(index(xts_price))     # Set index as date
  return(xts_price)                                             
}
