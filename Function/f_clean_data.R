f_clean_data_clustering <- function() {
  
  ### Cette fonction nettoie les données servant de base au clustering 
  ### effectuer sur les constituants du Dow Jones. La fonction ne prend pas
  ### d'entrée car les données sont directement tirées du le répertoire Raw_Data.
  ### Ces données sont :
  ###    1) dow_constituants.csv : Données sur les constituants du Dow Jones 
  ###       depuis sa création. 
  ###       [Source : S&P Global. "Compustat Daily Updates - Index Constituents" WRDS. https://wrds-www.wharton.upenn.edu/pages/get-data/compustat-capital-iq-standard-poors/compustat/north-america-daily/index-constituents/. 2023.
  ###    2) dow_constituants_fundamentals.csv : Données fondamentales annuelles 
  ###       des constituants du Dow Jones de 2007 à 2022 (C colonnes)
  ###       [Source : Refinitiv. "Worldscope - Fundamentals Annual" WRDS. https://wrds-www.wharton.upenn.edu/pages/get-data/thomson-reuters/worldscope/fundamentals-annual/. 2023.
  ###   3) fundamentals_mapping.txt : Fichier texte permettant de renommr de 
  ###     façon approprié les colonenes dans dow_constituants_fundamentals.csv
  
  
  #  OUTPUTS
  #   df   : [data.frame] (N x C) de données annuelles 'fundamentals' des 
  #          constituants du Dow Jones ainsi que les dates d'entrée et de 
  #          sortie de l'index. (C : nombre de fundamentals choisie + dates)
  
  
  # Importer les données nécéssaires au clustering
  DOW_const    <- read.csv(here("Raw_Data", "dow_constituants.csv"))                   # Constituants Dow Jones
  df_fund      <- read.csv(here("Raw_Data", "dow_constituants_fundamentals.csv"))      # Données sur les constituants
  fund_mapping <- read.csv(here("Raw_Data", "fundamentals_mapping.txt"), header=FALSE) # Mapping des colonnes
  
  
  # Nettoyer les données du Dow Jones
  DOW_const$thru[DOW_const$thru == ''] <- format(Sys.Date(),  "%Y-%m-%d") 
  
  colnames(DOW_const)[colnames(DOW_const) == "co_tic"]  <- "TICKER"
  colnames(DOW_const)[colnames(DOW_const) == "co_conm"] <- "NAME"
  
  DOW_const <- DOW_const[,c("TICKER","NAME","from","thru")]
  DOW_const <- DOW_const[order(DOW_const$TICKER),]
  
  # Nettoyer les données sur les données 'fundamentals' des constituants
  fund_mapping_split  <- data.frame(do.call(rbind, strsplit(fund_mapping$V1, ":", fixed = TRUE)))
  colnames(df_fund)   <- fund_mapping_split$X2
  
  colnames(df_fund)[colnames(df_fund) == "Company Name"] <- "NAME"
  
  df_fund <- subset(df_fund, select = -c(code,freq,CUSIP,NAME))
  
  # Merge les infos des constituants avec les données 'fundamentals'
  df <- merge(df_fund, DOW_const, by = "TICKER")
  
  # Enlever les lignes qui contiennes plus de NA que d'informations
  df <- df[rowSums(is.na(df)) < (ncol(df) / 2),]
  df
  
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
  # data <- read.csv(here("Raw_Data", "data_vol_small.csv"))
  data <- load(here("Raw_Data", "data_vol_big.rda"))
  df_vol <- get(data)
  # Retain specific columns
  df_vol <- df_vol[, c("ticker", "date", "days", "delta", "impl_volatility")]
  
  return(df_vol)
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
  data <- load(here("Raw_Data", "data_price_big.rda"))
  df_price <- get(data)
  # Retain specific columns
  df_price <- df_price[, c("ticker", "date", "close")]
  
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
  
  # Retrieve the appropriate ticker
  df_price <- df_price[df_price$ticker == ticker, ]
  df_price <- df_price[, !colnames(df_price) %in% "ticker"]     # Drop ticker column
  df_price$date <- as.Date(df_price$date, 
                           format = "%Y-%m-%d")                 # Set proper date format
  # Save as xts_object
  xts_price <- as.xts(df_price[, !names(df_price) %in% "date"], 
                      order.by = df_price$date)                 # Order by date
  xts_price <- apply(xts_price, 2, na.locf)                     # Fill the NAs with the previous value
  xts_price <- as.xts(xts_price)
  
  return(xts_price)                                             
}
