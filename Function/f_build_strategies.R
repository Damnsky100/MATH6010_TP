f_cluster <- function(K) {
  
  ### Cette fonction permet d'identifier différents groupes de stocks constituants
  ### le NASDAQ100 par approche de K-mean clustering selon des données de l'années. 
  ### 2007. L'analyse repose sur des 'fundamentals' des compagnies. Ces données sont 
  ### importées directement du répertoire Clean_Data.
  
  #  INPUTS 
  #   K : Nombre de groupe qu'on veut former par clustering
  
  #  OUTPUTS
  #   portfolio : [list] (K x 1) donnant les K stocks ayant le plus gros market 
  #   cap. dans leurs cluster.
  
  
  # Importer les données
  load(here('CLean_Data','df_NASDAQ100_fundamentals_clean.rda'))
  
  # Isoler seulement les données pour le clustering et scaler les colonnes
  df_fundamentals <- df_NASDAQ100_fundamentals_clean
  
  cluster_data <- as.matrix(subset(df_fundamentals, select = -c(Code,Year_,Freq,`TICKER SYMBOL`,CUSIP,`COMPANY NAME`)))
  cluster_data_scaled <- scale(cluster_data)
  
  # K-Means clustering
  K <- 10
  km.out <- kmeans(cluster_data_scaled, centers = K, nstart = 1000)
  df_fundamentals$CLUSTER <- km.out$cluster
  
  # Format des résultats
  df_result <- df_fundamentals[,c("TICKER SYMBOL", "COMPANY NAME", "MARKET CAPITALIZATION (U.S.$)", "CLUSTER")]
  df_result <- df_result[order(df_result$CLUSTER,-df_result$`MARKET CAPITALIZATION (U.S.$)`),]
  df_result <- df_result[!duplicated(df_result$CLUSTER),]
  df_result <- df_result[order(df_result$`TICKER SYMBOL`), ]
  portfolio <- df_result[, !(names(df_result) %in% c("MARKET CAPITALIZATION (U.S.$)", "CLUSTER"))]
  
  portfolio <- as.vector(portfolio)
  portfolio$'TICKER SYMBOL'
  
  print(portfolio$'TICKER SYMBOL')
}

f_run_price_regressions <- function(stocks_symbols) {
  ### Cette fonction execute le script pour trouver les regressions de chaque stock.
  
  #  Inputs
  #   stock_pairs: [character] la liste de combo possible.
  
  #  OUTPUTS
  #   predict_ratio: [data.frame] (N x C) dataframe contenant les ratios prédits.
  
  tickers <-  stocks_symbols
  
  # To load the data back into R at a later time
  load(here("Clean_Data", "stock_data.rda"))
  
  #Trouver notre univers d'investissement
  
  # Rouler la création de notre objet + manipulations
  result <- init_instance(priceData, tickers)
  
  # Retrieve the best elastic net model for each tickers (Just switch GE for the another ticker)
  result$resultsList$AMAT$model
  
  #Retrieve the coefficients of the models for each tickers (Just switch GE for the another ticker)
  result$resultsList$AMAT$coefficients
  
  #Retrieve the MSE (Out of sample - Testing set) for every model (Just switch GE for the another ticker)
  result$resultsList$AMAT$mse
  
  # Retrieve the price predicion in 10 days for the tickers 
  dates <- result$featuresList[[result$tickers[1]]]$date 
  testRange <- result$get_date_range("2017-12-31", "2023-01-01", 0, dates)
  result$pricePrediction 
  res <- as.data.frame(result$pricePrediction)
  rownames(res) <- dates[dates %in% testRange$start:testRange$end]
  colnames(res)<- tickers
  res
  #Retrieve l'univers d'investissement
  result$tickers 
  
  save(res, file = here("Clean_Data", "pricePrediction.rda"))
}

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
  
  # Rename
  names(list_ratios_technicals) <- unlist(all_pairs)
  
  return(list_ratios_technicals)
}
  
f_add_regression_predictions <- function(list_ratios_technicals) {
  ### Cette fonction joint les predictions de prix au data set avec les ratios techniques
  ### qui sont utilisés par les fonctions de trading.
  
  # Inputs:
  #   list_ratios_technicals: [List] La liste qui contient les ratios technique pour chaque stocks
  
  # Outputs
  #   list_ratios_technicals : [List] La liste de ratios technique avec les prix prédit.
  
  # store the names of the elements
  store_names <- names(list_ratios_technicals)
  
  # Load the predictions
  df <- load(here("Clean_Data", "pricePrediction.rda"))
  df_price_prediction <- get(df[1])
  
  unlist_all_pairs <- names(list_ratios_technicals)
  
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
      return(list_ratios_technicals[[x]]) 
    }

  })
  
  list_ratios_technicals <- lapply(list_ratios_technicals, na.locf)
  
  names(list_ratios_technicals) <- store_names
  
  # Save ratio technicals
  name_file <- paste0("list_ratios_technicals_all.rda")
  save(list_ratios_technicals, file = here('Output', name_file))
  
  return(list_ratios_technicals)
}

f_generate_pairs <- function(stocks_symbols) {
  ### Cette fonction permet de créer une liste ou chaque element 
  ### est une paire de stock.
  
  #  INPUTS 
  #   stocks_symbols : les stocks choisit
  
  #  OUTPUTS
  #   all_pairs : [list] liste contenant toutes les paires comme element.
  
  # Determine all combos and keep them named
  combo_matrix<- combn(stocks_symbols, 2)
  
  # Create empty list to store the pairs
  all_pairs <- list()
  
  # Iterate through the columns of the combinations matrix
  for (i in 1:ncol(combo_matrix)) {
    # Extract the two stocks in the current combination
    stockA <- combo_matrix[1, i]
    stockB <- combo_matrix[2, i]
    
    # Create the combination string and add it to the list
    combo <- paste(stockA, "/", stockB, sep = "")
    all_pairs[[i]] <- combo
  }
  
  cat(length(all_pairs), "Pairs of stocks were created\n")
  return(all_pairs)
}

f_compute_vol_ratios <- function(unique_pair, list_vols, list_prices) {
  ### Cette fonction crée les ratios de volatilités ainsi que les ratio de prix
  ### qui sont requis pour la strategie de trading.  
  
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

f_add_technicals <- function(xts_obj, rolling_window, SD) {
  ### Cette fonction rajouts les indicateurs au ratio de volatilité.
  
  #  INPUTS
  #   xts_obj: [xts_obj] (N x C)  xts_object contenant un ratio de vol
  #   rolling_window: [Scalar] le nombre de jours pour calcule les indicateurs
  #   SD: [scalar] le nombre de standard deviation
  
  #  OUTPUTS
  #   xts_obj: [xts_obj] (N x C) xts_object contenant un ratio de vol et indicateurs techniques
  
  MA_100 <- SMA(xts_obj[, 1], n = rolling_window)     # Moving Average
  SD_100 <- runSD(xts_obj[, 1], n = rolling_window)   # Rolling SD
  up_band <- MA_100 + SD * SD_100                     # SD above
  lo_band <- MA_100 - SD * SD_100                     # SD below
  current_SD <- (xts_obj[, 1]-MA_100[,1])/SD_100[, 1] # number of SD away from mean currently
  
  xts_output <- merge(xts_obj, MA_100, up_band, lo_band, current_SD)
  colnames(xts_output)[(ncol(xts_obj) + 1):(ncol(xts_obj) + 4)] <- c("MA_100", "up_band", "lo_band", "current_SD_level") 
  
  return(xts_output)
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


 