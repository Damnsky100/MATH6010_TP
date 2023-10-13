# Define a FinancialAnalysis class
ElasticNetPrediction <- R6Class("ElasticNetPrediction",
                                public = list(
                                  df_price = NULL,
                                  tickers = NULL,
                                  window_size = 10,
                                  featuresList = list(),
                                  resultsList = list(),
                                  predictionMatrix = NULL,
                                  pricePrediction = NULL,
                                  closePrice = NULL,
                                  
                                  initialize = function(df_price_path) {
                                    self$load_data(df_price_path)
                                  },
                                  
                                  get_date_range = function(start_date, end_date, offset = 0, dates) {
                                    ### Cette fonction récupère une plage de dates entre une date de début et une date de fin, avec un décalage optionnel.
                                    
                                    #  Inputs
                                    #   start_date: [Date] la date de début pour la plage de dates.
                                    #   end_date: [Date] la date de fin pour la plage de dates.
                                    #   offset: [numeric] (par défaut = 0) un décalage à appliquer à la date de fin.
                                    #   dates: [Date vector] un vecteur de dates parmi lesquelles la fonction recherche les indices de début et de fin.
                                    
                                    #  OUTPUTS
                                    #   Une liste contenant les dates de début et de fin correspondant à la plage de dates souhaitée.
                                    
                                    idxStart <- dates[which(dates > start_date)][1]
                                    idxEnd <- dates[which(dates < end_date)]
                                    idxEnd <- idxEnd[length(idxEnd) - offset]
                                    return(list(start = idxStart, end = idxEnd))
                                  },
                                  
                                  load_data = function(dataPrice) {
                                    ### Cette fonction charge les données de prix, effectue des transformations et stocke les données dans un objet xts.
                                    
                                    #  Inputs
                                    #   dataPrice: [data.frame] un dataframe contenant les données de prix avec une colonne 'date'.
                                    
                                    #  OUTPUTS
                                    #   Aucun retour direct. Cependant, la fonction met à jour l'objet `self$df_price` avec les données transformées.
                                    
                                    # Read the CSV file
                                    self$df_price <- dataPrice
                                    
                                    # Convert the 'date' column to Date type
                                    self$df_price$date <- as.Date(self$df_price$date, format="%Y-%m-%d")
                                    
                                    # Convert the data frame to an xts object
                                    #self$df_price <- xts(self$df_price[, -which(names(self$df_price) == "date")], order.by=self$df_price$date)
                                    self$df_price <- xts(self$df_price, order.by=self$df_price$date)
                                    
                                    # Handle NA values
                                    self$df_price[is.na(self$df_price)] <- 0
                                  },
                                  
                                  rolling_window = function(data, window_size) {
                                    ### Cette fonction génère une matrice de fenêtres roulantes à partir des données fournies.
                                    
                                    #  Inputs
                                    #   data: [vector] un vecteur contenant les données pour lesquelles les fenêtres roulantes doivent être générées.
                                    #   window_size: [integer] la taille de chaque fenêtre roulante.
                                    
                                    #  OUTPUTS
                                    #   result: [matrix] une matrice où chaque colonne représente une fenêtre roulante des données.
                                    
                                    # Number of windows
                                    n_windows <- length(data) - window_size + 1
                                    
                                    # Initialize matrix to store rolling windows
                                    result <- matrix(NA, nrow = window_size, ncol = n_windows)
                                    
                                    
                                    # Populate matrices with rolling windows and dates
                                    for (i in 1:n_windows) {
                                      result[, i] <- data[i:(i + window_size - 1)]
                                    }
                                    result
                                  },
                                  
                                  f_cumulativeReturnRolling = function(data, window_size){
                                    ### Cette fonction calcule les rendements cumulés sur une fenêtre roulante pour les données fournies.
                                    
                                    #  Inputs
                                    #   data: [data.frame] un dataframe contenant une colonne 'return' qui représente les rendements.
                                    #   window_size: [integer] la taille de chaque fenêtre roulante.
                                    
                                    #  OUTPUTS
                                    #   df: [data.frame] un dataframe contenant les rendements cumulés pour chaque fenêtre roulante. 
                                    #       Les noms des lignes du dataframe correspondent aux dates de chaque fenêtre.
                                    
                                    
                                    # Calculer le nombre de fenêtres roulantes possibles
                                    n_windows <- nrow(data) - window_size + 1
                                    
                                    # Utiliser la fonction rolling_window pour obtenir les fenêtres roulantes des rendements
                                    tmp <- self$rolling_window(as.numeric(data$return), window_size)
                                    
                                    # Calculer les rendements cumulés pour chaque fenêtre roulante
                                    cumulative_returns <- apply(tmp, 2, sum)
                                    
                                    # Extraire les dates des données
                                    dates <- index(data)
                                    
                                    # Créer un dataframe pour stocker les rendements cumulés
                                    df <- data.frame(cumulative_returns)
                                    
                                    # Attribuer les dates appropriées comme noms de lignes du dataframe
                                    rownames(df) <- dates[1:n_windows]
                                    
                                    return(df)
                                  },
                                  
                                  
                                  f_constructFeatures = function(ticker, df_price, window_size){
                                    ### Cette fonction construit les caractéristiques techniques pour un ticker donné.
                                    
                                    #  Inputs
                                    #   ticker: [character] le nom du ticker pour lequel les caractéristiques doivent être construites.
                                    #   df_price: [data.frame] un dataframe contenant les prix et autres informations financières.
                                    #   window_size: [integer] la taille de la fenêtre pour le calcul des rendements cumulés.
                                    
                                    #  OUTPUTS
                                    #   df_ticker: [data.frame] un dataframe contenant les caractéristiques techniques pour le ticker donné.
                                    
                                    # A few technical indicators functions from TTR, see class notes 
                                    # for their definition
                                    f_ATR <- function(x)
                                      ATR(HLC(x))[, 'atr']
                                    f_ADX <- function(x)
                                      ADX(HLC(x))[, 'ADX']
                                    f_Aroon <- function(x)
                                      aroon(cbind(Hi(x), Lo(x)), n = 2)$oscillator
                                    f_BB <- function(x)
                                      BBands(HLC(x))[, "pctB"]
                                    f_ChaikinVol <- function(x)
                                      Delt(chaikinVolatility(cbind(Hi(x), Lo(x))))[, 1]
                                    f_CLV <- function(x)
                                      EMA(CLV(HLC(x)))[, 1]
                                    f_MACD <- function(x)
                                      MACD(Cl(x))[, 2]
                                    f_MFI <- function(x)
                                      MFI(HLC(x), Vo(x))
                                    f_SAR <- function(x)
                                      SAR(cbind(Hi(x), Cl(x))) [, 1]
                                    f_SMI <- function(x)
                                      SMI(HLC(x))[, "SMI"]
                                    f_Volat <- function(x)
                                      volatility(OHLC(x), calc = "garman")[, 1]
                                    
                                    
                                    # Create a base name from ticker by removing any character that is not
                                    # a number or letter and then converting the results to lower case letters
                                    base_name <- tolower(gsub("[[:punct:]]", "", ticker))
                                    
                                    #Select the right tickers
                                    data <- self$df_price[self$df_price$ticker %in% ticker, ]
                                    
                                    
                                    tmp <- subset(data, select = -c(ticker))
                                    dates <- as.Date(index(tmp))
                                    numeric_tmp <-as.data.frame(lapply(subset(tmp, select=-date), as.numeric))
                                    data_xts <- xts::as.xts(numeric_tmp, order.by=dates)
                                    
                                    
                                    
                                    ## Retrouver 10 jours forecast returns
                                    retLead <- self$f_cumulativeReturnRolling(data, window_size)
                                    retLead = as.xts(retLead)
                                    index(retLead) = as.Date(index(retLead))
                                    ret_closeLead <- stats::lag(retLead, k=-1)
                                    
                                    data_xtsInput <- data_xts[index(data_xts) %in% index(retLead), ]
                                    
                                    ret = as.xts(data_xtsInput$ret)
                                    
                                    # Create data.frame with output variable and predictors
                                    df_ticker <- data.frame(
                                      date = index(data_xtsInput),
                                      retLead = ret_closeLead,
                                      coredata(stats::lag(ret, k = 0:3)),
                                      atr = coredata(f_ATR(data_xtsInput)),
                                      adx = coredata(f_ADX(data_xtsInput)),
                                      aroon = coredata(f_Aroon(data_xtsInput)),
                                      bb = coredata(f_BB(data_xtsInput)),
                                      chaikin_vol = coredata(f_ChaikinVol(data_xtsInput)),
                                      clv = coredata(f_CLV(data_xtsInput)),
                                      macd = coredata(f_MACD(data_xtsInput)),
                                      mfi = coredata(f_MFI(data_xtsInput)),
                                      sar = coredata(f_SAR(data_xtsInput)),
                                      smi = coredata(f_SMI(data_xtsInput)),
                                      volat = coredata(f_Volat(data_xtsInput))
                                    )
                                    
                                    # Rename the various columns using the base_name and the technical indicators names
                                    base_name <- tolower(gsub("[[:punct:]]", "", ticker))
                                    col_names <- c("date", "Y", 
                                                   paste0(base_name,"_adjclose"),
                                                   paste0(base_name,"_adjclose_lag1"),
                                                   paste0(base_name,"_adjclose_lag2"),
                                                   paste0(base_name,"_adjclose_lag3"),
                                                   "atr", "adx", "aaron", "bb", "chaikin_vol", "clv", 
                                                   "macd", "mfi", "sar", "smi", "volat")
                                    names(df_ticker) <- col_names
                                    
                                    df_ticker
                                  },
                                  
                                  f_elasticNet = function(trainingset, testingset) {
                                    
                                    ### Cette fonction entraîne un modèle de régression Elastic Net sur un ensemble d'entraînement et teste sa performance sur un ensemble de test.
                                    
                                    #  Inputs
                                    #   trainingset: [data.frame] un dataframe contenant les données d'entraînement.
                                    #   testingset: [data.frame] un dataframe contenant les données de test.
                                    
                                    #  OUTPUTS
                                    #   Une liste contenant:
                                    #   - model: [train object] le modèle Elastic Net entraîné.
                                    #   - predictions: [numeric vector] les prédictions du modèle sur l'ensemble de test.
                                    #   - coefficients: [data.frame] un dataframe contenant les coefficients non nuls du modèle.
                                    
                                    
                                    # Formula
                                    fmla <- Y ~ . - date
                                    
                                    # Grid for elastic net
                                    grid_enet <- expand.grid(alpha = seq(from = 0,
                                                                         to = 1, 
                                                                         by = 0.1),
                                                             lambda = seq(from = 0,
                                                                          to = 0.05,
                                                                          length.out = 1001))
                                    
                                    # Timeslice train control
                                    ctr_train <- trainControl(method = "timeslice",
                                                              initialWindow = 800,
                                                              horizon = 80,
                                                              skip = 10,
                                                              fixedWindow = TRUE,
                                                              allowParallel = TRUE)
                                    
                                    # Convert to data frame
                                    data <- data.frame(trainingset)
                                    
                                    # Train the model
                                    model_enet_best <- train(form = fmla,
                                                             data = data,
                                                             method = "glmnet",
                                                             tunegrid = grid_enet,
                                                             trControl = ctr_train, 
                                                             preProc = c("center", "scale"),
                                                             metric = "Rsquared")
                                    
                                    
                                    # Predict on the testing set
                                    predicTest <- predict(model_enet_best, testingset)
                                    
                                    # Extract coefficients from the best model
                                    coefs <- coef(model_enet_best$finalModel, s = model_enet_best$bestTune$lambda)
                                    
                                    # Convert sparse matrix to regular matrix
                                    matrix_coefs <- as.matrix(coefs)
                                    
                                    # Extract non-zero coefficients
                                    non_zero_coefs <- matrix_coefs[matrix_coefs != 0, , drop=FALSE]
                                    
                                    # Convert to data frame
                                    df_coefs <- as.data.frame(non_zero_coefs)
                                    
                                    return(list(model = model_enet_best, predictions = predicTest, coefficients = df_coefs))
                                  },
                                  
                                  get_price_prediction = function(testRange) {
                                    ### Cette fonction récupère les prédictions de prix pour une plage de dates donnée.
                                    
                                    #  Inputs
                                    #   testRange: [list] une liste contenant les dates de début et de fin pour lesquelles les prédictions de prix sont nécessaires.
                                    
                                    #  OUTPUTS
                                    #   self$pricePrediction: [matrix] une matrice contenant les prédictions de prix pour chaque ticker dans la plage de dates spécifiée.
                                    
                                    # Retrieve the prices
                                    df <- self$df_price[index(self$df_price) %in% testRange$start:testRange$end, c("ticker", "close", "date")]
                                    df <- df[df$ticker %in% self$tickers]
                                    df$close <- as.numeric(df$close)
                                    
                                    # Create an xts object for each ticker using the date information from df
                                    list_xts <- lapply(unique(df$ticker), function(ticker) {
                                      xts(df$close[df$ticker == ticker], order.by = as.Date(coredata(df$date[df$ticker == ticker])))
                                    })
                                    
                                    # Combine the cleaned xts objects
                                    combined_xts <- do.call(cbind, list_xts)
                                    
                                    # Convert the combined xts object to a matrix
                                    closePrice <- as.matrix(combined_xts)
                                    # Convert all columns of the matrix to numeric
                                    self$closePrice <- apply(closePrice, 2, as.numeric)
                                    
                                    tmp <- exp(self$predictionMatrix) * self$closePrice
                                    rownames(tmp) <- index(combined_xts)
                                    self$pricePrediction <- tmp
                                    self$pricePrediction
                                  }
                                  
                                )
)

init_instance = function(dataPrice, tickers) {
  ### Cette fonction initialise une instance de la classe ElasticNetPrediction et génère des prédictions de prix pour une liste de tickers.
  
  #  Inputs
  #   dataPrice: [data.frame] un dataframe contenant les données de prix pour chaque ticker.
  #   tickers: [character vector] une liste des tickers pour lesquels les prédictions doivent être générées.
  
  #  OUTPUTS
  #   elasticNet_pred: [ElasticNetPrediction object] une instance de la classe ElasticNetPrediction contenant les prédictions de prix, les modèles, et d'autres informations pertinentes pour chaque ticker.
  
  
  # Initialiser notre instance :  ElasticNetPrediction class
  elasticNet_pred <- ElasticNetPrediction$new(dataPrice)
  elasticNet_pred$tickers <- tickers
  # Generate Momentum features for each ticker
  featuresList <- vector("list", length(elasticNet_pred$tickers))
  names(featuresList) <- elasticNet_pred$tickers
  
  for (i in seq_along(elasticNet_pred$tickers)){
    ticker <- elasticNet_pred$tickers[i]
    tmp <- elasticNet_pred$f_constructFeatures(ticker, elasticNet_pred$df_price, elasticNet_pred$window_size)
    featuresList[[i]] = tmp
  }
  print("Features Building: Done")
  elasticNet_pred$featuresList <- featuresList
  # Get the training and testing date ranges for Lasso regression
  dates <- featuresList[[elasticNet_pred$tickers[1]]]$date 
  trainRange <- elasticNet_pred$get_date_range("2007-12-31", "2018-01-01", elasticNet_pred$window_size, dates)
  testRange <- elasticNet_pred$get_date_range("2017-12-31", "2023-01-01", 0, dates)
  
  n <- sum(dates %in% testRange$start:testRange$end)
  predictionMatrix <- matrix(NA, nrow=n, ncol=length(elasticNet_pred$tickers))
  colnames(predictionMatrix) <- elasticNet_pred$tickers
  
  resultsList <- vector("list", length(elasticNet_pred$tickers))
  names(resultsList) <- elasticNet_pred$tickers
  
  for (tick in seq_along(elasticNet_pred$tickers)) {
    df <- featuresList[[tick]]
    
    # Subset the data based on the defined time ranges
    trainSet <- df[df$date %in% trainRange$start:trainRange$end, ]
    testSet <- df[df$date %in% testRange$start:testRange$end, ]
    
    results <- elasticNet_pred$f_elasticNet(trainSet, testSet)
    
    # Store results in the main list
    resultsList[[tick]] <- list(
      predictions = as.xts(results$predictions),
      model = results$model,
      mse = mse(actual = testSet$Y, predicted = results$predictions),
      coefficients = results$coefficients
    )
    predictionMatrix[, tick] <- results$predictions
    
  }
  
  elasticNet_pred$resultsList <- resultsList
  print("Regression Building: Done")
  elasticNet_pred$predictionMatrix <- predictionMatrix
  
  elasticNet_pred$pricePrediction <- elasticNet_pred$get_price_prediction(testRange)
  
  return(elasticNet_pred)
}
