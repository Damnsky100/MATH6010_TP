
library(lubridate)
library(PerformanceAnalytics)
library(here)
library(xts)
library(zoo)
library(TTR)
library(stats)
library(caret)
library(glmnet)
library(Metrics)
library(quantmod)
library(R6)

source(here("Function", "f_clean_data.R"))
source(here("Function", "f_cluster.R"))




# Define a FinancialAnalysis class
LassoPrediction <- R6Class("LassoPrediction",
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
            idxStart <- dates[which(dates > start_date)][1]
            idxEnd <- dates[which(dates < end_date)]
            idxEnd <- idxEnd[length(idxEnd) - offset]
            return(list(start = idxStart, end = idxEnd))
        },
        
        load_data = function(dataPrice) {
            # Read the CSV file
            self$df_price <- dataPrice
            
            # Convert the 'date' column to Date type
            self$df_price$date <- as.Date(self$df_price$date, format="%Y-%m-%d")
            
            # Convert the data frame to an xts object
            self$df_price <- xts(self$df_price[, -which(names(self$df_price) == "date")], order.by=self$df_price$date)
            
            # Handle NA values
            self$df_price[is.na(self$df_price)] <- 0
        },
        
        rolling_window = function(data, window_size) {
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
            n_windows <- nrow(data) - window_size + 1
            tmp <- self$rolling_window(as.numeric(data$return), window_size)
            cumulative_returns <- apply(tmp, 2, PerformanceAnalytics::Return.cumulative)
            dates <- index(data)
            df <- data.frame(cumulative_returns)
            rownames(df) <- dates[1:n_windows]
            df
        },
        
        
        f_constructFeatures = function(ticker, df_price, window_size){
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
            numeric_tmp <- as.data.frame(lapply(tmp, as.numeric))
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

        f_predictLasso = function(trainingset, testingset) {
            
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
        # Retrieve the prices
                df <- self$df_price[index(self$df_price) %in% testRange$start:testRange$end, c("ticker", "close", "date")]
                df <- df[df$ticker %in% self$tickers]
                df$close <- as.numeric(df$close)

                # Create an xts object for each ticker using the date information from df
                list_xts <- lapply(unique(df$ticker), function(ticker) {
                    xts(df$close[df$ticker == ticker], order.by = as.Date(df$date[df$ticker == ticker]))
                })

                # Combine the cleaned xts objects
                combined_xts <- do.call(cbind, list_xts)

                # Convert the combined xts object to a matrix
                closePrice <- as.matrix(combined_xts)
                # Convert all columns of the matrix to numeric
                self$closePrice <- apply(closePrice, 2, as.numeric)

                tmp <- (1 + self$predictionMatrix) * self$closePrice
                rownames(tmp) <- index(combined_xts)
                self$pricePrediction <- tmp
                self$pricePrediction
            }





        
    )
)

init_instance = function(dataPrice, tickers) {

        # Initialiser notre instance :  LassoPrediction class
        lasso_pred <- LassoPrediction$new(dataPrice)
        lasso_pred$tickers <- tickers
        # Generate Momentum features for each ticker
        featuresList <- vector("list", length(lasso_pred$tickers))
        names(featuresList) <- lasso_pred$tickers

        for (i in seq_along(lasso_pred$tickers)){
            ticker <- lasso_pred$tickers[i]
            tmp <- lasso_pred$f_constructFeatures(ticker, lasso_pred$df_price, lasso_pred$window_size)
            featuresList[[i]] = tmp
            print(paste(ticker, " : Done"))
        }
        print("Features Building: Done")
        lasso_pred$featuresList <- featuresList
        # Get the training and testing date ranges for Lasso regression
        dates <- featuresList[[lasso_pred$tickers[1]]]$date 
        trainRange <- lasso_pred$get_date_range("2007-12-31", "2018-01-01", lasso_pred$window_size, dates)
        testRange <- lasso_pred$get_date_range("2017-12-31", "2023-01-01", 0, dates)

        n <- sum(dates %in% testRange$start:testRange$end)
        predictionMatrix <- matrix(NA, nrow=n, ncol=length(lasso_pred$tickers))
        colnames(predictionMatrix) <- lasso_pred$tickers

        resultsList <- vector("list", length(lasso_pred$tickers))
        names(resultsList) <- lasso_pred$tickers

        for (tick in seq_along(lasso_pred$tickers)) {
            df <- featuresList[[tick]]

            # Subset the data based on the defined time ranges
            trainSet <- df[df$date %in% trainRange$start:trainRange$end, ]
            testSet <- df[df$date %in% testRange$start:testRange$end, ]

            results <- lasso_pred$f_predictLasso(trainSet, testSet)

            # Store results in the main list
            resultsList[[tick]] <- list(
                predictions = as.xts(results$predictions),
                model = results$model,
                mse = mse(actual = testSet$Y, predicted = results$predictions),
                coefficients = results$coefficients
            )
            predictionMatrix[, tick] <- results$predictions

            print(paste(lasso_pred$tickers[tick], ", Finished, MSE:", resultsList[[tick]]$mse))
        }

        lasso_pred$resultsList <- resultsList
        print("Regression Building: Done")
        print(predictionMatrix)
        lasso_pred$predictionMatrix <- predictionMatrix

        lasso_pred$pricePrediction <- lasso_pred$get_price_prediction(testRange)

        return(lasso_pred)
}




get_stock_data <- function(tickers) {
  # Initialize a list to store the data
  data_list <- list()
  
  # Loop through each ticker and download the data
  for(ticker in tickers) {
    # Get the data from Yahoo Finance
    stock_data <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
    
    # Calculate adjusted values
    adj_factor <- Ad(stock_data) / Cl(stock_data)
    open_adj <- Op(stock_data) * adj_factor
    high_adj <- Hi(stock_data) * adj_factor
    low_adj <- Lo(stock_data) * adj_factor
    
    # Extract the required columns
    df <- data.frame(
      date = index(stock_data),
      ticker = ticker,
      open = open_adj,
      high = high_adj,
      low = low_adj,
      volume = Vo(stock_data),
      close = Ad(stock_data)
    )
    
    # Store the data in the list
    data_list[[ticker]] <- df
  }
  
  # Standardize column names for each data frame in the list
  for (ticker in tickers) {
    colnames(data_list[[ticker]]) <- c("date", "ticker", "open", "high", "low", "volume", "close")
  }

  
  # Combine all data frames in the list into a single data frame
  combined_data <- do.call(rbind, data_list)
  
  # Convert the 'Close' column to an xts object using the 'date' column as the index
  close_xts <- xts(combined_data$close, order.by=as.Date(combined_data$date))
  returns_xts <- Return.calculate(close_xts, method = "simple")
  combined_data$return <- coredata(returns_xts / 100)
  combined_data <- combined_data[-1, ]
  
  return(combined_data)
}


