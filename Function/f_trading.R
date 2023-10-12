f_run_strategy <- function(start_date, end_date, list_ratios_technicals, strategy, rf_model) {
  ### Cette execute la strategie choisit pour une période déterminée et crée ensuite la courbe d'équité.
  
  #  Inputs
  #   start_date: [Date] la date de début de la période..
  #   end_date: [Date] la date de fin de la période.
  #   list_ratios_technicals: [List] Liste qui contient les informations techniques pour chaques paires.
  #   strategy: [Character] La strategie choisit (Naive, Regression, Classification, Both)
  
  #  OUTPUTS
  #   list_trading: [List] Une liste contenant le trading pour chaque paires.
  #   df_all_trades: [Data.frame] Un data.frame avec tout les trades de la strategy (.rda et .xslx)
  
  # Starting Date
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Small function to crop data
  crop_data <- function(xts_obj, start_date, end_date) {
    tmp <- window(xts_obj, start = start_date, end = end_date)
    return(tmp)
  }
  
  # Crop the vol data
  list_ratios_technicals <- lapply(list_ratios_technicals, 
                                   crop_data, 
                                   start_date, end_date)
  
  
  # Naive Strategy
  if (strategy == "naive") {
    # Create the backtest 
    list_trading <- lapply(list_ratios_technicals, 
                           function(i) f_trading_naive(i, 10, (100/45), 0.05))
  } 
  
  # Regression Strategy
  if (strategy == "regression") {
    list_trading <- lapply(list_ratios_technicals, 
                           function(i) f_trading_regression(i, 10, (100/45), 0.05))
  }
  
  if (strategy == "classification") {
    list_trading <- lapply(list_ratios_technicals, 
                           function(i) f_trading_classification(i, 10, (100/45), rf_model))
  }
  
  # Retrieve all the trades
  list_trades <- lapply(list_trading, 
                        function(i) f_extract_trades(i))
  
  # Merge the trades from every pair
  df_all_trades <- do.call(rbind, list_trades)
  colnames(df_all_trades)[11] <- "profitable"
  
  # Save trades
  name_file <- paste0("df_all_trades_", strategy,"_",start_date,"_to_", end_date,".rda")
  save(df_all_trades, file = here('Output', name_file))
  cat(paste("Trades saved under:", name_file, "\n"))
  
  name_file_xlsx <- paste0("df_all_trades_", strategy,"_",start_date,"_to_", end_date,".xlsx")
  write_xlsx(df_all_trades, path = here('Output', name_file_xlsx))
  cat(paste("Trades saved under:", name_file_xlsx, "\n"))
  
  
  ### Create equity curve ###
  
  # Retrieve the equity curve from each pair in the list
  equity_curves <- lapply(list_trading, function(x) x$equity_curve)
  
  # Sum the curves
  total_equity_curve <- Reduce("+", equity_curves)
  
  # Convert xts to dataframe
  df_total_equity_curve <- as.data.frame(total_equity_curve)
  
  # Add the date index as a new column
  df_total_equity_curve$Date <- index(total_equity_curve)
  
  # Save equity curve
  name_file <- paste0("Equity_curve_", strategy,"_",start_date,"_to_", end_date,".rda")
  save(df_total_equity_curve, file = here('Output', name_file))
  cat(paste("Equity_curve saved under:", name_file, "\n"))
  
  name_file_xlsx <- paste0("Equity_curve_", strategy,"_",start_date,"_to_", end_date,".xlsx")
  write_xlsx(df_total_equity_curve, path = here('Output', name_file_xlsx))
  cat(paste("Equity_curve saved under:", name_file_xlsx, "\n"))
  
  return(list_trading)
}

f_trading_naive <- function(xts_obj, holding_period, trade_size, threshold) {
  ### Cette fonction applique la stratégie de trading sur le training et testing set.
  ### Elle crée le backtest.
  
  #  Inputs
  #   xts_obj: [xts_object] Objet qui contient un ratio de volatilités avec les indicateurs requis.
  #   holding_period: [scalar] Nombre de jours dans la trade. 
  #   trade_size: [scalar] capital par trade
  #   df_price_prediction: [Data.frame] Dataframe qui contient les predictions de ratios de prix dans 10 jours.
  
  #  OUTPUTS
  #   merged_data: [xts_object]  Objet qui contient les resultats de l'algorithme de trading.
  
  ### Notes ###
  # Trade flag:
  # 1 = buy
  # -1 = short
  # 11 = buy to close
  # -11 = sell to close
  
  # Store the stock name of the current pair
  Stock_A <- sub("_Price", "", colnames(xts_obj)[2])
  Stock_B <- sub("_Price", "", colnames(xts_obj)[3])
  
  # Check if stock is above 2 SD or below 2 SD at each timestep
  SD_flag <- ifelse(xts_obj[, "vol_ratio"] > xts_obj[,"up_band"], 2,
                    ifelse(xts_obj[, "vol_ratio"] < xts_obj[,"lo_band"], -2, 0))
  
  # Save the column price the time series its own object
  price_ratio <- xts_obj[,"price_ratio", ]
  
  # Accept flag is unused here, but retain the format for all stats.
  accept_flag <- xts_obj[, 1]*0
  
  # Create empty columns needed for the trading algorithm
  position <- xts_obj[, 1]*0
  trade_flag <- xts_obj[, 1]*0
  entry_price <- xts_obj[, 1]*0
  stop_price <- xts_obj[, 1]*0
  profit_price <- xts_obj[, 1]*0
  day_count <- xts_obj[, 1]*0
  exit_price <- xts_obj[, 1]*0
  
  ### Create empty columns for the PL construction
  current_price <- xts_obj[, 1]*0   # Current price of the underlying pair
  PL_position <- xts_obj[, 1]*0     # Current return of current trade
  Value_position <- xts_obj[, 1]*0
  closed_equity <- xts_obj[, 1]*0
  equity_curve <- xts_obj[, 1]*0
  
  # Start at index 2
  for (i in 2:nrow(xts_obj)) {
    # Look for trade flags
    if (!is.na(SD_flag[i-1])) {
      
      ### Ratio is below SD ###
      if (SD_flag[i-1] == -2) {
        
        # No position, enter short
        if (position[i-1] == 0) {
          position[i] <- -1
          trade_flag[i] <- -1
          day_count[i] <- 1
          
          # Current position, already short   
        } else if (position[i - 1] == -1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10  
          } else if (day_count[i-1] == holding_period) {
            # Close position
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- 11
          }
          
          # Current position, already long   
        } else if (position[i - 1] == 1) {
          # Close long position
          position[i] <- 0
          trade_flag[i] <- -11
          day_count[i] <- 0
        }
      }
      
      ### Ratio is above SD ###
      else if (SD_flag[i-1] == 2) {
        
        # No position, enter long
        if (position[i - 1] == 0) {
          position[i] <- 1
          trade_flag[i] <- 1
          day_count[i] <- 1
          
          # Current position, already long
        } else if (position[i - 1] == 1)  {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- -11
          }
          
          # Current position, already short 
        } else if (position[i - 1] == 1) { 
          # Close short position 
          position[i] <- 0
          trade_flag[i] <- 11
          day_count[i] <- 0
        }
      }
      
      ### Ratio is between SDs ###
      else {
        
        # No positon
        if (position[i-1] == 0) {
          # Do nothing
          position[i] <- position[i-1]
          day_count[i] <- 0
          
          # Position is short
        } else if (position[i - 1] == -1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- 11
          }
          
          # Position is long
        } else if (position[i - 1] == 1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- -11
          }
        }
      }
      
      
      ### Add some information
      # When trade is opened
      if (trade_flag[i] == 1 || trade_flag[i] == -1) {
        # Store entry price
        entry_price[i] <- price_ratio[i-1]
        
        # When trade is closed
      } else if (trade_flag[i] == 11 || trade_flag[i] == -11) {
        exit_price[i] <- price_ratio[i-1]
        
        # Current positon, retain entry price
      } else if (trade_flag[i] == 0  | (position[i] == 1 || position[i] == -1)) {
        entry_price[i] <- entry_price[i-1]
        
        # No trade, no positon
      } else 
        entry_price[i] <- 0
    }
  }
  
  message <- sprintf("%s/%s: done - naive", Stock_A, Stock_B)
  print(message)
  
  ### PL and equity curve ###
  # Merge all columns into the output xts object, including the SD_flag
  xts_output <- merge(xts_obj, SD_flag, position, trade_flag, accept_flag, day_count, entry_price, exit_price, Value_position, closed_equity, equity_curve)
  colnames(xts_output)[10:19] <- c("SD_flag", "position", "trade_flag", "accept_flag", "day_count", "entry_price", "exit_price", "Value_position", "closed_equity", "equity_curve")
  
  ## Calculate the equity curve
  # Start at the trade_size amount
  xts_output[1, "equity_curve"] <- trade_size
  xts_output[1, "closed_equity"] <- trade_size
  
  for (i in 2:nrow(xts_output)) {
    tmp <- as.numeric(xts_output[i ,"day_count"])
    # A position is opened, calculate equity curve
    if (xts_output[i, "position"] == 1) {
      xts_output[i, "Value_position"] <- (as.numeric(xts_output[i, 2]) / as.numeric(xts_output[i-tmp, 2]) -1) * trade_size - (as.numeric(xts_output[i, 3] / as.numeric(xts_output[i-tmp, 3])) -1) * trade_size 
      xts_output[i, "equity_curve"] <- as.numeric(xts_output[i-as.numeric(tmp), "equity_curve"]) + as.numeric(xts_output[i, "Value_position"])
    } else if (xts_output[i, "position"] == -1) {
      xts_output[i, "Value_position"] <- (-1*((as.numeric(xts_output[i, 2]) / as.numeric(xts_output[i-tmp, 2])) -1)) * trade_size + ((as.numeric(xts_output[i, 3]) / as.numeric(xts_output[i-tmp, 3])) -1) * trade_size
      xts_output[i, "equity_curve"] <- as.numeric(xts_output[i-as.numeric(tmp), "equity_curve"]) + as.numeric(xts_output[i, "Value_position"])
    } else if ((xts_output[i, "position"] == 0)) {
      xts_output[i, "Value_position"] <- 0
      xts_output[i, "equity_curve"] <- xts_output[i-1, "equity_curve"]
    }
  }
  
  # Calculate the close equity
  for (i in 2:nrow(xts_output)) {
    if (trade_flag[i] == 11 || trade_flag[i] == -11) {
      xts_output[i, "closed_equity"] <- xts_output[i-1, "equity_curve"]
    } else {
      xts_output[i, "closed_equity"] <- xts_output[i-1, "closed_equity"]
    }
  }
  
  return(xts_output)  
}

f_trading_regression <- function(xts_obj, holding_period, trade_size, threshold) {
  ### Cette fonction applique la stratégie de trading sur le training et testing set.
  ### Elle crée le backtest.
  
  #  Inputs
  #   xts_obj: [xts_object] Objet qui contient un ratio de volatilités avec les indicateurs requis.
  #   holding_period: [scalar] Nombre de jours dans la trade. 
  #   trade_size: [scalar] capital par trade
  #   df_price_prediction: [Data.frame] Dataframe qui contient les predictions de ratios de prix dans 10 jours.
  
  #  OUTPUTS
  #   merged_data: [xts_object]  Objet qui contient les resultats de l'algorithme de trading.
  
  ### Notes ###
  # Trade flag:
  # 1 = buy
  # -1 = short
  # 11 = buy to close
  # -11 = sell to close
  
  # Store the stock name of the current pair
  Stock_A <- sub("_Price", "", colnames(xts_obj)[2])
  Stock_B <- sub("_Price", "", colnames(xts_obj)[3])
  
  # Check if stock is above 2 SD or below 2 SD at each timestep
  SD_flag <- ifelse(xts_obj[, "vol_ratio"] > xts_obj[,"up_band"], 2,
                    ifelse(xts_obj[, "vol_ratio"] < xts_obj[,"lo_band"], -2, 0))
  
  # Save the column price the time series its own object
  price_ratio <- xts_obj[,"price_ratio", ]
  
  # Retrieve price ratio prediction
  ratio_price_predict <- xts_obj[,"price_prediction", ]
  accept_flag <- xts_obj[, 1]*0
  accept_flag <- ifelse(ratio_price_predict > (1+threshold) * price_ratio, 1,
                        ifelse(ratio_price_predict < (1-threshold) * price_ratio, -1, 0))
  
  # Create empty columns needed for the trading algo
  position <- xts_obj[, 1]*0
  trade_flag <- xts_obj[, 1]*0
  entry_price <- xts_obj[, 1]*0
  stop_price <- xts_obj[, 1]*0
  profit_price <- xts_obj[, 1]*0
  day_count <- xts_obj[, 1]*0
  exit_price <- xts_obj[, 1]*0
  
  ### Create empty columns for the PL construction
  current_price <- xts_obj[, 1]*0   # Current price of the underlying pair
  PL_position <- xts_obj[, 1]*0     # Current return of current trade
  Value_position <- xts_obj[, 1]*0
  closed_equity <- xts_obj[, 1]*0
  equity_curve <- xts_obj[, 1]*0
  
  # Start at index 2
  for (i in 2:nrow(xts_obj)) {
    # Look for trade flags
    if (!is.na(SD_flag[i-1])) {
      
      ### Ratio is below SD ###
      if (SD_flag[i-1] == -2 & accept_flag[i-1] == -1) {
        
        # No position, enter short
        if (position[i-1] == 0) {
          position[i] <- -1
          trade_flag[i] <- -1
          day_count[i] <- 1
          
          # Current position, already short   
        } else if (position[i - 1] == -1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10  
          } else if (day_count[i-1] == holding_period) {
            # Close position
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- 11
          }
          
          # Current position, already long   
        } else if (position[i - 1] == 1) {
          # Close long position
          position[i] <- 0
          trade_flag[i] <- -11
          day_count[i] <- 0
        }
      }
      
      ### Ratio is above SD ###
      else if (SD_flag[i-1] == 2 & accept_flag[i-1] == 1) {
        
        # No position, enter long
        if (position[i - 1] == 0) {
          position[i] <- 1
          trade_flag[i] <- 1
          day_count[i] <- 1
          
          # Current position, already long
        } else if (position[i - 1] == 1)  {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- -11
          }
          
          # Current position, already short 
        } else if (position[i - 1] == 1) { 
          # Close short position 
          position[i] <- 0
          trade_flag[i] <- 11
          day_count[i] <- 0
        }
      }
      
      ### Ratio is between SDs ###
      else {
        
        # No positon
        if (position[i-1] == 0) {
          # Do nothing
          position[i] <- position[i-1]
          day_count[i] <- 0
          
          # Position is short
        } else if (position[i - 1] == -1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- 11
          }
          
          # Position is long
        } else if (position[i - 1] == 1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- -11
          }
        }
      }
      
      
      ### Add some information
      # When trade is opened
      if (trade_flag[i] == 1 || trade_flag[i] == -1) {
        # Store entry price
        entry_price[i] <- price_ratio[i-1]
        
        # When trade is closed
      } else if (trade_flag[i] == 11 || trade_flag[i] == -11) {
        exit_price[i] <- price_ratio[i-1]
        
        # Current positon, retain entry price
      } else if (trade_flag[i] == 0  | (position[i] == 1 || position[i] == -1)) {
        entry_price[i] <- entry_price[i-1]
        
        # No trade, no positon
      } else 
        entry_price[i] <- 0
    }
  }
  
  message <- sprintf("%s/%s: done - regression", Stock_A, Stock_B)
  print(message)
  
  ### PL and equity curve ###
  # Merge all columns into the output xts object, including the SD_flag
  xts_output <- merge(xts_obj, SD_flag, position, trade_flag, accept_flag, day_count, entry_price, exit_price, PL_position, Value_position, closed_equity, equity_curve)
  colnames(xts_output)[10:20] <- c("SD_flag", "position", "trade_flag", "accept_flag", "day_count", "entry_price", "exit_price", "PL_position", "Value_position", "closed_equity", "equity_curve")
  
  ## Calculate the equity curve
  # Start at the trade_size amount
  xts_output[1, "equity_curve"] <- trade_size
  xts_output[1, "closed_equity"] <- trade_size
  
  for (i in 2:nrow(xts_output)) {
    tmp <- as.numeric(xts_output[i ,"day_count"])
    # A position is opened, calculate equity curve
    if (xts_output[i, "position"] == 1) {
      xts_output[i, "Value_position"] <- (as.numeric(xts_output[i, 2]) / as.numeric(xts_output[i-tmp, 2]) -1) * trade_size - (as.numeric(xts_output[i, 3] / as.numeric(xts_output[i-tmp, 3])) -1) * trade_size 
      xts_output[i, "equity_curve"] <- as.numeric(xts_output[i-as.numeric(tmp), "equity_curve"]) + as.numeric(xts_output[i, "Value_position"])
    } else if (xts_output[i, "position"] == -1) {
      xts_output[i, "Value_position"] <- (-1*((as.numeric(xts_output[i, 2]) / as.numeric(xts_output[i-tmp, 2])) -1)) * trade_size + ((as.numeric(xts_output[i, 3]) / as.numeric(xts_output[i-tmp, 3])) -1) * trade_size
      xts_output[i, "equity_curve"] <- as.numeric(xts_output[i-as.numeric(tmp), "equity_curve"]) + as.numeric(xts_output[i, "Value_position"])
    } else if ((xts_output[i, "position"] == 0)) {
      xts_output[i, "Value_position"] <- 0
      xts_output[i, "equity_curve"] <- xts_output[i-1, "equity_curve"]
    }
  }
  
  # Calculate the close equity
  for (i in 2:nrow(xts_output)) {
    if (trade_flag[i] == 11 || trade_flag[i] == -11) {
      xts_output[i, "closed_equity"] <- xts_output[i-1, "equity_curve"]
    } else {
      xts_output[i, "closed_equity"] <- xts_output[i-1, "closed_equity"]
    }
  }
  
  return(xts_output)
}

f_trading_clustering <- function(xts_obj, holding_period, trade_size, threshold) {
  ### Cette fonction applique la stratégie de trading sur le training et testing set.
  ### Elle crée le backtest.
  
  #  Inputs
  #   xts_obj: [xts_object] Objet qui contient un ratio de volatilités avec les indicateurs requis.
  #   holding_period: [scalar] Nombre de jours dans la trade. 
  #   trade_size: [scalar] capital par trade
  #   df_price_prediction: [Data.frame] Dataframe qui contient les predictions de ratios de prix dans 10 jours.
  
  #  OUTPUTS
  #   merged_data: [xts_object]  Objet qui contient les resultats de l'algorithme de trading.
  
  ### Notes ###
  # Trade flag:
  # 1 = buy
  # -1 = short
  # 11 = buy to close
  # -11 = sell to close
  
  # Store the stock name of the current pair
  Stock_A <- sub("_Price", "", colnames(xts_obj)[2])
  Stock_B <- sub("_Price", "", colnames(xts_obj)[3])
  
  # Check if stock is above 2 SD or below 2 SD at each timestep
  SD_flag <- ifelse(xts_obj[, "vol_ratio"] > xts_obj[,"up_band"], 2,
                    ifelse(xts_obj[, "vol_ratio"] < xts_obj[,"lo_band"], -2, 0))
  
  # Save the column price the time series its own object
  price_ratio <- xts_obj[,"price_ratio", ]
  
  # Retrieve price ratio prediction
  ratio_price_predict <- xts_obj[,"price_prediction", ]
  accept_flag <- xts_obj[, 1]*0
  accept_flag <- ifelse(ratio_price_predict > (1+threshold) * price_ratio, 1,
                        ifelse(ratio_price_predict < (1-threshold) * price_ratio, -1, 0))
  
  # Create empty columns needed for the trading algo
  position <- xts_obj[, 1]*0
  trade_flag <- xts_obj[, 1]*0
  entry_price <- xts_obj[, 1]*0
  stop_price <- xts_obj[, 1]*0
  profit_price <- xts_obj[, 1]*0
  day_count <- xts_obj[, 1]*0
  exit_price <- xts_obj[, 1]*0
  
  ### Create empty columns for the PL construction
  current_price <- xts_obj[, 1]*0   # Current price of the underlying pair
  PL_position <- xts_obj[, 1]*0     # Current return of current trade
  Value_position <- xts_obj[, 1]*0
  closed_equity <- xts_obj[, 1]*0
  equity_curve <- xts_obj[, 1]*0
  
  # Start at index 2
  for (i in 2:nrow(xts_obj)) {
    # Look for trade flags
    if (!is.na(SD_flag[i-1])) {
      
      ### Ratio is below SD ###
      if (SD_flag[i-1] == -2 & accept_flag[i-1] == -1) {
        
        # No position, enter short
        if (position[i-1] == 0) {
          position[i] <- -1
          trade_flag[i] <- -1
          day_count[i] <- 1
          
          # Current position, already short   
        } else if (position[i - 1] == -1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10  
          } else if (day_count[i-1] == holding_period) {
            # Close position
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- 11
          }
          
          # Current position, already long   
        } else if (position[i - 1] == 1) {
          # Close long position
          position[i] <- 0
          trade_flag[i] <- -11
          day_count[i] <- 0
        }
      }
      
      ### Ratio is above SD ###
      else if (SD_flag[i-1] == 2 & accept_flag[i-1] == 1) {
        
        # No position, enter long
        if (position[i - 1] == 0) {
          position[i] <- 1
          trade_flag[i] <- 1
          day_count[i] <- 1
          
          # Current position, already long
        } else if (position[i - 1] == 1)  {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- -11
          }
          
          # Current position, already short 
        } else if (position[i - 1] == 1) { 
          # Close short position 
          position[i] <- 0
          trade_flag[i] <- 11
          day_count[i] <- 0
        }
      }
      
      ### Ratio is between SDs ###
      else {
        
        # No positon
        if (position[i-1] == 0) {
          # Do nothing
          position[i] <- position[i-1]
          day_count[i] <- 0
          
          # Position is short
        } else if (position[i - 1] == -1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- 11
          }
          
          # Position is long
        } else if (position[i - 1] == 1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- -11
          }
        }
      }
      
      
      ### Add some information
      # When trade is opened
      if (trade_flag[i] == 1 || trade_flag[i] == -1) {
        # Store entry price
        entry_price[i] <- price_ratio[i-1]
        
        # When trade is closed
      } else if (trade_flag[i] == 11 || trade_flag[i] == -11) {
        exit_price[i] <- price_ratio[i-1]
        
        # Current positon, retain entry price
      } else if (trade_flag[i] == 0  | (position[i] == 1 || position[i] == -1)) {
        entry_price[i] <- entry_price[i-1]
        
        # No trade, no positon
      } else 
        entry_price[i] <- 0
    }
  }
  
  message <- sprintf("%s/%s: done - regression", Stock_A, Stock_B)
  print(message)
  
  ### PL and equity curve ###
  # Merge all columns into the output xts object, including the SD_flag
  xts_output <- merge(xts_obj, SD_flag, position, trade_flag, accept_flag, day_count, entry_price, exit_price, PL_position, Value_position, closed_equity, equity_curve)
  colnames(xts_output)[10:20] <- c("SD_flag", "position", "trade_flag", "accept_flag", "day_count", "entry_price", "exit_price", "PL_position", "Value_position", "closed_equity", "equity_curve")
  
  ## Calculate the equity curve
  # Start at the trade_size amount
  xts_output[1, "equity_curve"] <- trade_size
  xts_output[1, "closed_equity"] <- trade_size
  
  for (i in 2:nrow(xts_output)) {
    tmp <- as.numeric(xts_output[i ,"day_count"])
    # A position is opened, calculate equity curve
    if (xts_output[i, "position"] == 1) {
      xts_output[i, "Value_position"] <- (as.numeric(xts_output[i, 2]) / as.numeric(xts_output[i-tmp, 2]) -1) * trade_size - (as.numeric(xts_output[i, 3] / as.numeric(xts_output[i-tmp, 3])) -1) * trade_size 
      xts_output[i, "equity_curve"] <- as.numeric(xts_output[i-as.numeric(tmp), "equity_curve"]) + as.numeric(xts_output[i, "Value_position"])
    } else if (xts_output[i, "position"] == -1) {
      xts_output[i, "Value_position"] <- (-1*((as.numeric(xts_output[i, 2]) / as.numeric(xts_output[i-tmp, 2])) -1)) * trade_size + ((as.numeric(xts_output[i, 3]) / as.numeric(xts_output[i-tmp, 3])) -1) * trade_size
      xts_output[i, "equity_curve"] <- as.numeric(xts_output[i-as.numeric(tmp), "equity_curve"]) + as.numeric(xts_output[i, "Value_position"])
    } else if ((xts_output[i, "position"] == 0)) {
      xts_output[i, "Value_position"] <- 0
      xts_output[i, "equity_curve"] <- xts_output[i-1, "equity_curve"]
    }
  }
  
  # Calculate the close equity
  for (i in 2:nrow(xts_output)) {
    if (trade_flag[i] == 11 || trade_flag[i] == -11) {
      xts_output[i, "closed_equity"] <- xts_output[i-1, "equity_curve"]
    } else {
      xts_output[i, "closed_equity"] <- xts_output[i-1, "closed_equity"]
    }
  }
  
  return(xts_output)
}

f_trading_both <- function(xts_obj, holding_period, trade_size, threshold) {
  ### Cette fonction applique la stratégie de trading sur le training et testing set.
  ### Elle crée le backtest.
  
  #  Inputs
  #   xts_obj: [xts_object] Objet qui contient un ratio de volatilités avec les indicateurs requis.
  #   holding_period: [scalar] Nombre de jours dans la trade. 
  #   trade_size: [scalar] capital par trade
  #   df_price_prediction: [Data.frame] Dataframe qui contient les predictions de ratios de prix dans 10 jours.
  
  #  OUTPUTS
  #   merged_data: [xts_object]  Objet qui contient les resultats de l'algorithme de trading.
  
  ### Notes ###
  # Trade flag:
  # 1 = buy
  # -1 = short
  # 11 = buy to close
  # -11 = sell to close
  
  # Store the stock name of the current pair
  Stock_A <- sub("_Price", "", colnames(xts_obj)[2])
  Stock_B <- sub("_Price", "", colnames(xts_obj)[3])
  
  # Check if stock is above 2 SD or below 2 SD at each timestep
  SD_flag <- ifelse(xts_obj[, "vol_ratio"] > xts_obj[,"up_band"], 2,
                    ifelse(xts_obj[, "vol_ratio"] < xts_obj[,"lo_band"], -2, 0))
  
  # Save the column price the time series its own object
  price_ratio <- xts_obj[,"price_ratio", ]
  
  # Retrieve price ratio prediction
  ratio_price_predict <- xts_obj[,"price_prediction", ]
  accept_flag <- xts_obj[, 1]*0
  accept_flag <- ifelse(ratio_price_predict > (1+threshold) * price_ratio, 1,
                        ifelse(ratio_price_predict < (1-threshold) * price_ratio, -1, 0))
  
  # Create empty columns needed for the trading algo
  position <- xts_obj[, 1]*0
  trade_flag <- xts_obj[, 1]*0
  entry_price <- xts_obj[, 1]*0
  stop_price <- xts_obj[, 1]*0
  profit_price <- xts_obj[, 1]*0
  day_count <- xts_obj[, 1]*0
  exit_price <- xts_obj[, 1]*0
  
  ### Create empty columns for the PL construction
  current_price <- xts_obj[, 1]*0   # Current price of the underlying pair
  PL_position <- xts_obj[, 1]*0     # Current return of current trade
  Value_position <- xts_obj[, 1]*0
  closed_equity <- xts_obj[, 1]*0
  equity_curve <- xts_obj[, 1]*0
  
  # Start at index 2
  for (i in 2:nrow(xts_obj)) {
    # Look for trade flags
    if (!is.na(SD_flag[i-1])) {
      
      ### Ratio is below SD ###
      if (SD_flag[i-1] == -2 & accept_flag[i-1] == -1) {
        
        # No position, enter short
        if (position[i-1] == 0) {
          position[i] <- -1
          trade_flag[i] <- -1
          day_count[i] <- 1
          
          # Current position, already short   
        } else if (position[i - 1] == -1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10  
          } else if (day_count[i-1] == holding_period) {
            # Close position
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- 11
          }
          
          # Current position, already long   
        } else if (position[i - 1] == 1) {
          # Close long position
          position[i] <- 0
          trade_flag[i] <- -11
          day_count[i] <- 0
        }
      }
      
      ### Ratio is above SD ###
      else if (SD_flag[i-1] == 2 & accept_flag[i-1] == 1) {
        
        # No position, enter long
        if (position[i - 1] == 0) {
          position[i] <- 1
          trade_flag[i] <- 1
          day_count[i] <- 1
          
          # Current position, already long
        } else if (position[i - 1] == 1)  {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- -11
          }
          
          # Current position, already short 
        } else if (position[i - 1] == 1) { 
          # Close short position 
          position[i] <- 0
          trade_flag[i] <- 11
          day_count[i] <- 0
        }
      }
      
      ### Ratio is between SDs ###
      else {
        
        # No positon
        if (position[i-1] == 0) {
          # Do nothing
          position[i] <- position[i-1]
          day_count[i] <- 0
          
          # Position is short
        } else if (position[i - 1] == -1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- 11
          }
          
          # Position is long
        } else if (position[i - 1] == 1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- -11
          }
        }
      }
      
      
      ### Add some information
      # When trade is opened
      if (trade_flag[i] == 1 || trade_flag[i] == -1) {
        # Store entry price
        entry_price[i] <- price_ratio[i-1]
        
        # When trade is closed
      } else if (trade_flag[i] == 11 || trade_flag[i] == -11) {
        exit_price[i] <- price_ratio[i-1]
        
        # Current positon, retain entry price
      } else if (trade_flag[i] == 0  | (position[i] == 1 || position[i] == -1)) {
        entry_price[i] <- entry_price[i-1]
        
        # No trade, no positon
      } else 
        entry_price[i] <- 0
    }
  }
  
  message <- sprintf("%s/%s: done - regression", Stock_A, Stock_B)
  print(message)
  
  ### PL and equity curve ###
  # Merge all columns into the output xts object, including the SD_flag
  xts_output <- merge(xts_obj, SD_flag, position, trade_flag, accept_flag, day_count, entry_price, exit_price, PL_position, Value_position, closed_equity, equity_curve)
  colnames(xts_output)[10:20] <- c("SD_flag", "position", "trade_flag", "accept_flag", "day_count", "entry_price", "exit_price", "PL_position", "Value_position", "closed_equity", "equity_curve")
  
  ## Calculate the equity curve
  # Start at the trade_size amount
  xts_output[1, "equity_curve"] <- trade_size
  xts_output[1, "closed_equity"] <- trade_size
  
  for (i in 2:nrow(xts_output)) {
    tmp <- as.numeric(xts_output[i ,"day_count"])
    # A position is opened, calculate equity curve
    if (xts_output[i, "position"] == 1) {
      xts_output[i, "Value_position"] <- (as.numeric(xts_output[i, 2]) / as.numeric(xts_output[i-tmp, 2]) -1) * trade_size - (as.numeric(xts_output[i, 3] / as.numeric(xts_output[i-tmp, 3])) -1) * trade_size 
      xts_output[i, "equity_curve"] <- as.numeric(xts_output[i-as.numeric(tmp), "equity_curve"]) + as.numeric(xts_output[i, "Value_position"])
    } else if (xts_output[i, "position"] == -1) {
      xts_output[i, "Value_position"] <- (-1*((as.numeric(xts_output[i, 2]) / as.numeric(xts_output[i-tmp, 2])) -1)) * trade_size + ((as.numeric(xts_output[i, 3]) / as.numeric(xts_output[i-tmp, 3])) -1) * trade_size
      xts_output[i, "equity_curve"] <- as.numeric(xts_output[i-as.numeric(tmp), "equity_curve"]) + as.numeric(xts_output[i, "Value_position"])
    } else if ((xts_output[i, "position"] == 0)) {
      xts_output[i, "Value_position"] <- 0
      xts_output[i, "equity_curve"] <- xts_output[i-1, "equity_curve"]
    }
  }
  
  # Calculate the close equity
  for (i in 2:nrow(xts_output)) {
    if (trade_flag[i] == 11 || trade_flag[i] == -11) {
      xts_output[i, "closed_equity"] <- xts_output[i-1, "equity_curve"]
    } else {
      xts_output[i, "closed_equity"] <- xts_output[i-1, "closed_equity"]
    }
  }
  
  
  return(xts_output)
}

f_trading_classification <- function(xts_obj, holding_period, trade_size, rf_model) {
  ### Cette fonction applique la stratégie de trading sur le training et testing set.
  ### Elle crée le backtest.
  
  #  Inputs
  #   xts_obj: [xts_object] Objet qui contient un ratio de volatilités avec les indicateurs requis.
  #   holding_period: [scalar] Nombre de jours dans la trade. 
  #   trade_size: [scalar] capital par trade
  #   rf_model: [train] modele random forest calibré sur les trades in sample
  
  #  OUTPUTS
  #   merged_data: [xts_object]  Objet qui contient les resultats de l'algorithme de trading.
  
  ### Notes ###
  # Trade flag:
  # 1 = buy
  # -1 = short
  # 11 = buy to close
  # -11 = sell to close
  
  # Store the stock name of the current pair
  Stock_A <- sub("_Price", "", colnames(xts_obj)[2])
  Stock_B <- sub("_Price", "", colnames(xts_obj)[3])
  
  # Check if stock is above 2 SD or below 2 SD at each timestep
  SD_flag <- ifelse(xts_obj[, "vol_ratio"] > xts_obj[,"up_band"], 2,
                    ifelse(xts_obj[, "vol_ratio"] < xts_obj[,"lo_band"], -2, 0))
  
  # Save the column price the time series its own object
  price_ratio <- xts_obj[,"price_ratio", ]
  
  # Accept flags unused
  accept_flag <- xts_obj[, 1]*0
  
  # Create empty columns needed for the trading algo
  position <- xts_obj[, 1]*0
  trade_flag <- xts_obj[, 1]*0
  entry_price <- xts_obj[, 1]*0
  stop_price <- xts_obj[, 1]*0
  profit_price <- xts_obj[, 1]*0
  day_count <- xts_obj[, 1]*0
  exit_price <- xts_obj[, 1]*0
  
  ### Create empty columns for the PL construction
  current_price <- xts_obj[, 1]*0   # Current price of the underlying pair
  PL_position <- xts_obj[, 1]*0     # Current return of current trade
  Value_position <- xts_obj[, 1]*0
  closed_equity <- xts_obj[, 1]*0
  equity_curve <- xts_obj[, 1]*0
  
  # Start at index 2
  for (i in 2:nrow(xts_obj)) {
    # Look for trade flags
    if (!is.na(SD_flag[i-1])) {
      
      # Il ne resterait plus qu'a utiliser les inputs bloomberg joint a list_ratio_technicals pour faire la prediction
      inputs = 0
      predicted_value = predict(rf_model, inputs)
      ### Ratio is below SD ###
      if (SD_flag[i-1] == -2 & accept_flag[i-1] == -1) {
        
        # No position, enter short
        if (position[i-1] == 0) {
          position[i] <- -1
          trade_flag[i] <- -1
          day_count[i] <- 1
          
          # Current position, already short   
        } else if (position[i - 1] == -1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10  
          } else if (day_count[i-1] == holding_period) {
            # Close position
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- 11
          }
          
          # Current position, already long   
        } else if (position[i - 1] == 1) {
          # Close long position
          position[i] <- 0
          trade_flag[i] <- -11
          day_count[i] <- 0
        }
      }
      
      ### Ratio is above SD ###
      else if (SD_flag[i-1] == 2 & accept_flag[i-1] == 1) {
        
        # No position, enter long
        if (position[i - 1] == 0) {
          position[i] <- 1
          trade_flag[i] <- 1
          day_count[i] <- 1
          
          # Current position, already long
        } else if (position[i - 1] == 1)  {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- -11
          }
          
          # Current position, already short 
        } else if (position[i - 1] == 1) { 
          # Close short position 
          position[i] <- 0
          trade_flag[i] <- 11
          day_count[i] <- 0
        }
      }
      
      ### Ratio is between SDs ###
      else {
        
        # No positon
        if (position[i-1] == 0) {
          # Do nothing
          position[i] <- position[i-1]
          day_count[i] <- 0
          
          # Position is short
        } else if (position[i - 1] == -1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- 11
          }
          
          # Position is long
        } else if (position[i - 1] == 1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- -11
          }
        }
      }
      
      
      ### Add some information
      # When trade is opened
      if (trade_flag[i] == 1 || trade_flag[i] == -1) {
        # Store entry price
        entry_price[i] <- price_ratio[i-1]
        
        # When trade is closed
      } else if (trade_flag[i] == 11 || trade_flag[i] == -11) {
        exit_price[i] <- price_ratio[i-1]
        
        # Current positon, retain entry price
      } else if (trade_flag[i] == 0  | (position[i] == 1 || position[i] == -1)) {
        entry_price[i] <- entry_price[i-1]
        
        # No trade, no positon
      } else 
        entry_price[i] <- 0
    }
  }
  
  message <- sprintf("%s/%s: done - regression", Stock_A, Stock_B)
  print(message)
  
  ### PL and equity curve ###
  # Merge all columns into the output xts object, including the SD_flag
  xts_output <- merge(xts_obj, SD_flag, position, trade_flag, accept_flag, day_count, entry_price, exit_price, PL_position, Value_position, closed_equity, equity_curve)
  colnames(xts_output)[10:20] <- c("SD_flag", "position", "trade_flag", "accept_flag", "day_count", "entry_price", "exit_price", "PL_position", "Value_position", "closed_equity", "equity_curve")
  
  ## Calculate the equity curve
  # Start at the trade_size amount
  xts_output[1, "equity_curve"] <- trade_size
  xts_output[1, "closed_equity"] <- trade_size
  
  for (i in 2:nrow(xts_output)) {
    tmp <- as.numeric(xts_output[i ,"day_count"])
    # A position is opened, calculate equity curve
    if (xts_output[i, "position"] == 1) {
      xts_output[i, "Value_position"] <- (as.numeric(xts_output[i, 2]) / as.numeric(xts_output[i-tmp, 2]) -1) * trade_size - (as.numeric(xts_output[i, 3] / as.numeric(xts_output[i-tmp, 3])) -1) * trade_size 
      xts_output[i, "equity_curve"] <- as.numeric(xts_output[i-as.numeric(tmp), "equity_curve"]) + as.numeric(xts_output[i, "Value_position"])
    } else if (xts_output[i, "position"] == -1) {
      xts_output[i, "Value_position"] <- (-1*((as.numeric(xts_output[i, 2]) / as.numeric(xts_output[i-tmp, 2])) -1)) * trade_size + ((as.numeric(xts_output[i, 3]) / as.numeric(xts_output[i-tmp, 3])) -1) * trade_size
      xts_output[i, "equity_curve"] <- as.numeric(xts_output[i-as.numeric(tmp), "equity_curve"]) + as.numeric(xts_output[i, "Value_position"])
    } else if ((xts_output[i, "position"] == 0)) {
      xts_output[i, "Value_position"] <- 0
      xts_output[i, "equity_curve"] <- xts_output[i-1, "equity_curve"]
    }
  }
  
  # Calculate the close equity
  for (i in 2:nrow(xts_output)) {
    if (trade_flag[i] == 11 || trade_flag[i] == -11) {
      xts_output[i, "closed_equity"] <- xts_output[i-1, "equity_curve"]
    } else {
      xts_output[i, "closed_equity"] <- xts_output[i-1, "closed_equity"]
    }
  }
  
  return(xts_output)
}
f_extract_trades <- function(xts_object){
  ### Cette fonction retrouve toutes les trades executés dans l'historique de trading
  ### et les enregistre dans un data.frame.
  
  #  Inputs
  #   xts_object: [xts_object] Un object contenant l'historique de trading d'une paire.
  
  #  OUTPUTS
  #   output: [data.frame] (N x C) Data.frame contenant les trades executés dans le backtest.
  
  # Empty list to store the trades
  trades <- list()
  # Intitialize variable to store each trade
  current_trade <- NULL
  
  # Look row by row for trades
  for(i in 1:nrow(xts_object)){
    
    # Store the trade flag
    flag <- xts_object[i, "trade_flag"]
    
    # If trade flag is open long or open short
    if(flag %in% c(1, -1)){
      # Store the trade
      current_trade <- list(start_date=index(xts_object)[i], 
                            entry_price=xts_object[i, "entry_price"], 
                            direction=flag)
      # Store information
      current_trade$MA_100 <- xts_object[i, "MA_100"]
      current_trade$up_band <- xts_object[i, "up_band"]
      current_trade$lo_band <- xts_object[i, "lo_band"]
      current_trade$current_SD <- xts_object[i-1, "current_SD_level"]
      
      # If trade flag is close long or close short
    } else if(flag %in% c(-11, 11) && !is.null(current_trade)){
      # Store information
      current_trade$end_date <- index(xts_object)[i]
      current_trade$exit_price <- xts_object[i, "exit_price"]
      current_trade$value <- xts_object[i-1, "Value_position"]
      
      # Store profitability flag
      current_trade$profitable <- ifelse(current_trade$value > 0, 1, 0)
      current_trade$days <- xts_object[i-1, "day_count"]
      current_trade$ratio <- paste(sub("_Price", "", colnames(xts_object)[2]), 
                                   sub("_Price", "", colnames(xts_object)[3]), 
                                   sep="/")
      
      # Append onto the trade list
      trades <- append(trades, list(current_trade))
      
      # Empty the current_trade variable
      current_trade <- NULL
    }
  }
  output <- do.call(rbind, lapply(trades, as.data.frame))
  
  return(output)
}

f_trading_ <- function(xts_obj, holding_period, trade_size, threshold) {
  ### Cette fonction applique la stratégie de trading sur le training et testing set.
  ### Elle crée le backtest.
  
  #  Inputs
  #   xts_obj: [xts_object] Objet qui contient un ratio de volatilités avec les indicateurs requis.
  #   holding_period: [scalar] Nombre de jours dans la trade. 
  #   trade_size: [scalar] capital par trade
  #   df_price_prediction: [Data.frame] Dataframe qui contient les predictions de ratios de prix dans 10 jours.
  
  #  OUTPUTS
  #   merged_data: [xts_object]  Objet qui contient les resultats de l'algorithme de trading.
  
  ### Notes ###
  # Trade flag:
  # 1 = buy
  # -1 = short
  # 11 = buy to close
  # -11 = sell to close
  
  # Store the stock name of the current pair
  Stock_A <- sub("_Price", "", colnames(xts_obj)[2])
  Stock_B <- sub("_Price", "", colnames(xts_obj)[3])
  
  # Check if stock is above 2 SD or below 2 SD at each timestep
  SD_flag <- ifelse(xts_obj[, "vol_ratio"] > xts_obj[,"up_band"], 2,
                    ifelse(xts_obj[, "vol_ratio"] < xts_obj[,"lo_band"], -2, 0))
  
  # Save the column price the time series its own object
  price_ratio <- xts_obj[,"price_ratio", ]
  
  # Accept flag is unused here, but retain the format for all stats.
  accept_flag <- xts_obj[, 1]*0
  
  # Create empty columns needed for the trading algorithm
  position <- xts_obj[, 1]*0
  trade_flag <- xts_obj[, 1]*0
  entry_price <- xts_obj[, 1]*0
  stop_price <- xts_obj[, 1]*0
  profit_price <- xts_obj[, 1]*0
  day_count <- xts_obj[, 1]*0
  exit_price <- xts_obj[, 1]*0
  
  ### Create empty columns for the PL construction
  current_price <- xts_obj[, 1]*0   # Current price of the underlying pair
  PL_position <- xts_obj[, 1]*0     # Current return of current trade
  Value_position <- xts_obj[, 1]*0
  closed_equity <- xts_obj[, 1]*0
  equity_curve <- xts_obj[, 1]*0
  
  # Start at index 2
  for (i in 2:nrow(xts_obj)) {
    # Look for trade flags
    if (!is.na(SD_flag[i-1])) {
      
      ### Ratio is below SD ###
      if (SD_flag[i-1] == -2) {
        
        # No position, enter short
        if (position[i-1] == 0) {
          position[i] <- -1
          trade_flag[i] <- -1
          day_count[i] <- 1
          
          # Current position, already short   
        } else if (position[i - 1] == -1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10  
          } else if (day_count[i-1] == holding_period) {
            # Close position
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- 11
          }
          
          # Current position, already long   
        } else if (position[i - 1] == 1) {
          # Close long position
          position[i] <- 0
          trade_flag[i] <- -11
          day_count[i] <- 0
        }
      }
      
      ### Ratio is above SD ###
      else if (SD_flag[i-1] == 2) {
        
        # No position, enter long
        if (position[i - 1] == 0) {
          position[i] <- 1
          trade_flag[i] <- 1
          day_count[i] <- 1
          
          # Current position, already long
        } else if (position[i - 1] == 1)  {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- -11
          }
          
          # Current position, already short 
        } else if (position[i - 1] == 1) { 
          # Close short position 
          position[i] <- 0
          trade_flag[i] <- 11
          day_count[i] <- 0
        }
      }
      
      ### Ratio is between SDs ###
      else {
        
        # No positon
        if (position[i-1] == 0) {
          # Do nothing
          position[i] <- position[i-1]
          day_count[i] <- 0
          
          # Position is short
        } else if (position[i - 1] == -1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- 11
          }
          
          # Position is long
        } else if (position[i - 1] == 1) {
          # Day count below 10
          if (day_count[i-1] < holding_period) {
            # Do nothing
            position[i] <- position[i - 1]
            day_count[i] <- day_count[i-1] + 1
            # Day count = to 10
          } else if (day_count[i-1] == holding_period) {
            position[i] <- 0
            day_count[i] <- 0
            trade_flag[i] <- -11
          }
        }
      }
      
      
      ### Add some information
      # When trade is opened
      if (trade_flag[i] == 1 || trade_flag[i] == -1) {
        # Store entry price
        entry_price[i] <- price_ratio[i-1]
        
        # When trade is closed
      } else if (trade_flag[i] == 11 || trade_flag[i] == -11) {
        exit_price[i] <- price_ratio[i-1]
        
        # Current positon, retain entry price
      } else if (trade_flag[i] == 0  | (position[i] == 1 || position[i] == -1)) {
        entry_price[i] <- entry_price[i-1]
        
        # No trade, no positon
      } else 
        entry_price[i] <- 0
    }
  }
  
  message <- sprintf("%s/%s: done - naive", Stock_A, Stock_B)
  print(message)
  
  ### PL and equity curve ###
  # Merge all columns into the output xts object, including the SD_flag
  xts_output <- merge(xts_obj, SD_flag, position, trade_flag, accept_flag, day_count, entry_price, exit_price, Value_position, closed_equity, equity_curve)
  colnames(xts_output)[10:19] <- c("SD_flag", "position", "trade_flag", "accept_flag", "day_count", "entry_price", "exit_price", "Value_position", "closed_equity", "equity_curve")
  
  ## Calculate the equity curve
  # Start at the trade_size amount
  xts_output[1, "equity_curve"] <- trade_size
  xts_output[1, "closed_equity"] <- trade_size
  
  for (i in 2:nrow(xts_output)) {
    tmp <- as.numeric(xts_output[i ,"day_count"])
    # A position is opened, calculate equity curve
    if (xts_output[i, "position"] == 1) {
      xts_output[i, "Value_position"] <- (as.numeric(xts_output[i, 2]) / as.numeric(xts_output[i-tmp, 2]) -1) * trade_size - (as.numeric(xts_output[i, 3] / as.numeric(xts_output[i-tmp, 3])) -1) * trade_size 
      xts_output[i, "equity_curve"] <- as.numeric(xts_output[i-as.numeric(tmp), "equity_curve"]) + as.numeric(xts_output[i, "Value_position"])
    } else if (xts_output[i, "position"] == -1) {
      xts_output[i, "Value_position"] <- (-1*((as.numeric(xts_output[i, 2]) / as.numeric(xts_output[i-tmp, 2])) -1)) * trade_size + ((as.numeric(xts_output[i, 3]) / as.numeric(xts_output[i-tmp, 3])) -1) * trade_size
      xts_output[i, "equity_curve"] <- as.numeric(xts_output[i-as.numeric(tmp), "equity_curve"]) + as.numeric(xts_output[i, "Value_position"])
    } else if ((xts_output[i, "position"] == 0)) {
      xts_output[i, "Value_position"] <- 0
      xts_output[i, "equity_curve"] <- xts_output[i-1, "equity_curve"]
    }
  }
  
  # Calculate the close equity
  for (i in 2:nrow(xts_output)) {
    if (trade_flag[i] == 11 || trade_flag[i] == -11) {
      xts_output[i, "closed_equity"] <- xts_output[i-1, "equity_curve"]
    } else {
      xts_output[i, "closed_equity"] <- xts_output[i-1, "closed_equity"]
    }
  }
  
  return(xts_output)  
}

