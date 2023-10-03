f_trading <- function(xts_obj, holding_period, df_clusters_yearly) {
  ### Cette fonction applique la stratégie de trading sur le training et testing set.
  ### Elle crée le backtest.
  
  #  Inputs
  #   xts_obj: [xts_object] Objet qui contient un ratio de volatilités avec les indicateurs requis.
  #   holding_period: [scalar] Nombre de jours dans la trade. 
  #   df_clusters_yearly: [data.frame] (N x C) Dataframe qui contient les clusters à chaque année.
  
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
  price_ratio <- xts_obj[,"price_ratio"]
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
  PL_curve <- xts_obj[, 1]*0        # Current return of current trade
  closed_equity <- xts_obj[, 1]*0
  equity_curve <- xts_obj[, 1]*0
  
  # Start at index 2
  for (i in 2:nrow(xts_obj)) {
    # Store the current year -1 (will be needed to check trading cluster)
    previous_year <- as.character(as.numeric(format(index(xts_obj[i, ]), "%Y")) - 1)

    ### Ratio is above SD ###    
    if (!is.na(SD_flag[i-1])) {
      if (SD_flag[i-1] == 2) {
        # Check if the pair is available in the trading cluster
        if (!(Stock_A %in% df_clusters_yearly[[previous_year]]) || 
            !(Stock_B %in% df_clusters_yearly[[previous_year]])) {
          next
        }
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
      else if (SD_flag[i-1] == -2) {
        # Check if the pair is available in the trading cluster
        if (!(Stock_A %in% df_clusters_yearly[[previous_year]]) || 
            !(Stock_B %in% df_clusters_yearly[[previous_year]])) {
          next
        }
        
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
      else if (SD_flag[i-1] == 0) {
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
      # When trade is opened
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
  
  ### PL and equity curve ###
  # Merge all columns into the output xts object, including the SD_flag
  xts_output <- merge(xts_obj, SD_flag, position, trade_flag, day_count, entry_price, exit_price, PL_curve, closed_equity, equity_curve)
  colnames(xts_output)[8:16] <- c("SD_flag", "position", "trade_flag", "day_count", "entry_price", "exit_price", "PL_curve", "closed_equity", "equity_curve")
  
  # Start at 1$
  xts_output[1, "equity_curve"] <- 1
  xts_output[1, "closed_equity"] <- 1
  for (i in 2:nrow(xts_obj)) {
    # Open long position, calculate current return
    if (xts_output[i, "position"] == 1) {
      xts_output[i, "PL_curve"] <- (as.numeric(xts_output[i, "price_ratio"]) - as.numeric(xts_output[i, "entry_price"]))/as.numeric(xts_output[i, "entry_price"])
    # Open short position, calculate current return
    } else if (xts_output[i, "position"] == -1) { 
      xts_output[i, "PL_curve"] <- (as.numeric(xts_output[i, "entry_price"]) - as.numeric(xts_output[i, "price_ratio"]))/as.numeric(xts_output[i, "entry_price"])
    # No position
    } else if ((xts_output[i, "position"] == 0)) {
      xts_output[i, "PL_curve"] <- 0
    }
  }
  
  # Calculate the equity curve
  for (i in 2:nrow(xts_output)) {
    # No trades were closed, closed equity remains the same
    if (xts_output[i, "exit_price"] == 0) {
      xts_output[i, "closed_equity"] <-  as.numeric(xts_output[i-1, "closed_equity"])
    } else if (xts_output[i, "exit_price"] != 0) {
    # Trade was closed, adjust the closed equity
      xts_output[i, "closed_equity"] <-  as.numeric(xts_output[i-1, "closed_equity"]) * (1+as.numeric(xts_output[i-1, "PL_curve"]))
    }
    # Equity curve is the closed equity and the open PL
    xts_output[i, "equity_curve"] <- as.numeric(xts_output[i, "closed_equity"]) * (1+as.numeric(xts_output[i, "PL_curve"]))
  }
  
  return(xts_output)
}