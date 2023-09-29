f_trading <- function(xts_obj, holding_period) {
  SD_flag <- ifelse(xts_obj[, "vol_ratio"] > xts_obj[,"up_band"], 2,
                    ifelse(xts_obj[, "vol_ratio"] < xts_obj[,"lo_band"], -2, 0))
  price_ratio <- xts_obj[,"price_ratio"]
  # Create empty columns for the current position
  position <- xts_obj[, 1]*0
  trade_flag <- xts_obj[, 1]*0
  entry_price <- xts_obj[, 1]*0
  day_count <- xts_obj[, 1]*0
  exit_price <- xts_obj[, 1]*0
  ### for PL construction
  current_price <- xts_obj[, 1]*0  # current price of position
  PL_curve <- xts_obj[, 1]*0
  closed_equity <- xts_obj[, 1]*0
  equity_curve <- xts_obj[, 1]*0
  
  for (i in 2:nrow(xts_obj)) { 
    # Ratio is above SD
    if (SD_flag[i-1] == 2) {
      if (position[i-1] == 0) {
        # Sell the asset
        position[i] <- -1
        trade_flag[i] <- -1
        day_count[i] <- 1
      } else if (position[i - 1] == -1) {
        if (day_count[i-1] < holding_period) {
          # Do nothing
          position[i] <- position[i - 1]
          day_count[i] <- day_count[i-1] + 1
        } else if (day_count[i-1] == holding_period) {
          position[i] <- 0
          day_count[i] <- 0
          trade_flag[i] <- 11
        }
      } else if (position[i - 1] == 1) {
        # Close long position
        position[i] <- 0
        trade_flag[i] <- -11
        day_count[i] <- 0
      }
    }
    # Ratio is below SD
    else if (SD_flag[i-1] == -2) {
      if (position[i - 1] == 0) {
        # Buy the asset
        position[i] <- 1
        trade_flag[i] <- 1
        day_count[i] <- 1
      } else if (position[i - 1] == 1)  {
        if (day_count[i-1] < holding_period) {
          # Do nothing
          position[i] <- position[i - 1]
          day_count[i] <- day_count[i-1] + 1
        } else if (day_count[i-1] == holding_period) {
          position[i] <- 0
          day_count[i] <- 0
          trade_flag[i] <- -11
        }
      } else if (position[i - 1] == 1) { 
        # Close long position 
        position[i] <- 0
        trade_flag[i] <- 11
        day_count[i] <- 0
      }
    }
    # Ratio is between SDs
    else if (SD_flag[i-1] == 0) {
      if (position[i-1] == 0) {
        position[i] <- position[i-1]
        day_count[i] <- 0
      } else if (position[i - 1] == -1) {
        if (day_count[i-1] < holding_period) {
          # Do nothing
          position[i] <- position[i - 1]
          day_count[i] <- day_count[i-1] + 1
        } else if (day_count[i-1] == holding_period) {
          position[i] <- 0
          day_count[i] <- 0
          trade_flag[i] <- 11
        }
      } else if (position[i - 1] == 1) {
        if (day_count[i-1] < holding_period) {
          # Do nothing
          position[i] <- position[i - 1]
          day_count[i] <- day_count[i-1] + 1
        } else if (day_count[i-1] == holding_period) {
          position[i] <- 0
          day_count[i] <- 0
          trade_flag[i] <- -11
        }
      }
    }
    if (trade_flag[i] == 1 || trade_flag[i] == -1) {
      entry_price[i] <- price_ratio[i-1]
    } else if (trade_flag[i] == 11 || trade_flag[i] == -11) {
      exit_price[i] <- price_ratio[i-1] 
    } else if (trade_flag[i] == 0  | (position[i] == 1 || position[i] == -1)) {
      entry_price[i] <- entry_price[i-1]
    } else 
      entry_price[i] <- 0
  }
  
  # Merge all columns into the output xts object, including SD_flag
  xts_output <- merge(xts_obj, SD_flag, position, trade_flag, day_count, entry_price, exit_price, PL_curve, closed_equity, equity_curve)
  colnames(xts_output)[8:16] <- c("SD_flag", "position", "trade_flag", "day_count", "entry_price", "exit_price", "PL_curve", "closed_equity", "equity_curve")
  
  xts_output[1, "equity_curve"] <- 1
  xts_output[1, "closed_equity"] <- 1
  for (i in 2:nrow(xts_obj)) {
    if (xts_output[i, "position"] == 1) {
      xts_output[i, "PL_curve"] <- (as.numeric(xts_output[i, "price_ratio"]) - as.numeric(xts_output[i, "entry_price"]))/as.numeric(xts_output[i, "entry_price"])
    } else if (xts_output[i, "position"] == -1) { 
      xts_output[i, "PL_curve"] <- (as.numeric(xts_output[i, "entry_price"]) - as.numeric(xts_output[i, "price_ratio"]))/as.numeric(xts_output[i, "entry_price"])  
    } else if ((xts_output[i, "position"] == 0)) {
      xts_output[i, "PL_curve"] <- 0
    }
  }
  
  for (i in 2:nrow(xts_output)) {
    if (xts_output[i, "exit_price"] == 0) {
      xts_output[i, "closed_equity"] <-  as.numeric(xts_output[i-1, "closed_equity"])
    } else if (xts_output[i, "exit_price"] != 0) {
      xts_output[i, "closed_equity"] <-  as.numeric(xts_output[i-1, "closed_equity"]) * (1+as.numeric(xts_output[i-1, "PL_curve"]))
    }
    xts_output[i, "equity_curve"] <- as.numeric(xts_output[i, "closed_equity"]) * (1+as.numeric(xts_output[i, "PL_curve"]))
  }
  
  # Retrieve only the trades
  #xts_trades <- xts_output[!is.na(xts_output$trade_flag),]
  
  return(xts_output)
}
