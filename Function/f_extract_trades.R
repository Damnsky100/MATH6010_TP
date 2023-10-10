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

