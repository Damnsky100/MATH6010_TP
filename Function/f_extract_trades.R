f_extract_trades <- function(xts_object){
  # empty list to store
  trades <- list()
  current_trade <- NULL
  # look row by row for trades
  for(i in 1:nrow(xts_object)){
    flag <- xts_object[i, "trade_flag"]
    if(flag %in% c(1, -1)){
      current_trade <- list(start_date=index(xts_object)[i], entry_price=xts_object[i, "entry_price"], direction=flag)
    } else if(flag %in% c(-11, 11) && !is.null(current_trade)){
      current_trade$end_date <- index(xts_object)[i]
      current_trade$exit_price <- xts_object[i, "exit_price"]
      current_trade$return <- xts_object[i-1, "PL_curve"]
      current_trade$profitable <- ifelse(current_trade$return > 0, 1, 0)
      current_trade$days <- xts_object[i-1, "day_count"]
      current_trade$ratio <- paste(sub("_Price", "", colnames(xts_object)[2]), 
                                   sub("_Price", "", colnames(xts_object)[3]), 
                                   sep="/")
      trades <- append(trades, list(current_trade))
      current_trade <- NULL
    }
  }
  output <- do.call(rbind, lapply(trades, as.data.frame))
}

