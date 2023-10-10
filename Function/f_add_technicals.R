
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