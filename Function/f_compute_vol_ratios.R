library("xts")
library("here")

f_compute_vol_ratios <- function(stock_pairs, list_vols, list_prices) {
  ### Cette fonction crée les ratios de volatilités ainsi que ratio de prix.
  
  #  Inputs
  #   stock_pairs: [vector] (2 x 1) vecteur contenant les pairs de stocks.
  #   list_vols: [list] (length of N) liste contenant les de données journalière 
  #                 de volatilité des constituants du Dow Jones choisit.
  #   list_prices: [list] (length of N) liste contenant les de données journalière 
  #                 de prix des constituants du Dow Jones choisit.
  
  #  OUTPUTS
  #   merged_data: [list] (length of N) liste contenant les ratios journalier de chacune des paires.
  
  # Store the daily implied vol. of both stock in the pair
  vol_A <- list_vols[[stock_pairs[1]]]
  vol_B <- list_vols[[stock_pairs[2]]]
  
  #Create ratio 
  ratio <- vol_A / vol_B
  colnames(ratio) <- paste("vol_ratio")
  
  # Retrieve the prices from the list_price
  price_A <- list_prices[[stock_pairs[1]]]
  price_B <- list_prices[[stock_pairs[2]]]
  price_ratio <- price_A / price_B
  # Rename for clarity
  colnames(price_A) <- paste(stock_pairs[1], "_Price", sep="")
  colnames(price_B) <- paste(stock_pairs[2], "_Price", sep="")
  colnames(price_ratio) <- paste("price_ratio")
  
  # Merge ratio with the prices
  merged_data <- merge.xts(ratio, price_A, price_B, price_ratio)
  
  return(merged_data)
}