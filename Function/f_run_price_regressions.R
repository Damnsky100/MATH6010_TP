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