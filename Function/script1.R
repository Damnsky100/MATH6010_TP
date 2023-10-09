library("xts")
library("here")
library("zoo")
library("quantmod")
library("TTR")
library("openxlsx")
library("readxl")

source(here("Function", "f_add_technicals.R"))
source(here("Function", "f_compute_vol_ratios.R"))
source(here("Function", "f_trading.R"))
source(here("Function", "f_clean_data.R"))
source(here("Function", "f_cluster.R"))
source(here("Function", "f_extract_trades.R"))


#### Initial Cluster #####  

# Retrieve the cluster which will be used
f_clean_data_clustering()
df_clusters_yearly <- f_cluster(10)

# Store tickers
stocks_symbols <- unique(df_clusters_yearly) 


#### Volatility and Price data #####

df_vol <- f_get_vol_data()              # Volatility
df_price <- f_get_price_data()          # Price 
 
# Apply the clean_vol_data function to the list of tickers
list_vols <- lapply(stocks_symbols, 
                    function(ticker) f_clean_vol_data(df_vol, 30, ticker))
names(list_vols) <- stocks_symbols    # Name the list
# Apply the clean_price_data to the list of tickers
list_prices <- lapply(stocks_symbols, 
                      function(ticker) f_clean_price_data(df_price, ticker))
names(list_prices) <- stocks_symbols # Name the list



##### CREATE THE UNIQUE PAIRS OF STOCKS ######

# Determine all combos and keep them name
# Store all possible pairs in a matrix
combo_matrix<- combn(stocks_symbols, 2)
# Create empty list to store the pairs
all_pairs <- list()
# Iterate through the columns of the combinations matrix
for (i in 1:ncol(combo_matrix)) {
  # Extract the two stocks in the current combination
  stockA <- combo_matrix[1, i]
  stockB <- combo_matrix[2, i]
  
  # Create the combination string and add it to the list
  combo <- paste(stockA, "/", stockB, sep = "")
  all_pairs[[i]] <- combo
}


##### CREATE THE RATIOS ######

# Create and save every volatility and price ratio
list_volatility_ratios <- lapply(all_pairs, 
                                 f_compute_vol_ratios, 
                                 list_vols, list_prices)

# Rename every list element with the pair name
names(list_volatility_ratios) <- all_pairs

# Clean the xts_objects within the list
# Some stocks did not exits at certain point in time and it creates NAs
list_ratios <- lapply(list_volatility_ratios, function(x) {
  # For each xts object, remove rows with NAs
  return(na.omit(x))
})

# Apply function to add technicals 
list_ratios_technicals <- lapply(list_ratios, 
                                 function(i) f_add_technicals(i, 100, 2))



#### add predictions to the technicals ###

df <- load(here("Clean_Data", "pricePrediction.rda"))
df_price_prediction <- get(df[1])
z <- unlist(all_pairs)
df_price_ratio_predict <- f_compute_predict_price_ratios(z, df_price_prediction)


# Iterate over the list and join the columns
list_ratios_technicals <- lapply(names(list_ratios_technicals), function(x) {
  # Check if the column exists in the dataframe
  if(x %in% colnames(df_price_ratio_predict)) {
    # Extract column from dataframe based on xts object's name
    col_data <- df_price_ratio_predict[, x, drop=FALSE]
    
    # Convert the column to xts
    price_prediction <- xts(col_data[, x], order.by=as.Date(rownames(col_data)))
    
    # Merge with the xts object
    merged_xts <- merge(list_ratios_technicals[[x]], price_prediction)
    
    return(merged_xts)
  } else {
    return(list_ratios_technicals[[x]])  # Return original xts if no matching column
  }
})

list_ratios_technicals <- lapply(list_ratios_technicals, na.locf)

# Chose a starting date 
#start_date <- as.Date("2008-01-01")
start_date <- as.Date("2018-01-01")
end_date <- as.Date("2022-12-31")

# Function to crop every list element for what we need
crop_data <- function(xts_obj, start_date, end_date) {
  tmp <- window(xts_obj, start = start_date, end = end_date)
  return(tmp)
}

# Crop the volatilities for the dates we want
list_ratios_technicals <- lapply(list_ratios_technicals, 
                                 crop_data, 
                                 start_date, end_date)


#### Trading ####

# Create the backtest 
list_trading <- lapply(list_ratios_technicals, 
                       function(i) f_trading(i, 10, (100/45), 0.05))

# Retrieve all the trades
list_trades <- lapply(list_trading, 
                      function(i) f_extract_trades(i))
# Merge
df_all_trades <- do.call(rbind, list_trades)
colnames(df_all_trades)[11] <- "profitable"

# Save Naive strategy results
list_trading_naive <- list_trading
write.xlsx(list_trading_naive, here("output", "list_trading_outofsample_naive.xlsx"))
save(list_trading_naive, file = here('Clean_Data', "list_trading_outofsample_naive.rda"))
# Save trades
write.xlsx(df_all_trades, here("output", "df_all_trades_outofsample_naive.xlsx"))
save(df_all_trades, file = here('Clean_Data', "df_all_trades_outofsample_naive.rda"))

### Create equity curve ###

# Retrieve all the equity curves
equity_curves <- lapply(list_trading, function(x) x$equity_curve)
# Sum together to get porftolio value
total_equity_curve <- Reduce("+", equity_curves)





#### PLOTS #####
### plot to see equity curve
plot(index(total_equity_curve), coredata(total_equity_curve[,"equity_curve"]), 
     type = 'l', col = 'black', main = "test")


# Convert list of xts objects to one multi-column zoo object
multi_col_zoo <- do.call(merge, equity_curves)

# Generate 28 distinct colors
colors <- rainbow(ncol(multi_col_zoo))

# Plot using plot.zoo
plot(multi_col_zoo, plot.type="single", col=colors, lwd=2, screen=1)



naive_equity_curves <- equity_curves
naive_total_equity_curve <- total_equity_curve

reg_equity_curves 
reg_total_equity_curve 

my_xts <- merge(naive_total_equity_curve, reg_total_equity_curve)
plot(index(my_xts), my_xts[, 1], type="l", col="blue", 
     ylim=c(min(my_xts), max(my_xts)), 
     xlab="Date", ylab="Value", main="Equity Curves")

# Add the second column
lines(index(my_xts), my_xts[, 2], col="red")

# Add a horizontal line at the $100 mark
abline(h=100, col="grey", lty=2, lwd=2)

# Add a legend
legend("topleft", legend=c('Naive strat.', 'with price prediction'), 
       col=c("blue", "red"), lty=1, cex=0.8)

##### PLOTS #####

# look vol ratio ( doit inverser les signaux... cest normal)

for (i in 1:length(list_trading)) {
tmp <- list_trading[[i]]

plot(index(tmp), coredata(tmp[,"vol_ratio"]), type = 'l', col = 'black',
     main = "test",
     xlab = "Date", ylab = "Ratio")


# Add the technicals
lines(index(tmp), coredata(tmp[,"MA_100"]), col = 'pink')
lines(index(tmp), coredata(tmp[,"up_band"]), col = 'purple')
lines(index(tmp), coredata(tmp[,"lo_band"]), col = 'purple')

# Plot points with different colors based on their value in the 'trade_flag' column
points(index(tmp[tmp[, "trade_flag"] == -1, ]), tmp[tmp[, "trade_flag"] == -1, "vol_ratio"], col = 'red', pch = 19)
points(index(tmp[tmp[, "trade_flag"] == 1, ]), tmp[tmp[, "trade_flag"] == 1, "vol_ratio"], col = 'green', pch = 19)
points(index(tmp[tmp[, "trade_flag"] == -11, ]), tmp[tmp[, "trade_flag"] == -11, "vol_ratio"], col = 'blue', pch = 19)
points(index(tmp[tmp[, "trade_flag"] == 11, ]), tmp[tmp[, "trade_flag"] == 11, "vol_ratio"], col = 'yellow', pch = 19)

# Add a legend
legend("topleft", legend = c("Buy", "Sell", "Close Short", "Close Long"), col = c('red', 'green', 'blue', 'yellow'), pch = 19)
}

# look on price ratio

for (i in 1:length(list_trading)) {
  tmp <- list_trading[[i]]
  
  plot(index(tmp), coredata(tmp[,"price_ratio"]), type = 'l', col = 'black',
       main = "test",
       xlab = "Date", ylab = "Ratio")
  
  # Plot points with different colors based on their value in the 'trade_flag' column
  points(index(tmp[tmp[, "trade_flag"] == 1, ]), tmp[tmp[, "trade_flag"] == 1, "price_ratio"], col = 'red', pch = 19)
  points(index(tmp[tmp[, "trade_flag"] == -1, ]), tmp[tmp[, "trade_flag"] == -1, "price_ratio"], col = 'green', pch = 19)
  points(index(tmp[tmp[, "trade_flag"] == 11, ]), tmp[tmp[, "trade_flag"] == 11, "price_ratio"], col = 'blue', pch = 19)
  points(index(tmp[tmp[, "trade_flag"] == -11, ]), tmp[tmp[, "trade_flag"] == -11, "price_ratio"], col = 'yellow', pch = 19)
  
  # Add a legend
  legend("topleft", legend = c("Buy", "Sell", "Close Short", "Close Long"), col = c('red', 'green', 'blue', 'yellow'), pch = 19)
}


### plot to see
plot(index(tmp), coredata(tmp[,"price_ratio"]), type = 'l', col = 'black',
     main = "test",
     xlab = "Date", ylab = "Ratio")

# Plot points with different colors based on their value in the 'trade_flag' column
points(index(tmp[tmp[, "trade_flag"] == 1, ]), tmp[tmp[, "trade_flag"] == 1, "price_ratio"], col = 'red', pch = 19)
points(index(tmp[tmp[, "trade_flag"] == -1, ]), tmp[tmp[, "trade_flag"] == -1, "price_ratio"], col = 'green', pch = 19)
points(index(tmp[tmp[, "trade_flag"] == -11, ]), tmp[tmp[, "trade_flag"] == -11, "price_ratio"], col = 'blue', pch = 19)
points(index(tmp[tmp[, "trade_flag"] == 11, ]), tmp[tmp[, "trade_flag"] == 11, "price_ratio"], col = 'yellow', pch = 19)

# Add a legend
legend("topleft", legend = c("Buy", "Sell", "Close Short", "Close Long"), col = c('red', 'green', 'blue', 'yellow'), pch = 19)











