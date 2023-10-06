library("xts")
library("here")
library("zoo")
library("quantmod")
library("TTR")
library("openxlsx")

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

# Retrieve the clusters we used
# data <- load(here("Raw_Data", "clusters.rda"))
# df_clusters_yearly <- get(data[1])
# df_clusters_yearly <- df_clusters_yearly[,1]    # Only use year 2007
# stocks_symbols <- unique(df_clusters_yearly) # Store tickers

# NO DELL, NO GOOG
stocks_symbols <- c("AMAT", "DISH", "GILD", "MSFT", "QCOM", "QRTEA", "RYAAY", "VRTX")

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

# Chose a starting date 
start_date <- as.Date("2008-01-01")
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
                       function(i) f_trading(i, 10))

# Retrieve all the trades
list_trades <- lapply(list_trading, 
                      function(i) f_extract_trades(i))

# Merge
df_all_trades <- do.call(rbind, list_trades)

# Save trades
write.xlsx(df_all_trades, here("output", "trades_nasdaq.xlsx"))


### Create equity curve ###

# Retrieve all the equity curves
equity_curves <- lapply(list_trading, function(x) x$equity_curve)
# Sum together to get porftolio value
total_equity_curve <- Reduce("+", equity_curves)

# Save
write.xlsx(total_equity_curve, here("output", "Equity_curve.xlsx"))




#### PLOTS #####
### plot to see equity curve
plot(index(total_equity_curve), coredata(total_equity_curve[,"equity_curve"]), 
     type = 'l', col = 'black', main = "test")


# Convert list of xts objects to one multi-column zoo object
multi_col_zoo <- do.call(merge, my_list)

# Generate 28 distinct colors
colors <- rainbow(ncol(multi_col_zoo))

# Plot using plot.zoo
plot(multi_col_zoo, plot.type="single", col=colors, lwd=2, screen=1)




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











