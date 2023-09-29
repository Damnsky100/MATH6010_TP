library("xts")
library("here")
library("zoo")
library("quantmod")
library("TTR")
source(here("Function", "f_get_vol_data.R"))
source(here("Function", "f_clean_vol_data.R"))
source(here("Function", "f_get_price_data.R"))
source(here("Function", "f_clean_price_data.R"))
source(here("Function", "f_add_technicals.R"))
source(here("Function", "f_compute_vol_ratios.R"))
source(here("Function", "f_trading.R"))

####### DATA ##########

# Volatility data
df_vol <- f_get_vol_data()
# vector with the unique tickers
stocks_symbols <- unique(df_vol$ticker)  
# Create an empty list with names based on ticker symbols
list_vols <- vector("list", length(stocks_symbols))
# Apply the clean_vol_data function to every ticker
list_vols <- lapply(stocks_symbols, function(ticker) f_clean_vol_data(df_vol, 30, ticker))
names(list_vols) <- stocks_symbols

# Price data
df_price <- f_get_price_data()
# Apply the clean_vol_data function to every ticker
list_prices <- lapply(stocks_symbols, function(ticker) f_clean_price_data(df_price, ticker))
names(list_prices) <- stocks_symbols


##### CREATE RATIOS ######

# Determine all combos, keep them named
stock_pairs <- combn(stocks_symbols, 2)
  
# create and save every vol ratio in a list
list_ratios <- lapply(seq_len(ncol(stock_pairs)), function(i) f_compute_vol_ratios(stock_pairs[, i], list_vols, list_prices))
# Rename every list element with the pair name
names(list_ratios) <- apply(stock_pairs, 2, function(combos) paste(combos, collapse = "/"))

# Apply function to add technicals to every list element
list_ratios_technicals <- lapply(list_ratios, function(i) f_add_technicals(i, 100, 2))

####
# Chose a starting date 
start_date <- as.Date("2001-01-01")
end_date <- as.Date("2022-12-30")

# Function to crop every list element for what we need
crop_data <- function(xts_obj, start_date, end_date) {
  tmp <- window(xts_obj, start = start_date, end = end_date)
  return(tmp)
}
# Crop the volatilities for the dates we want
list_ratios_technicals <- lapply(list_ratios_technicals, crop_data, start_date = start_date, end_date = end_date)




######################################################################
### Trading


list_trading <- lapply(list_ratios_technicals, function(i) f_trading(i, 10))



##### PLOTS #####
tmp <- list_trading[[3]]

plot(index(tmp), coredata(tmp[,"equity_curve"]), type = 'l', col = 'black',
     main = "test",
     xlab = "Date", ylab = "Ratio")


### plot to see
plot(index(tmp), coredata(tmp[,"vol_ratio"]), type = 'l', col = 'black',
     main = "test",
     xlab = "Date", ylab = "Ratio")

# Add the technicals
lines(index(tmp), coredata(tmp[,"MA_100"]), col = 'pink')
lines(index(tmp), coredata(tmp[,"up_band"]), col = 'purple')
lines(index(tmp), coredata(tmp[,"lo_band"]), col = 'purple')

# Plot points with different colors based on their value in the 'trade_flag' column
points(index(tmp[tmp[, "trade_flag"] == "BUY", ]), tmp[tmp[, "trade_flag"] == "BUY", "vol_ratio"], col = 'red', pch = 19)
points(index(tmp[tmp[, "trade_flag"] == "SELL", ]), tmp[tmp[, "trade_flag"] == "SELL", "vol_ratio"], col = 'green', pch = 19)
points(index(tmp[tmp[, "trade_flag"] == "CLOSE SHORT", ]), tmp[tmp[, "trade_flag"] == "CLOSE SHORT", "vol_ratio"], col = 'blue', pch = 19)
points(index(tmp[tmp[, "trade_flag"] == "CLOSE LONG", ]), tmp[tmp[, "trade_flag"] == "CLOSE LONG", "vol_ratio"], col = 'yellow', pch = 19)

# Add a legend
legend("topleft", legend = c("Buy", "Sell", "Close Short", "Close Long"), col = c('red', 'green', 'blue', 'yellow'), pch = 19)



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











