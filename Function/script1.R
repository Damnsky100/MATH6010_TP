library("xts")
library("here")
library("zoo")
library("quantmod")
library("TTR")
library("openxlsx")
source(here("Function", "f_get_vol_data.R"))
source(here("Function", "f_clean_vol_data.R"))
source(here("Function", "f_get_price_data.R"))
source(here("Function", "f_clean_price_data.R"))
source(here("Function", "f_add_technicals.R"))
source(here("Function", "f_compute_vol_ratios.R"))
source(here("Function", "f_trading.R"))
source(here("Function", "f_clean_data.R"))
source(here("Function", "f_cluster_DOW.R"))
source(here("Function", "f_extract_trades.R"))

#### Initial Cluster #####
# Retrieve the cluster which will be used
f_clean_data_clustering()
df_clusters_yearly <- f_cluster_DOW(10)

# Unlist the dataframe to find all the unique stocks
df_unlist <- as.vector(unlist(df_clusters_yearly))
unique_stocks <- unique(df_unlist)


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




###### COMBOS  ######

get_pairs <- function(stocks) {
  combn(stocks, 2)
}

# Create a matrix to store pairs for each year
all_pairs <- do.call(cbind, lapply(df_clusters_yearly, function(col) get_pairs(unique(na.omit(col)))))

# Remove duplicate columns (unique pairs)
unique_pairs_overall <- all_pairs[, !duplicated(t(all_pairs))]

print(unique_pairs_overall)

###########################3


# create and save every vol ratio in a list
list_ratios <- lapply(seq_len(ncol(stock_pairs)), function(i) f_compute_vol_ratios(stock_pairs[, i], list_vols, list_prices))
# Rename every list element with the pair name
names(list_ratios) <- apply(stock_pairs, 2, function(combos) paste(combos, collapse = "/"))

# Clean the xts_objects within the list
# Some stocks did not exits at certain point in time and it creates NAs
list_ratios <- lapply(list_ratios, function(x) {
  # For each xts object, remove rows where any column has NA
  return(na.omit(x))
})


# Apply function to add technicals to every list element
list_ratios_technicals <- lapply(list_ratios, function(i) f_add_technicals(i, 100, 2))

####
# Chose a starting date 
start_date <- as.Date("2008-01-01")
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
 
list_trading <- lapply(list_ratios_technicals, function(i) f_trading(i, 10, df_clusters_yearly))

# get trades
list_trades <- lapply(list_trading, function(i) f_extract_trades(i))
# filer the null
list_df_trades <- list_trades[sapply(list_trades, function(df) !is.null(df))]

# merge
df_all_trades <- lapply(names(list_df_trades), function(df_name) {
  df <- list_df_trades[[df_name]]
  df$source <- df_name
  return(df)
})

# Convert the list output from lapply back to a list (if it's not already)
df_all_trades <- as.list(df_all_trades)

# Combine the dataframes into one
combined_df <- do.call(rbind, df_all_trades)
#### clean up
# Remove the 'source' column
combined_df$source <- NULL
# Rename 
colnames(combined_df)[6] <- "return"
colnames(combined_df)[7] <- "profitable"

write.xlsx(combined_df, here("output", "trades_small.xlsx"))



##### PLOTS #####

for (i in 1:length(list_trading)) {
tmp <- list_trading[[i]]

plot(index(tmp), coredata(tmp[,"equity_curve"]), type = 'l', col = 'black',
     main = "test",
     xlab = "Date", ylab = "Ratio")
}

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











