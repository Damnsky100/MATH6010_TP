### Clustering Algorithm
# Retrieve and clean the data
f_clean_data_clustering()

# Run the clustering algorithm (K-mean, 10) and,
# Store the tickers
stocks_symbols <- f_cluster(10)

# Retrieve stock information from quantmod
priceData <- get_stock_data(stocks_symbols)

summary(priceData)

# Save the 'data' object to an RData file
save(priceData, file = here("Clean_Data", "stock_data.rda"))

# Retrieve the volatility data
df_vol <- f_get_vol_data()

# Retrieve price data
df_price <- f_get_price_data()  
