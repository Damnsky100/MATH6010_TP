### Indicators
# Generate the indicators to be used by the trading algorithm
list_ratios_technicals <- f_compute_ratios_technicals(stocks_symbols, 100, 2, df_vol, df_price)

### Training set
# Apply the training algorithm to the training set
# The function will also store the trade log of the back test for the classification algorithm
f_run_strategy(start_date="2008-01-01", end_date="2017-12-31", list_ratios_technicals, strategy="naive")

### Backtest
# Apply the trading algorithm to the testing set
# The function will store the trade log for every strategy and their correspondin equity curve.
f_run_strategy(start_date="2018-01-01", end_date="2022-12-31", list_ratios_technicals, strategy="naive")
f_run_strategy(start_date="2018-01-01", end_date="2022-12-31", list_ratios_technicals, strategy="regression")
#f_run_strategy(start_date="2018-01-01", end_date="2022-12-31", list_ratios_technicals, strategy="classification")
#f_run_strategy(start_date="2018-01-01", end_date="2022-12-31", list_ratios_technicals, strategy="both")


### Generate the reports
# Retrieve the information from each strategy
equity_curves <- f_load_equity_curves()
all_trades <- f_load_trades()

# Generate the risk reports
risk_metrics <- f_risk_metrics(equity_curves, all_trades)
print(risk_metrics["Portfolio_statistics"])
print(risk_metrics["Trades_statistics"])

# Generate the plots