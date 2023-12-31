#################################
##### GENERATE REPORTS #########
################################

# Retrieve the equity curves
equity_curves <- f_load_equity_curves()

# Retrieve the trades
all_trades_list<- f_load_trades()

# Generate the risk reports
risk_metrics <- f_risk_metrics(equity_curves, all_trades_list, 0.025)
print(risk_metrics["Portfolio_statistics"])
print(risk_metrics["Trades_statistics"])

# Plots

f_call_plots(equity_curves, all_trades_list)



