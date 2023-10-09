f_risk_metrics <- function(equity_curves) {
  
  
  # Convert the Date column to a date format
  equity_curves$Date <- as.Date(equity_curves$Date, format="%Y%m%d")
  
  # Create the xts object
  all_curves_xts <- xts(equity_curves[, -which(names(equity_curves) == "Date")], order.by=equity_curves$Date)
  
  # Compute logarithmic returns
  all_returns_xts <- diff(log(all_curves_xts))
  
  # Drop the first row since it's NA after computing returns
  all_returns_xts <- all_returns_xts[-1, ]
  
  # Extract the series for naive and regression
  naive <- all_returns_xts[, 1]
  regression <- all_returns_xts[, 2]
  
  # Function to calculate metrics for a given xts object
  calculate_metrics <- function(returns) {
    
    total_ret = round(Return.cumulative(returns) * 100, 2)
    annual_sd = round(StdDev(returns) * sqrt(252) * 100, 2)  
    Sharpe_ratio = round(SharpeRatio.annualized(returns, Rf=0, scale=252), 2)
    Sortino_ratio = round(SortinoRatio(returns, MAR=0, method="moment") * sqrt(252), 2)
    Max_drawdown = round(maxDrawdown(returns) * 100, 2)
    
    metrics <- c(TotalReturn = total_ret,
                 AnnualStandardDeviation = annual_sd,
                 SharpeRatio = Sharpe_ratio,
                 SortinoRatio = Sortino_ratio,
                 MaxDrawdown = Max_drawdown)
    
    return(metrics)
  }
  
  naive_metrics <- calculate_metrics(naive)
  regression_metrics <- calculate_metrics(regression)
  
  # Print the metrics
  print(naive_metrics)
  print(regression_metrics)
  
  naive <- all_curves_xts[,1]
  regression <- all_curves_xts[,2]
  
  plot(index(naive), naive, type="l", col="blue", 
      ylim=c(min(naive), max(naive)), 
      xlab="Date", ylab="Value", main="Equity Curves")

  # Add the second column
  lines(index(regression), regression, col="red")

  # Add a horizontal line at the $100 mark
  abline(h=100, col="grey", lty=2, lwd=2)

  # Add a legend
  legend("topleft", legend=c('Naive strat.', 'with price prediction'), 
        col=c("blue", "red"), lty=1, cex=0.8)


}





