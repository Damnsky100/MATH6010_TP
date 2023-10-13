f_load_trades <- function() {
  ### Cette fonction retrouve les transactions qui sont sauvegardées en .RDA et les combines ensembles.
  ### Elle ne contient pas d'input puisqu'elle retrouve les fichier .rda dans le dossier "output".  
  
  #  OUTPUTS
  #   output: [liste] Une liste ou chaque element est tout les trades de chaque stratégie.
  
  # Create empty list
  all_trades_list <- list()
   
  # Naive Strat
  loaded_name <- load(here("output", "df_all_trades_naive_2018-01-01_to_2022-12-31.rda"))
  Naive_trades <- get(loaded_name)
  all_trades_list$Naive_trades <- Naive_trades
  
  # Regression Strat
  loaded_name <- load(here("output", "df_all_trades_regression_2018-01-01_to_2022-12-31.rda"))
  Regression_trades <- get(loaded_name)
  all_trades_list$Regression_trades <- Regression_trades
  
  # Classification Strat
  loaded_name <- load(here("output", "df_all_trades_classification_2018-01-01_to_2022-12-31.rda"))
  Classification_trades <- get(loaded_name)
  all_trades_list$Classification_trades <- Classification_trades
  
  # Both Strat
  Ensemble_trades <- load(here("output", "df_all_trades_both_2018-01-01_to_2022-12-31.rda"))
  Both_trades <- get(loaded_name)
  all_trades_list$Both_trades <- Both_trades
  
  return(all_trades_list)
}

f_load_equity_curves <- function() {
  ### Cette fonction retrouve les courbes d'équités qui sont sauvegardées en .RDA et les combines ensembles.
  ### Elle ne contient pas d'input puisqu'elle retrouve les fichier .rda dans le dossier "output".  
  
  #  OUTPUTS
  #   output: [data.frame] (N x C) Data.frame contenant toutes les courbes d'équité.
  
  # Naive Strat
  loaded_name <- load(here("output", "Equity_curve_naive_2018-01-01_to_2022-12-31.rda"))
  naive_strat <- get(loaded_name)
  
  # Regression Strat
  loaded_name <- load(here("output", "Equity_curve_regression_2018-01-01_to_2022-12-31.rda"))
  regression_strat <- get(loaded_name)
  
  # Classification Strat
  loaded_name <- load(here("output", "Equity_curve_classification_2018-01-01_to_2022-12-31.rda"))
  classification_strat <- get(loaded_name)
  
  # Both Strat
  loaded_name <- load(here("output", "Equity_curve_both_2018-01-01_to_2022-12-31.rda"))
  both_strat <- get(loaded_name)

  
  # Combine them together
  all_curves <- cbind(naive_strat, regression_strat[1], classification_strat[1], both_strat[1])
  names(all_curves) <- c("Naive", "Date" , "Régression", "Classification", "Ensemble")
  all_curves <- all_curves[, c(2, 1, 3, 4, 5)]
  return(all_curves)
}

f_risk_metrics <- function(equity_curves, all_trades) {
  ### Cette fonction calcule les mesures de risques et retrouvent des informations pertinentes.
  
  # Inputs
  #   equity_curves: [Data.frame] Dataframe contenant toutes les courbes d'équités.
  #   all_trades: [List] Liste contenant toute les trades pour chaque paires.
  
  # Convert the Date column
  equity_curves$Date <- as.Date(equity_curves$Date, format="%Y%m%d")
  
  # Create xts object
  all_curves_xts <- xts(equity_curves[, -which(names(equity_curves) == "Date")], order.by=equity_curves$Date)
  
  # Compute log rets
  all_returns_xts <- diff(log(all_curves_xts))
  all_returns_xts <- all_returns_xts[-1, ]
  
  # Extract the series
  Naive <- all_returns_xts[, 1]
  Regression <- all_returns_xts[, 2]
  Classification <- all_returns_xts[, 3]
  Ensemble <- all_returns_xts[, 4]
  
  # Function to calculate risk metrics
  calculate_metrics <- function(returns) {
    
    total_ret <- round(Return.cumulative(returns) * 100, 2)
    annual_sd <- round(StdDev(returns) * sqrt(252) * 100, 2)  
    Sharpe_ratio <- round(SharpeRatio.annualized(returns, Rf=0, scale=252), 2)
    Sortino_ratio <- round(SortinoRatio(returns, MAR=0, method="moment") * sqrt(252), 2)
    Max_drawdown <- round(maxDrawdown(returns) * 100, 2)
    Var_95 <- round(quantile(returns, probs = 0.05), 4)*100
    ES_95 <- round(ES(returns, p = 0.05), 4)*100
    
    # Start at
    starting_value <- 100
    # End at
    ending_value <- tail(returns$Value_position, n = 1)
    
    # Calculate excess return
    excess_return <- ending_value - starting_value
    
    # Combine 
    metrics <- cbind(
      c(TotalReturn = total_ret,
        AnnualStandardDeviation = annual_sd,
        SharpeRatio = Sharpe_ratio,
        SortinoRatio = Sortino_ratio,
        MaxDrawdown = Max_drawdown,
        PortfolioValue = ending_value,
        ExcessReturn = excess_return,
        Var = Var_95,
        ES_95 = ES_95
      ) 
    )
    
    return(metrics)
  }
  
  # Calculate the metrics for each strategy
  naive_metrics <- calculate_metrics(Naive)
  regression_metrics <- calculate_metrics(Regression)
  classification_metrics <- calculate_metrics(Classification)
  both_metrics <- calculate_metrics(Ensemble)
  
  # Group the information
  Portfolio_statistics <- cbind(naive_metrics, regression_metrics, classification_metrics, both_metrics)
  colnames(Portfolio_statistics) <- c("Naive", "Regression", "Classification", "Both")
  
  
  calculate_statistics <- function(df) {
    n_rows <- length(df$Value_position)  
    num_profitable <- sum(df$profitable == 1)
    percent_profitable <- (num_profitable / n_rows) * 100
    
    avg_ret_position <- round(mean(df$Value_position), 4)
    max_proft_position <- round(max(df$Value_position), 4)
    max_loss_position <- round(min(df$Value_position), 4)  
    
    statistics <- data.frame(
      Transactions = n_rows,
      Transactions_profitables_perc = percent_profitable,
      Gain_moyen = avg_ret_position,  
      Max_profit = max_proft_position,
      Max_perte = max_loss_position
    )
    return(statistics)
  }
  # Apply to every strat
  statistics_list <- lapply(all_trades, calculate_statistics)
  # Merge
  merged_df <- do.call(rbind, statistics_list)
  # Transpose the merged data frame
  Trades_statistics <- t(merged_df)
  
  risk_metrics <- list(Portfolio_statistics = Portfolio_statistics, Trades_statistics = Trades_statistics)
  
  return(risk_metrics)
  
  name_file <- paste0("Risk_metrics.rda")
  save(risk_metrics, file = here('Output', name_file))
  cat(paste("Risk_metrics.rda saved under:", name_file, "\n"))
  
}


f_call_plots <- function(equity_curves){
  ### All curves together
  # Melt the data
  df <- pivot_longer(equity_curves, -Date, names_to = "Method", values_to = "Value")
  
  # Plot
  p <- ggplot(df, aes(x = Date, y = Value, color = Method)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Stratégies de trading", x = "Date", y = "Valeur du portefeuille") +
    scale_color_discrete(name = "Stratégie:") +
    theme(axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "top")
  
  # Display the plot
  print(p)
  
  ### Only Naive
  # Data to xts
  equity_curve_xts <- xts(equity_curves[,2], order.by = as.Date(equity_curves$Date))
  
  # Daily log rets
  daily_returns <- Return.calculate(equity_curve_xts, method = "log")
  
  # Drawdown
  drawdown <- Drawdowns(daily_returns)
  
  par(mfrow = c(3, 1))
  # Equity Curve
  a <- plot(index(equity_curve_xts), equity_curve_xts, type = "l", col = "blue", ylab = "Equity Curve", xlab = "Date")
  # Daily Returns
  b <- plot(index(daily_returns), daily_returns, type = "h", col = "darkgreen", ylab = "Daily Returns", xlab = "Date")
  # Drawdown
  c <- plot(index(drawdown), drawdown, type = "h", col = "red", ylab = "Drawdown", xlab = "Date")
  print(a)
  print(b)
  print(c)
  par(mfrow = c(1, 1))
}



