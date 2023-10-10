f_load_trades <- function() {
  ### Cette fonction retrouve les transactions qui sont sauvegardées en .RDA et les combines ensembles.
  ### Elle ne contient pas d'input puisqu'elle retrouve les fichier .rda dans le dossier "output".  
  
  #  OUTPUTS
  #   output: [liste] Une liste ou chaque element est tout les trades de chaque stratégie.
  
  # Empty list
  all_trades_list <- list()
  
  # Naive Strat
  loaded_name <- load(here("output", "df_all_trades_naive_2018-01-01_to_2022-12-31.rda"))
  naive_trades <- get(loaded_name)
  cat("Loaded: naive_trades\n")
  all_trades_list$naive_trades <- naive_trades
  
  # Regression Strat
  loaded_name <- load(here("output", "df_all_trades_regression_2018-01-01_to_2022-12-31.rda"))
  regression_trades <- get(loaded_name)
  cat("Loaded: regression_trades\n")
  all_trades_list$regression_trades <- regression_trades
  
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
  cat("Loaded: naive_strat\n")
  
  # Regression Strat
  loaded_name <- load(here("output", "Equity_curve_regression_2018-01-01_to_2022-12-31.rda"))
  regression_strat <- get(loaded_name)
  cat("Loaded: regression_strat\n")
  
  # Combine them together
  all_curves <- merge(naive_strat, regression_strat, by = "Date", all = TRUE)
  names(all_curves) <- c("Date", "Naive", "Regression")
  
  return(all_curves)
}

f_risk_metrics <- function(equity_curves, all_trades) {
  ### Cette fonction calcul les mesures de risques et retrouvent des informations pertinentes.
  
  # Inputs
  #   equity_curves: [Data.frame] Dataframe contenant toutes les courbes d'équités.
  #   all_trades: [List] Liste contenant toute les trades pour chaque paires.
  
  # Convert the Date column
  equity_curves$Date <- as.Date(equity_curves$Date, format="%Y%m%d")
  
  # Create xts object
  all_curves_xts <- xts(equity_curves[, -which(names(equity_curves) == "Date")], order.by=equity_curves$Date)
  
  # Compute log returns
  all_returns_xts <- diff(log(all_curves_xts))
  all_returns_xts <- all_returns_xts[-1, ]
  
  # Extract the series for naive and regression
  naive <- all_returns_xts[, 1]
  regression <- all_returns_xts[, 2]
  
  # Function to calculate metrics
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
  
  naive_metrics <- calculate_metrics(naive)
  regression_metrics <- calculate_metrics(regression)
  
  Portfolio_statistics <- cbind(naive_metrics, regression_metrics)
  colnames(Portfolio_statistics) <- c("Naive", "Regression")
  
  
  calculate_statistics <- function(df) {
    n_rows <- length(df$Value_position)  
    num_profitable <- sum(df$profitable == 1)
    percent_profitable <- (num_profitable / n_rows) * 100
    
    avg_ret_position <- round(mean(df$Value_position), 4)
    max_proft_position <- round(max(df$Value_position), 4)
    max_loss_position <- round(min(df$Value_position), 4)  
    
    statistics <- data.frame(
      Trades = n_rows,
      Profitable_trades_percentage = percent_profitable,
      Avg_ret_position = avg_ret_position,  
      Max_profit_position = max_proft_position,
      Max_loss_position = max_loss_position
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
