f_load_trades <- function() {
  ### Cette fonction retrouve les transactions qui sont sauvegardées en .RDA et les combines ensembles.
  ### Elle ne contient pas d'input puisqu'elle retrouve les fichier .rda dans le dossier "output".  
  
  #  OUTPUTS
  #   output: [liste] Une liste ou chaque element est tout les trades de chaque stratégie.
  
  # Create empty list
  all_trades_list <- list()
   
  # Naive Strat
  loaded_name <- load(here("Output", "df_all_trades_naive_2018-01-01_to_2022-12-31.rda"))
  Naive_trades <- get(loaded_name)
  all_trades_list$Naive_trades <- Naive_trades
  
  # Regression Strat
  loaded_name <- load(here("Output", "df_all_trades_regression_2018-01-01_to_2022-12-31.rda"))
  Regression_trades <- get(loaded_name)
  all_trades_list$Regression_trades <- Regression_trades
  
  # Classification Strat
  loaded_name <- load(here("Output", "df_all_trades_classification_2018-01-01_to_2022-12-31.rda"))
  Classification_trades <- get(loaded_name)
  all_trades_list$Classification_trades <- Classification_trades
  
  # Both Strat
  Ensemble_trades <- load(here("Output", "df_all_trades_both_2018-01-01_to_2022-12-31.rda"))
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
  loaded_name <- load(here("Output", "Equity_curve_naive_2018-01-01_to_2022-12-31.rda"))
  naive_strat <- get(loaded_name)
  
  # Regression Strat
  loaded_name <- load(here("Output", "Equity_curve_regression_2018-01-01_to_2022-12-31.rda"))
  regression_strat <- get(loaded_name)
  
  # Classification Strat
  loaded_name <- load(here("Output", "Equity_curve_classification_2018-01-01_to_2022-12-31.rda"))
  classification_strat <- get(loaded_name)
  
  # Both Strat
  loaded_name <- load(here("Output", "Equity_curve_both_2018-01-01_to_2022-12-31.rda"))
  both_strat <- get(loaded_name)

  
  # Combine them together
  all_curves <- cbind(naive_strat, regression_strat[1], classification_strat[1], both_strat[1])
  names(all_curves) <- c("Naive", "Date" , "Regression", "Classification", "Ensemble")
  all_curves <- all_curves[, c(2, 1, 3, 4, 5)]
  return(all_curves)
}

f_risk_metrics <- function(equity_curves, all_trades, risk_free) {
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
  calculate_metrics <- function(returns, equity_curve, risk_free) {
    # Compute annual to daily
    Rf <- (1 + risk_free)^(1/252) - 1
    
    # Arithmetic return
    total_ret <- round((last(equity_curve)/first(equity_curve) - 1) * 100, 2)
    
    # log rets and some risk metrics
    total_ret_log <- round(Return.cumulative(returns) * 100, 2)
    Sharpe_ratio <- SharpeRatio(returns, Rf=Rf, scale=252)[1]*100
    Max_drawdown <- round(maxDrawdown(returns) * 100, 2)
    Var_95 <- round(quantile(returns, probs = 0.05), 4)*100
    ES_95 <- round(ES(returns, p = 0.05), 4)*100
    End_value <- last(equity_curve)
    
    
    # Combine 
    metrics <- cbind(
      c(TotalReturn = total_ret,
        TotalReturn_log = total_ret_log,
        SharpeRatio_with_rf = Sharpe_ratio,
        MaxDrawdown = Max_drawdown,
        Var = Var_95,
        ES_95 = ES_95,
        PortfolioValue = End_value,
        ExcessReturn = End_value - 100
      ) 
    )
    
    return(metrics)
  }
  
  # Calculate the metrics for each strategy
  # Naive
  naive_metrics <- calculate_metrics(Naive, equity_curves$Naive, 0.025)
  # Regressopm
  regression_metrics <- calculate_metrics(Regression, equity_curves$Regression, 0.025)
  # Classification
  classification_metrics <- calculate_metrics(Classification, equity_curves$Classification, 0.025)
  # Both
  both_metrics <- calculate_metrics(Ensemble, equity_curves$Ensemble, 0.025)
  # Group the information
  Portfolio_statistics <- cbind(naive_metrics, regression_metrics, classification_metrics, both_metrics)
  colnames(Portfolio_statistics) <- c("de Base", "Stategie 1", "Stategie 2", "Stategie 3")
  
  # Calculate some interesting information about the trades
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
  
}


f_call_plots <- function(equity_curves, all_trades_list){
  ### All curves together
  # Melt the data
  df <- pivot_longer(equity_curves, -Date, names_to = "Method", values_to = "Value")
  
  # Plot
  p1 <- ggplot(df, aes(x = Date, y = Value, color = Method)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    labs(title = "Valeur total des portefeuilles", x = "Date", y = "Dollars") +
    scale_color_discrete(name = "Stratégies:") +
    theme(axis.text = element_text(face = "bold", size = 12),
          axis.title = element_text(face = "bold", size = 12),
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "top",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.border = element_rect(colour = "black", fill = NA, size = 1))
  
  # Display the plot
  print(p1)
  ggsave(filename = here("Output", "Equity_curves.jpg"), plot = p1, width = 10, height = 6, dpi = 300)
  
 
  # Save some dictionaries for the histogram of the trades
  # Save the name of the strats
  strats <- names(all_trades_list)
  color_dict <- c("Naive_trades" = "#00BFC4",
                  "Regression_trades" = "#7CAE00",
                  "Classification_trades" = "#F8766D",
                  "Both_trades" = "#C77CFF")
  title_dict <- c("Naive_trades" = "P&L par transaction (Stratégie de Base)",
                  "Regression_trades" = "P&L par transaction (Stratégie 2)",
                  "Classification_trades" = "P&L par transaction (Stratégie 3)",
                  "Both_trades" = "P&L par transaction (Stratégie 3)")
  
  methods <- names(color_dict)
  
  # Loop the ggplot function through each strategies
  for (method in methods) {
    
    # Retrieve the dollar returns for each trade
    trade_values <- all_trades_list[[method]]$Value_position
    
    # Plot
    p <- ggplot(data.frame(trade_values), aes(x=trade_values)) + 
      geom_histogram(binwidth=0.01, fill=color_dict[method], color="black", alpha=0.7) + 
      labs(title=title_dict[method], x="Dollars", y="Fréquence") +
      theme(axis.text = element_text(face = "bold", size = 12),
            axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(hjust = 0.5, size = 18),
            legend.position = "top",
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            panel.border = element_rect(colour = "black", fill = NA, size = 1),
            panel.background = element_rect(fill = "white", colour = "white"))
    
    # Display the plot
    print(p)
    
    # Save in JPEG
    ggsave(filename = here("Output", paste0("Trades_", method, ".jpg")), plot = p, width = 10, height = 6, dpi = 300)
  }
}
  

