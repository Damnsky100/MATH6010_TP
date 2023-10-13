f_train_classification <- function(start_date, end_date) {
  ### Cette fonction applique l'algorithme de classification (Random Forest).
  
  # Inputs:
  #   start_date: [Date] La date de debut du training set.
  #   end_date: [Date]  La date de fin du training set.
  #
  # Outputs
  #   rf_default: Le modèle de random forest estimé.
  
  # Load data
  load(here("Clean_Data", "trade_classification_is.rda"))
  
  dataset <- trade_classification
  dataset$PL_position.1 <- factor(dataset$PL_position.1, levels = c("0", "1"))
  y <- dataset$PL_position.1
  x <- dataset[, -which(names(dataset) == "PL_position.1")]
  
  #10 folds repeat 3 times
  control <- trainControl(method='repeatedcv',
                          number=10,
                          repeats=3)
  
  #Metric compare model is Accuracy
  set.seed(1234)
  
  #Number randomly variable selected is mtry
  mtry <- sqrt(ncol(x))
  tunegrid <- expand.grid(.mtry=mtry)
  rf_default <- train(x=x,
                      y=y,
                      data = dataset,
                      method='rf',
                      metric='Accuracy',
                      tuneGrid=tunegrid,
                      trControl=control)
  return(rf_default)
}

f_create_training_data_classification <- function(start_date, end_date){
  ### Cette fonction regroupe les données in sample
  ### requises pour entrainer l'algorithme de classification.
  
  # Inputs:
  #   start_date: [Date] La date de debut du training set.
  #   end_date: [Date]  La date de fin du training set.
  #
  # Outputs
  #   rf_default: Le dataset de training pour l'algo.
  
  # Data.frame construit à partir des données Bloomberg
  merged_df <- read.csv(here("Raw_Data", "classification_data.csv"))
  load(here("Clean_Data", "df_all_trades_naive.rda"))
  
  df_all_trades <- df_all_trades[df_all_trades$start_date >= start_date & df_all_trades$start_date <= end_date, ]
  
  # Data.frame construit à partir des trades faits in sample
  df_all_trades <- cbind(df_all_trades, as.data.frame(do.call(rbind, strsplit(as.character(df_all_trades$ratio), '/'))))
  
  # Nommer les columnes en fonction du nominateur et denominateur de la paire
  colnames(df_all_trades)[14:15] <- c("numerator", "denominator")
  
  # conversion en date
  df_all_trades$start_date <- as.Date(df_all_trades$start_date)
  merged_df$dates <- as.Date(merged_df$dates)
  
  # Inner join fait sur la date et le stock
  result_df <- merge(df_all_trades, merged_df, by.x = c("start_date", "numerator"), by.y = c("dates", "stock"), all = FALSE)
  
  colnames(result_df)[which(names(result_df) %in% colnames(merged_df))] <- paste0(colnames(result_df)[which(names(result_df) %in% colnames(merged_df))], "_num")
  
  # suppression des colomnes donnant un forward seeing bias ou inutile
  result_df <- result_df[, !(names(result_df) %in% c("dates_num", "stock_num", "Latest.earnings.date_num", "trade_flag", "ratio", "X_num"))]
  
  # Autre inner join
  result_df <- merge(result_df, merged_df, by.x = c("start_date", "denominator"), by.y = c("dates", "stock"), all = FALSE)
  
  colnames(result_df)[which(names(result_df) %in% colnames(merged_df))] <- paste0(colnames(result_df)[which(names(result_df) %in% colnames(merged_df))], "_den")
  
  # # suppression des colomnes donnant un forward seeing bias ou inutile
  trade_classification <- result_df[, !(names(result_df) %in% c("dates_den", "stock_den", "Latest.earnings.date_den", "end_date", "exit_price", "Value_position", "PL_position", "day_count", "X_den"))]
  
  rda_file_path <- file.path(here("Clean_Data"), 'trade_classification_is.rda')
  save(trade_classification, file = rda_file_path)
  
}

f_create_test_data_classification <- function(list_ratios_technicals){
  ### Cette fonction regroupe les données out-of-sample 
  ### requises pour l'algorithme de classification.
  ### Elle utilise des données de BBG qui se trouve dans le classification_data.csv.
  
  # Inputs:
  #   list_ratios_technicals: [List] Liste de ratios techniques de chaque paire

  # Outputs
  #   merged_list: [List] Liste contenant les données requises.
  
  # Read the CSV file
  df <- read.csv(here("Raw_data", "classification_data.csv"))
  df <- df[-1, ]
  df <- dplyr::rename(df, date = dates)
  
  # Split the data into a list of data frames for each stock ticker
  list_df_classification <- split(df, df$stock)
  
  # Load the list_ratios_technicals
  # test <- load(here("Output", "list_ratios_technicals_all.RDA"))
  # test <- get(test)
  
  trade_class <- get(load(here("Clean_Data", "trade_classification_is.RDA")))
  
  # Merge the list_ratios technicals data with the classification prediction
  merged_list <- lapply(names(list_ratios_technicals), function(ticker) {
    # Split the ticker
    split_ticker <- unlist(strsplit(ticker, "/"))
    numerator <- split_ticker[1]
    denominator <- split_ticker[2]
    
    # Convert xts to data.frame
    xts_df <- as.data.frame(list_ratios_technicals[[ticker]])
    
    # Add a date column to the data frame version of the xts object
    xts_df$date <- as.character(index(list_ratios_technicals[[ticker]]))
    
    # Append "_den" to the column names except for the "date" column
    cols <- setdiff(colnames(list_df_classification[[numerator]]), "date")
    colnames(list_df_classification[[numerator]])[colnames(list_df_classification[[numerator]]) %in% cols] <- paste0(cols, "_num")
    
    # Merge the numerator
    merged_df_numerator <- merge(xts_df, list_df_classification[[numerator]], by = "date")
    
    # Append "_den" to the column names except for the "date" column
    cols <- setdiff(colnames(list_df_classification[[denominator]]), "date")
    colnames(list_df_classification[[denominator]])[colnames(list_df_classification[[denominator]]) %in% cols] <- paste0(cols, "_den")
    
    # Merge the denominator
    merged_df_denominator <- merge(merged_df_numerator, list_df_classification[[denominator]], by = "date")
    
    desired_order <- c("date", "stock_den", "stock_num", "vol_ratio", "MA_100", "up_band", "lo_band",
                       "Market.Cap_num", "Bloomberg.1.year.distance.to.default_num", "Volume.total.call_num", "Volume.total.put_num",
                       "open.int.total.call_num", "open.int.total.put_num", "Stock.volume_num",
                       "Put.Call.Volume.ratio_num", "Put.Call.open.interest.ratio_num", "Short.interest.equity.float_num",
                       "incoming.earnings_num", "Market.Cap_den", "Bloomberg.1.year.distance.to.default_den",
                       "Volume.total.call_den", "Volume.total.put_den", "open.int.total.call_den", "open.int.total.put_den",
                       "Stock.volume_den", "Put.Call.Volume.ratio_den", "Put.Call.open.interest.ratio_den",
                       "Short.interest.equity.float_den", "incoming.earnings_den")
    
    merged_df_denominator <- merged_df_denominator[,match(desired_order, colnames(merged_df_denominator))]
    
    colnames(merged_df_denominator) <- colnames(trade_classification[, -which(names(trade_class) == "PL_position.1")])
    
    merged_list <- merged_df_denominator
    
    return(merged_list)
    
  })
  
  # Save the names and save the file
  names(merged_list) <- names(list_ratios_technicals)
  save(merged_list, file = here("Clean_Data", "merged_list.rda"))
  
  return(merged_list)
}

f_classification_prediction <- function(merged_list_element, classification_model){
  ### Cette fonction applique le modèle de random forest qui a été estimé et 
  ### determine les prédictions de profitabilité pour le data set donné.
  
  # Inputs:
  #   merged_list_element: [List] Liste contenant les données requises.
  #   classification_model: Modèle de random forest estimé
  # Outputs
  #   class_prediction_xts: [List] Liste d'objet xts qui contient les prédictions.
  
  # Save the original merged_list_element as tmp
  tmp <- merged_list_element
  
  # Used the predict function to generate a classification
  class_prediction <- predict(classification_model, tmp)
  class_prediction <- as.numeric(class_prediction)
  
  # Join the class prediction onto tmp
  tmp <- cbind(tmp, class_prediction)
  
  # Keep only the "start_date" and "class_prediction" columns
  tmp <- tmp[, c("start_date", "class_prediction"), drop = FALSE]
  
  # Rename the "class_prediction" column
  colnames(tmp)[colnames(tmp) == "class_prediction"] <- "classification_flag"
  
  tmp[,"classification_flag"] <- tmp[,"classification_flag"]-1
  
  # Convert "start_date" to a Date object
  tmp$start_date <- as.Date(tmp$start_date)
  
  # Create an xts object with "start_date" as the date index
  class_prediction_xts <- xts(tmp[,"classification_flag"], order.by = tmp[,"start_date"])
  
  return(class_prediction_xts)
}

f_add_classification_predictions <- function(list_ratios_technicals, classification_model){
  ### Cette fonction joint les predictions de prix au data set avec les ratios techniques
  ### qui sont utilisés par les fonctions de trading.
  
  # Inputs:
  #   list_ratios_technicals: [List] La liste qui contient les ratios technique pour chaque stocks
  
  # Outputs
  #   list_ratios_technicals : [List] La liste de ratios technique avec les prix prédit.
  
  # Store names
  store_names <- names(list_ratios_technicals)
  
  # Retrieve the information for the test data
  merged_list <- f_create_test_data_classification(list_ratios_technicals)
  
  # Retrieve file
  #merged_list <- get(load(here('Clean_Data','merged_list.rda')))
  
  # Apply the classification model to the out-of-sample data
  class_predictions <- lapply(merged_list, function(x) f_classification_prediction(x, classification_model))
  
  # Empty_list to store
  join_list <- list()
  
  # Merge together the technicals and the classification prediction
  for (i in seq_along(list_ratios_technicals)) {
    tech <- list_ratios_technicals[[i]]
    classification_flag <- class_predictions[[i]]
    
    merged_x <- merge(tech, classification_flag, join = "inner")
    join_list[[i]] <- merged_x
  }
  names(join_list) <- store_names
  # Save as original technical data set
  list_ratios_technicals <- join_list
  
  return(list_ratios_technicals)
}