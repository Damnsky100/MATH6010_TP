#######################
##### Indicators #####
######################

# Generate the indicators to be used by the trading algorithm
list_ratios_technicals <- f_compute_ratios_technicals(stocks_symbols, 100, 2, df_vol, df_price)

######################
##### In-Sample #####
#####################

# Apply the training algorithm to the training set
# The function will also store the trade log of the back test for the classification algorithm

# Doit fixer probleme de colonne ici... si ca plante peut skip pour linstant le fichier est sauvegarder
#naive_in_sample <- f_run_strategy(start_date="2008-01-01", end_date="2017-12-31", list_ratios_technicals, strategy="naive")
#cat(paste("Completed: Naive trading (In-Sample)"))

# Generate the prediction for the classification model

# Retrieve the information fromthe training set
f_create_training_data_classification("2008-01-01", "2017-12-31")

# Train the classification model on the training set
classification_model <- f_train_classification()

# Retrieve the information for the out-of-sample set
f_create_test_data_classification()

# Retrieve file
merged_list <- get(load(here('Clean_Data','merged_list.rda')))

# Apply the classification model to the out-of-sample data
class_predictions <- lapply(merged_list, f_classification_prediction)

# Rename
names(class_predictions) <- names(merged_list)

# Empty_list to store
join_list <- list()

# Merge together the technicals and the classification prediction
for (i in seq_along(list_ratios_technicals)) {
  tech <- list_ratios_technicals[[i]]
  classification_flag <- class_predictions[[i]]
  
  merged_x <- merge(tech, classification_flag, join = "inner")
  join_list[[i]] <- merged_x
}

# Save as original technical data set
list_ratios_technicals <- join_list

##########################
##### Out-of Sample #####
#########################

# Apply the trading algorithm to the testing set
# The function will store the trade log for every strategy and their correspondin equity curve.
naive_out_sample <- f_run_strategy(start_date="2018-01-01", end_date="2022-12-31", list_ratios_technicals, strategy="naive")
cat(paste("Completed: Naive trading (Out-of-Sample)"))
regression_out_sample <- f_run_strategy(start_date="2018-01-01", end_date="2022-12-31", list_ratios_technicals, strategy="regression")
cat(paste("Completed: Regression trading (Out-of-Sample)"))
classification_out_sample <- f_run_strategy(start_date="2018-01-01", end_date="2022-12-31", list_ratios_technicals, strategy="classification")
cat(paste("Completed: Classification trading (Out-of-Sample)"))
both_out_sample<- f_run_strategy(start_date="2018-01-01", end_date="2022-12-31", list_ratios_technicals, strategy="both")
cat(paste("Completed: Both trading (Out-of-Sample)"))