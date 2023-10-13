#######################
##### INDICATORS #####
######################

# Generate the indicators to be used by the trading algorithm for every stock pair.
list_ratios_technicals <- f_compute_ratios_technicals(stocks_symbols, 100, 2, df_vol, df_price)

#########################
### REGRESSION MODEL ###
########################

# Estimate the regression models and create the price predictions for every pair.
f_run_price_regressions(stocks_symbols)

# Join the price predictions to the indicators for every pair. (will used for out of sample only)
list_ratios_technicals <- f_add_regression_predictions(list_ratios_technicals)


#################################
##### STRATEGY - IN-SAMPLE #####
################################

# Apply the training algorithm to the training set
# The function will also store the trade log of the back test for the classification algorithm
Naive_in_sample <- f_run_strategy(start_date="2008-01-01", end_date="2017-12-31", list_ratios_technicals, strategy="naive")
cat(paste("Completed: Naive trading (In-Sample)"))


############################
### CLASSIFICATION MODEL ###
############################

# Retrieve the information for the training set
f_create_training_data_classification("2008-01-01", "2017-12-31")

# Estimate the regression model
classification_model <- f_train_classification()

# Create the out-of-sample classification predictions for every stock pair.
list_ratios_technicals <- f_add_classification_predictions(list_ratios_technicals, classification_model)


#####################################
##### STRATEGY - OUT-OF-SAMPLE #####
####################################

# Apply the trading algorithm to the testing set
# The function will store the trade log for every strategy and their corresponding equity curve
start_date = "2018-01-01"
end_date = "2022-12-31"

# Naive
naive_out_sample <- f_run_strategy(start_date, end_date, list_ratios_technicals, strategy="naive")
cat(paste("Completed: Naive trading (Out-of-Sample)"))
# Regression
regression_out_sample <- f_run_strategy(start_date, end_date, list_ratios_technicals, strategy="regression")
cat(paste("Completed: Regression trading (Out-of-Sample)"))
# Classification
classification_out_sample <- f_run_strategy(start_date, end_date, list_ratios_technicals, strategy="classification")
cat(paste("Completed: Classification trading (Out-of-Sample)"))
# Both
both_out_sample<- f_run_strategy(start_date, end_date, list_ratios_technicals, strategy="both")
cat(paste("Completed: Both trading (Out-of-Sample)"))