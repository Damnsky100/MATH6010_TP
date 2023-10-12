f_train_classification <- function() {
  
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
  set.seed(123)
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
  print(rf_default)
  return(rf_default)
}

