rf_fit = function(training.set,vars_final){
  # control <- trainControl(method='cv',
  #                       number=5)  #repeats=5
  # tunegrid <- expand.grid( .mtry = c(10,15))
  # set.seed(1111)
  # model <- train(y_pred ~ .,
  #                data = training.set[,c("y_pred",vars_final)],
  #                method = 'rf',
  #                metric = 'RMSE',
  #                tuneGrid = tunegrid,
  #                trControl = control,
  #                ntree = 500)
  set.seed(1234)
  model = randomForest(y_pred ~ ., data=training.set[,c("y_pred",vars_final)],
                       maxnodes= 10, ntree=100)
  return(model)
}

# 
# tune_result <- tuneRF(
#   x = training.set[,c(vars_final)],    # predictors
#   y = training.set$y_pred,  # response
#   stepFactor = 1.5,  # how much to increase/decrease mtry each step
#   improve = 0.01,    # minimum improvement to continue
#   ntreeTry = 500,    # number of trees for each trial
#   trace = TRUE,      # whether to print progress
# )
