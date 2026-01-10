xgb_fit = function(training.set,vars_final){
  # set.seed(123)
  # control <- trainControl(method='cv',
  #                         number=5,
  #                         search = 'grid')  #repeats=5
  # 
  # tunegrid <- expand.grid( nrounds = 300,
  #                          max_depth = c(8),
  #                          eta = c(0.5,0.05),
  #                          gamma = 0,
  #                          colsample_bytree = 1,
  #                          min_child_weight = 1,
  #                          subsample = 1)
  # model <- train(y_pred ~ .,
  #                    data = training.set[,c("y_pred",vars_final)],
  #                    method = 'xgbTree',
  #                    metric = 'RMSE',
  #                    tuneGrid = tunegrid,
  #                    trControl = control)
  
  x = training.set[,vars_final] %>% as.matrix()
  y = training.set$y_pred
  model =
    xgboost(
    x, y,
    verbosity = 1,
    nrounds = 1500,
    verbose = F,
    max_depth = 10,
    reg_lambda = 0.8,
    learning_rate = 0.1
  )
  
  
  return(model)
}
