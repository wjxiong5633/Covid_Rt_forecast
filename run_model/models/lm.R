lm_fit = function(training.set,vars_final){
  model = lm(y_pred ~ ., data = training.set[,c("y_pred",vars_final)])
  return(model)
}

