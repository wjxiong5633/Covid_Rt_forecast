
ridge_fit = function(training.set,vars_final){
  # control = trainControl(method="cv", number= 5)
  # tune.grid = expand.grid(lambda = 5 ^ seq(-2, -1, length = 100), alpha = c(0,1))
  # model <- train(true_rt ~., data = training.set[W,], method = "glmnet",
  #                preProcess=c('scale', 'center'),
  #                tuneGrid = tune.grid,
  #                trControl=control)
  set.seed(1)
  lambdas_to_try <- 10^seq(-2, 2, length.out = 20)
  # Setting alpha = 0 implements ridge regression
  x = training.set[,vars_final]
  y = training.set$y_pred
  ridge_cv <- cv.glmnet(x = as.matrix(x), y = y, 
                        alpha = 0, lambda = lambdas_to_try,
                        standardize = TRUE, nfolds = 5)
  
  lambda_cv <- ridge_cv$lambda.1se
  
  # Fit final model, get its sum of squared residuals and multiple R-squared
  model_cv <- glmnet(x = as.matrix(x), y = y,
                     alpha = 0, lambda = lambda_cv, standardize = TRUE)
  return(model_cv)
}

