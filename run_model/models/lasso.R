
lasso_fit = function(training.set,vars_final){
  
  set.seed(123)
  lambdas_to_try <- 10^seq(-2, 3, length.out = 15)
  # Setting alpha = 0 implements lasso regression
  x = training.set[,vars_final]
  y = training.set$y_pred
  lasso_cv <- cv.glmnet(x = as.matrix(x), y = y, 
                        alpha = 1, lambda = lambdas_to_try,
                        standardize = TRUE, nfolds = 5)
  
  lambda_cv <- lasso_cv$lambda.1se
  
  # Fit final model, get its sum of squared residuals and multiple R-squared
  model_cv <- glmnet(x = as.matrix(x), y = y,
                     alpha = 1, lambda = lambda_cv, standardize = TRUE)
  return(model_cv)
}

