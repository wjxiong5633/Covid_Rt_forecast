gpr_fit = function(training.set,vars_final){
  # model = gausspr(y_pred ~ ., data = training.set[,c("y_pred",vars_final)], 
  #             type='regression',variance.model=TRUE,kernel='rbfdot', kpar=list(sigma=0.0001))
  # 
  # model = gausspr(y_pred ~ ., data = training.set[,c("y_pred",vars_final)])
  set.seed(1234)
  model = gausspr(y_pred ~ ., data = training.set[,c("y_pred",vars_final)],
              type='regression',variance.model=FALSE, tol = 0.0005,
              kernel='polydot')
  # model = gausspr(x = training.set[,vars_final], y = training.set["y_pred"], 
  #                 scaled = TRUE, type= NULL, kernel="rbfdot",
  #         kpar="automatic", var=1, variance.model = FALSE, tol=0.0005,
  #         cross=0, fit=TRUE,na.action = na.omit)
  
  return(model)
}

