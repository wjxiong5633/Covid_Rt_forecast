garch_fit = function(training.set,vars_final){
  set.seed(123)
  x = training.set[,vars_final]
  y = training.set$y_pred
  
  fit.spec <- ugarchspec(variance.model     = list(model = "sGARCH",
                                                   garchOrder = c(1, 1)), 
                         mean.model         = list(armaOrder = c(0, 1),
                                                   include.mean = TRUE,
                                                   external.regressors = as.matrix(x)), 
                         distribution.model = "norm")
  model   <- ugarchfit(data = y, spec = fit.spec,solver ='solnp')
  
  return(model)
}
# 
# 
# fit.spec0 <- ugarchspec(
#   variance.model = list(model="sGARCH", garchOrder=c(1,1)),
#   mean.model     = list(armaOrder=c(0,0), include.mean=TRUE),
#   distribution.model="norm"
# )
# 
# test.model <- ugarchfit(data=y, spec=fit.spec0, solver="nlminb")





