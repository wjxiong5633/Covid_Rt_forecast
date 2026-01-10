ugarch_fit = function(training.set){
  max_lag = 14
  modelspec <- ugarchspec(mean.model = list(armaOrder=c(max_lag,0) ,external.regressors  = data.matrix(training.set[,-1])), 
                        variance.model = list(model = "sGARCH",garchOrder= c(1,1)))
  model <- ugarchfit(data=training.set$true_rt,spec=modelspec)
  return(model)
}


# pred_m <- ugarchforecast(model,n.ahead = 7,external.forecasts =  list(mregfor=newdata_x))
# pred_m@forecast$seriesFor
