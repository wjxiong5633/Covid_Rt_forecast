arima_fit = function(training.set,vars_final){
  x = training.set[,vars_final]
  y = training.set$y_pred
  mytry = try(auto.arima(y,xreg = data.matrix(x),stationary = T),silent = T)
  model = mytry
  # if(inherits(try(mytry$fitted,silent = T),"try-error")){
  #   print("error")
  #   model = auto.arima(training.set$y_pred, xreg = data.matrix(training.set$rt_temp_past_1))
  # }else{
  #   model = mytry
  # }
  return(model)
}





