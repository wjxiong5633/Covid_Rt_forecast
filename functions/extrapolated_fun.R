
extrapolate_fun = function(cov_lag_NA_df){
  
  cov_lag_NA_df$date <- as.Date(cov_lag_NA_df$date)
  covariates <- names(cov_lag_NA_df[,-c(1:4)])
  target_date <- tail(cov_lag_NA_df$date ,1)
  extrapolated_df <- cov_lag_NA_df[,c(1:4)]
  
  # Loop through each covariate to perform the extrapolation
  for (cov in covariates) {
    ## print(cov)
    # Create a temporary data frame for the covariate
    temp_data <- cov_lag_NA_df %>%
      dplyr::select(date, cov) %>%
      drop_na()  # Remove rows with NA values for the specific covariate
  
    if (nrow(temp_data) > 1 & tail(temp_data$date,1) < tail(cov_lag_NA_df$date,1)) {
      
      future_dates <- seq(from = max(temp_data$date) + 1, to = target_date, by = "day")
      forecasted_values <- rep(colMeans(tail(temp_data[cov],1)),length(future_dates))
      # Combine the forecasted values and dates
      forecasted_data <- data.frame(date = future_dates, forecasted_values)
      names(forecasted_data) = c("date",cov)
      cov_merge_df = rbind(temp_data,forecasted_data)
    }else{
      cov_merge_df = temp_data
    }
    extrapolated_df = left_join(extrapolated_df, cov_merge_df, by = "date")
  }
  
  # View the extrapolated dataframe
  ##head(extrapolated_df)
  return(extrapolated_df) 
}





extrapolate_arima_fun = function(cov_lag_NA_df){
  
  # Assuming your dataframe 'cov_lag_NA_df' has a column 'date' in Date format
  cov_lag_NA_df$date <- as.Date(cov_lag_NA_df$date)
  
  # Ensure that covariates A, B, C, D, E, F are present in the dataframe
  covariates <- names(cov_lag_NA_df[,-c(1:2)])
  
  # Set the target date for extrapolation
  target_date <- tail(cov_lag_NA_df$date ,1)
  
  # Create an empty dataframe to store the extrapolated results
  extrapolated_df <- cov_lag_NA_df[,c(1:2)]
  
  # Loop through each covariate to perform the extrapolation
  for (cov in covariates) {
    ##print(cov)
    # Create a temporary data frame for the covariate
    temp_data <- cov_lag_NA_df %>%
      dplyr::select(date, cov) %>%
      drop_na()  
    
    if (nrow(temp_data) > 1 & tail(temp_data$date,1) < tail(cov_lag_NA_df$date,1)) {
      future_dates <- seq(from = max(temp_data$date) + 1, to = target_date, by = "day")
      
      arima.model = auto.arima(temp_data[cov])
      forecasted_values = forecast(arima.model,h = length(future_dates))$mean
      
      # Combine the forecasted values and dates
      forecasted_data <- data.frame(date = future_dates, forecasted_values)
      names(forecasted_data) = c("date",cov)
      cov_merge_df = rbind(temp_data,forecasted_data)
    }else{
      cov_merge_df = temp_data
    }
    extrapolated_df = left_join(extrapolated_df, cov_merge_df, by = "date")
  }
  
  # View the extrapolated dataframe
  ##head(extrapolated_df)
  return(extrapolated_df) 
}





extrapolate_poly_fun = function(cov_lag_NA_df){
  
  # Assuming your dataframe 'cov_lag_NA_df' has a column 'date' in Date format
  cov_lag_NA_df$date <- as.Date(cov_lag_NA_df$date)
  
  # Ensure that covariates A, B, C, D, E, F are present in the dataframe
  covariates <- names(cov_lag_NA_df[,-c(1:2)])
  
  # Set the target date for extrapolation
  target_date <- tail(cov_lag_NA_df$date ,1)
  
  # Create an empty dataframe to store the extrapolated results
  extrapolated_df <- cov_lag_NA_df[,c(1:2)]
  
  # Loop through each covariate to perform the extrapolation
  for (cov in covariates) {
    ##print(cov)
    # Create a temporary data frame for the covariate
    temp_data <- cov_lag_NA_df %>%
      dplyr::select(date, cov) %>%
      drop_na()  
    
    if (nrow(temp_data) > 1 & tail(temp_data$date,1) < tail(cov_lag_NA_df$date,1)) {
      future_dates <- seq(from = max(temp_data$date) + 1, to = target_date, by = "day")
      
      value = as.numeric(temp_data[cov][,1])
      days = as.numeric(temp_data$date)
      modeldata = data.frame(value, days)
      model <- lm(value ~  poly(days,3),modeldata) 
      
      new_days = as.numeric(future_dates)
      forecasted_values <- predict(model, newdata = data.frame(days = new_days))
      
      # Combine the forecasted values and dates
      forecasted_data <- data.frame(date = future_dates, forecasted_values)
      names(forecasted_data) = c("date",cov)
      cov_merge_df = rbind(temp_data,forecasted_data)
    }else{
      cov_merge_df = temp_data
    }
    extrapolated_df = left_join(extrapolated_df, cov_merge_df, by = "date")
  }
  
  # View the extrapolated dataframe
  ##head(extrapolated_df)
  return(extrapolated_df) 
}
# Fit the spline (use a cubic spline, but you can adjust the degree)
# spline_model <- smooth.spline(as.numeric(temp_data$date), temp_data[[cov]], cv = TRUE)
# 
# # Generate future dates to extrapolate (from the last observed date to the target date)
# future_dates <- seq(from = max(temp_data$date) + 1, to = target_date, by = "day")

# # Predict the future values using the spline model
# forecasted_values <- predict(spline_model, as.numeric(future_dates))$y


# temp_data$date_numeric = as.numeric(temp_data$date)
# model_linear <- lm(temp_data[[cov]] ~ date_numeric, data = temp_data)  # Use numeric representation of date
# 
# # Predict the future values till the target date
# future_dates <- seq(from = max(temp_data$date) + 1, to = target_date, by = "day")
# future_numeric_dates <- as.numeric(future_dates)  # Convert dates to numeric for prediction
# 
# # Extrapolate the covariate values
# forecasted_values <- predict(model_linear, newdata = data.frame(date_numeric = future_numeric_dates))


