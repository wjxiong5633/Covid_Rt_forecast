generate_raw_values <- function(growth_rate_forecast, last_value) {
  # Initialize the vector for raw values, with the size of the growth rate forecast
  raw_values <- numeric(length(growth_rate_forecast))
  
  # Set the last value as the true value
  raw_values[length(raw_values)] <- last_value
  
  # Calculate the raw values iteratively, starting from the last day
  for (i in (length(growth_rate_forecast) - 1):1) {
    raw_values[i] <- raw_values[i + 1] / (1 + growth_rate_forecast[i])  # Reverse calculation
  }
  
  # Return the raw values
  return(raw_values)
}
