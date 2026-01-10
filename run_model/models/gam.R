gam_fit = function(training.set, vars_final){
  smooth_terms <- paste0("s(", vars_final, ",k = 5)", collapse = " + ")
  formula <- as.formula(paste("y_pred ~", smooth_terms))
  model <- gam(formula, data = training.set, method = "REML",select = TRUE)
  return(model)
}
