nn_fit = function(training.set,vars_final){
  # Define Keras Model
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", input_shape = n_features) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1)  # Regression output
  
  # Compile Model
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(learning_rate = 0.001),
    metrics = c("mae")
  )
  
  # Train Model
  history <- model %>% fit(
    training.set[,vars_final], y_train = training.set$y_pred,
    epochs = 100,
    batch_size = 32,
    validation_split = 0.2,
    verbose = 1
  )
}
  
  
#   # model = nnet(training.set$y_pred ~ ., data = training.set[,vars_final],
#   #      size= 10, #round(nrow(training.set[W,])/(3*ncol(training.set[W,])))
#   #      linout=T,maxit=1000,decay=0.001,trace=F,MaxNWts = 84581)
#   # return(model)
#   train_control <- trainControl(method = "cv", number = 5)
# 
#   model = caret::train(
#     y_pred~ .,
#     data = training.set[,c(vars_final,"y_pred")],
#     method = "mlp",               # Multilayer Perceptron (RSNNS package)
#     trControl = train_control,
#     linOut = TRUE,                # Continuous output for regression
#     size = 10,              # Reduce hidden units
#     maxit = 500,           # Increase max iterations
#     trace = FALSE
#   )
#   return(model)
# }
# # 
# 
# nn_fit = function(training.set,vars_final){
#   model <- brnn(
#     as.matrix(training.set[,vars_final]),
#     training.set$y_pred,
#     neurons = 10,   # Hidden units
#     epochs = 1000   # Number of iterations
#   )
#   
#   return(model)
#   
# }
