gbm_fit = function(training.set,W){
  # model = gbm(true_rt ~ ., data = training.set[W,],n.minobsinnode =10, shrinkage = 0.1,
  #             distribution = "gaussian", #cv.folds = 5, # bag.fraction = 0.6, interaction.depth = 3,
  #             n.trees =500,verbose=FALSE)
  
  caretGrid =  expand.grid(n.trees = 500,interaction.depth=c(1,3),
                           shrinkage=c(0.3),n.minobsinnode=5)
  metric <- "RMSE"
  trainControl <- trainControl(method="cv", number=5)
  model <- train(y_pred ~ ., data=training.set[W,], distribution="gaussian", method="gbm",
                 trControl=trainControl , verbose=FALSE,
                 tuneGrid=caretGrid,
                 metric=metric, bag.fraction=0.7)
  # 
  return(model)
  
}

# model_fit = gbm_fit
# 
# 
# model_fit(training.set,W)
