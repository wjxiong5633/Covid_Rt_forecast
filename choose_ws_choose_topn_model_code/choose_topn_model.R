################################################################################
library(plyr)
library(dplyr)
library(tidyverse)
library(ISOweek)
library(locpol)
library(forecast)
library(scoringutils)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(cowplot)
library(patchwork)
library(data.table)
library(reshape2)
library(rugarch)
library(scales)
library(patchwork)

file.sources = list.files( c("./functions","./run_model/models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)
train_res_all = tibble()
model_name_list = c("arima","gam","garch","gbm","gpr","gru","lm","rf","svr","xgb")
# 
# for (modelname in model_name_list){
#   print(modelname)
#   train_res = train_res = read.csv(paste("./train_choose_ws/",modelname,"_train.csv",sep = "")) 
#   train_res_all = rbind (train_res_all,train_res)
# }
# 
# 
evaluate_error = function(mydf){
  rmse_res =
    mydf %>% mutate(abs_error = true - pred) %>%
    group_by(proj) %>%
    dplyr::summarise(rmse = sqrt(mean(abs_error^2,na.rm=T))) %>%
    ungroup() %>% as.data.frame()

  return(rmse_res)
}
# # 
# rmse_res_all =
#   train_res_all %>%
#   group_by(modelname) %>%
#   group_modify(~ evaluate_error(.x))

# 
# n_res = 
#   sapply(2:5, function(n){
#   rmse_top =
#     rmse_res_all %>%
#     group_by(proj) %>%
#     slice_min(order_by = rmse,n = n,with_ties = TRUE) %>%
#     dplyr::mutate(rank = dplyr::row_number()) %>%
#     filter(rank<=n) %>%
#     mutate(rank = paste("model",rank,sep=""))
# 
#   result = rmse_top %>%
#     pivot_wider(id_cols = c(proj),names_from = rank,values_from = modelname)
# 
#   ensemble_train = tibble()
#   for (myproj in 0:14){
#     select_model = rmse_top$modelname[rmse_top$proj == myproj]
# 
#     ensemble_pred_df =
#       train_res_all %>%
#       filter(modelname %in% select_model)
# 
#     mean_df =
#       ensemble_pred_df %>%
#       group_by(date) %>%
#       dplyr::summarise(pred = mean(pred)) %>%
#       mutate(proj = myproj)
# 
#     ensemble_train_proj = mean_df
# 
#     ensemble_train = rbind(ensemble_train,ensemble_train_proj)
#   }
#   
#   ensemble_train_res = left_join(ensemble_train,train_res[,c("date","true","proj")] %>% distinct(),by = c("date","proj"))
# 
#   ensemble_rmse = evaluate_error(ensemble_train_res)[,2]
#   #rmse(ensemble_train_res$pred,ensemble_train_res$true)
#   return(ensemble_rmse)
# })

# 
# n_res = n_res %>% as.data.frame() %>% 
#   mutate(proj = 0:14)
# colnames(n_res) = c("n_2","n_3","n_4","n_5","proj")
# 
# n_res %>% 
#   pivot_longer(n_2:n_5,names_to = "number",values_to = "rmse") %>% 
#   ggplot(aes(x = proj))+
#   geom_point(aes(y = rmse,color = number))
# 
# n_res %>% 
#   pivot_longer(n_2:n_5,names_to = "number",values_to = "rmse") %>% 
#   group_by(number) %>% 
#   dplyr::summarise(mean_rmse = mean(rmse))

valid_res_all = tibble()
for (modelname in model_name_list){
  print(modelname)
  valid_res = read.csv(paste("./pred_reportcase_point/",modelname,"_reportcase_point.csv",sep = "")) %>%
    filter(date <= as.Date("2022-06-30")) %>%
    #filter(mytestdate <= as.Date("2022-06-30")-15) %>%
    mutate(modelname = modelname,
           pred = pred_reportcase,
           true = true_reportcase)
  valid_res_all = rbind (valid_res_all,valid_res)
}


valid_res_all %>% 
  group_by(proj) %>% 
  dplyr::summarise(count = n())
#
rmse_res_all =
  valid_res_all %>% group_by(modelname) %>%
  group_modify(~ evaluate_error(.x))




sapply(1:5, function(n){
  rmse_top =
    rmse_res_all %>%
    group_by(proj) %>%
    slice_min(order_by = rmse,n = n,with_ties = TRUE) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    filter(rank<=n) %>%
    mutate(rank = paste("model",rank,sep=""))

  result = rmse_top %>%
    pivot_wider(id_cols = c(proj),names_from = rank,values_from = modelname)

  ensemble_valid = tibble()
  for (myproj in 0:14){
    select_model = rmse_top$modelname[rmse_top$proj == myproj]

    ensemble_pred_df =
      valid_res_all %>%
      filter(modelname %in% select_model)

    mean_df =
      ensemble_pred_df %>%
      group_by(date) %>%
      dplyr::summarise(pred = mean(pred)) %>%
      mutate(proj = myproj)

    ensemble_valid_proj = mean_df

    ensemble_valid = rbind(ensemble_valid,ensemble_valid_proj)
  }
  ensemble_valid_res = left_join(ensemble_valid,valid_res[,c("date","true")] %>% distinct(),by = "date")

  #ensemble_rmse = evaluate_error(ensemble_valid_res)[,2]
  ensemble_rmse = rmse(ensemble_valid_res$pred,ensemble_valid_res$true)
  return(ensemble_rmse)
})

