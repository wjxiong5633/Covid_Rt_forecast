library(plyr)
library(dplyr)
library(tidyverse)
library(ISOweek)
library(locpol)
library(forecast)
library(scoringutils)
evaluate_error = function(mydf){

  rmse_res = 
    mydf %>% mutate(abs_error = true - pred) %>% 
    group_by(proj) %>% 
    dplyr::summarise(rmse = sqrt(mean(abs_error^2,na.rm=T))) %>% 
    ungroup() %>% as.data.frame()
  
  return(rmse_res)
}


model_name_list = c("arima","garch","gam","gbm","gpr","gru",
                    "lm","rf","svr","xgb")
case.point <-tibble()
for(modelname in model_name_list){
  print(modelname)
  model.point = read.csv(paste("./pred_reportcase_interval/interval_",modelname,".csv",sep = "")) %>% 
    dplyr::select(modelname,date,proj,mytestdate,true_reportcase,pred_reportcase,starts_with("lower"),starts_with("upper")) %>% 
    mutate(true = true_reportcase,
           pred= pred_reportcase,
           mytestdate = as.Date(mytestdate)) %>% 
    filter(mytestdate %in% seq(as.Date("2022-05-25"),as.Date("2022-12-31"),by = "day")) 
  case.point = rbind(case.point,model.point)
}


case.point = case.point %>% drop_na()
whole_period = seq(as.Date("2022-06-15"),as.Date("2022-12-31"),by = "day")
select_model_df = tibble()
ensemble_df = tibble()
idate = 2


for (idate in c(1:length(whole_period))){
  mydate = whole_period[idate]
  print(mydate)
  rmse_top_all = tibble()
  
  for (myproj in 0:14){
    
    mydf = case.point %>% 
      filter(proj == myproj) %>% 
      filter(mytestdate>= mydate - myproj - 7 & date<=  mydate -1)
    
    rmse_res_proj = 
      mydf %>% group_by(modelname) %>% 
      group_modify(~ evaluate_error(.x))
    
    rmse_top = 
      rmse_res_proj %>% 
      group_by(proj) %>% 
      slice_min(order_by = rmse,n = 3,with_ties = TRUE) %>%
      dplyr::mutate(rank = dplyr::row_number()) %>% 
      filter(rank<=3) %>% 
      mutate(date_analysis  = mydate,
             rank = paste("model",rank,sep=""))
    rmse_top_all = rbind (rmse_top_all, rmse_top)
    
    select_model = rmse_top$modelname
    
    ensemble_pred_df = 
      case.point %>% 
      filter(mytestdate == mydate & proj == myproj) %>% 
      filter(modelname %in% select_model)
    
    
    mean_df = 
      ensemble_pred_df %>% dplyr::select(true_reportcase,pred,starts_with("lower"),starts_with("upper")) %>% 
      colMeans() %>% as.data.frame() %>% t()
    
    ensemble_mydate_myproj = cbind(data.frame(mytestdate = mydate,date = mydate+ myproj, proj = myproj), mean_df) %>% 
      mutate(true_logcase = unique(log(ensemble_pred_df$true_reportcase)))
    
    ensemble_df = rbind(ensemble_df,ensemble_mydate_myproj)
  }
  
  
  result = rmse_top_all%>% 
    pivot_wider(id_cols = c(date_analysis ,proj),names_from = rank,values_from = modelname)
  
  select_model_df = rbind(select_model_df,result)
}


true_reportcase = readRDS("./data/true_reportcase.rds") %>% 
  mutate(true_logcase = log(report_case+0.0001))

ensemble_df_case = 
  ensemble_df %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(modelname = "ensemble",
         pred_reportcase = pred,
         pred_logcase = log(pred))

write.csv(ensemble_df_case, paste("./pred_reportcase_interval/","interval_ensemble.csv",sep = ""),row.names = F)
write.csv(select_model_df, "./ensemble/select_model_bycase.csv",row.names = F)



ensemble_df_case %>%  
  filter(mytestdate %in% seq(as.Date("2022-07-01"),as.Date("2022-12-31"),by = "day")) %>% 
  group_by(proj) %>% 
  dplyr::summarise(rmse =  sqrt(mean((true_reportcase-pred_reportcase )^2,na.rm=T))) %>% 
  as.data.frame()


model.interval = read.csv("./pred_reportcase_interval/interval_gam.csv") %>% 
  dplyr::select(modelname,date,mytestdate,proj,true_reportcase,pred_reportcase, 
                true_logcase,pred_logcase,
                starts_with("lower"),starts_with("upper")) %>% 
  mutate(mytestdate = as.Date(mytestdate)) %>% 
  filter(mytestdate %in% seq(as.Date("2022-07-01"),as.Date("2022-12-31"),by = "day"))                   

model.interval %>% 
  group_by(proj) %>% 
  dplyr::summarise(rmse =  sqrt(mean((true_reportcase-pred_reportcase )^2,na.rm=T))) %>% 
  as.data.frame()

