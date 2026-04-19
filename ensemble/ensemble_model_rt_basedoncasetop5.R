
evaluate_error = function(mydf){
  
  rmse_res = 
    mydf %>% mutate(abs_error = true - pred) %>% 
    group_by(proj) %>% 
    dplyr::summarise(rmse = sqrt(mean(abs_error^2,na.rm=T))) %>% 
    ungroup() %>% as.data.frame()
  
  return(rmse_res)
}

select_model_df = read.csv("./ensemble/select_model_bycase.csv")

model_name_list = c("arima","garch","gam","lm","rf","xgb","svr","gpr","gru","gbm")
rt.point <-tibble()
for(modelname in model_name_list){
  print(modelname)
  model.point = read.csv(paste("./pred_rt_interval/interval_",modelname,".csv",sep = "")) %>% 
    dplyr::select(modelname,date,proj,date_analysis ,true_rt,pred_rt,starts_with("lower"),starts_with("upper")) %>% 
    mutate(true = true_rt,
           pred= pred_rt,
           date_analysis  = as.Date(date_analysis )) %>% 
    filter(date_analysis  %in% seq(as.Date("2022-05-25"),as.Date("2022-12-31"),by = "day")) 
  rt.point = rbind(rt.point,model.point)
}



whole_period = seq(as.Date("2022-07-01"),as.Date("2022-12-31"),by = "day")
ensemble_df = tibble()
idate = 1

for (idate in c(1:length(whole_period))){
  mydate = whole_period[idate]
  print(mydate)
  rmse_top_all = tibble()
  for (myproj in 0:7){
    topmodels = select_model_df %>% filter(date_analysis == mydate) %>% 
      filter(proj == myproj)
    
    select_model = topmodels[,3:5]
    ensemble_pred_df = 
      rt.point %>% 
      mutate(date_analysis = as.Date(date_analysis)) %>% 
      filter(date_analysis  == mydate & proj == myproj) %>% 
      filter(modelname %in% select_model)
    
    mean_df = 
      ensemble_pred_df %>% dplyr::select(true_rt,pred,starts_with("lower"),starts_with("upper")) %>% 
      colMeans() %>% as.data.frame() %>% t()
    
    ensemble_mydate_myproj = cbind(data.frame(date_analysis  = mydate,
                                              date = mydate+ myproj, proj = myproj), mean_df) %>% 
      mutate(true_rt= unique(ensemble_pred_df$true_rt))
    
    ensemble_df = rbind(ensemble_df,ensemble_mydate_myproj)
  }
}


ensemble_df_rt= 
  ensemble_df %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(modelname = "ensemble2",
         pred_rt =  pred)

write.csv(ensemble_df_rt, paste("./pred_rt_interval/","interval_ensemble2.csv",sep = ""),row.names = F)

