
evaluate_error = function(mydf){
  
  rmse_res = 
    mydf %>% mutate(abs_error = true - pred) %>% 
    group_by(proj) %>% 
    dplyr::summarise(rmse = sqrt(mean(abs_error^2,na.rm=T))) %>% 
    ungroup() %>% as.data.frame()
  
  return(rmse_res)
}



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
select_model_df = tibble()
ensemble_df = tibble()
idate = 1

for (idate in c(1:length(whole_period))){
  mydate = whole_period[idate]
  print(mydate)
  rmse_top_all = tibble()
  for (myproj in 0:7){
    mydf = rt.point %>%  
      filter(proj == myproj) %>% 
      filter(date_analysis>= mydate - myproj - 7 & date<=  mydate -1)
    
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
  
  
  result = rmse_top_all%>% 
    pivot_wider(id_cols = c(date_analysis ,proj),names_from = rank,values_from = modelname)
  
  select_model_df = rbind(select_model_df,result)
}




ensemble_df_rt= 
  ensemble_df %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(modelname = "ensemble",
         pred_rt =  pred)

write.csv(ensemble_df_rt, paste("./pred_rt_interval/","interval_ensemble.csv",sep = ""),row.names = F)
write.csv(select_model_df, "./ensemble/select_model_byrt.csv",row.names = F)


