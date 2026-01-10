
get_error_df = function(model_res){
  error_df =
    data.frame(
      type = "pred",
      rmse = rmse(model_res$pred_rt,model_res$true_rt),
      mape = mape(model_res$pred_rt,model_res$true_rt))
      ##auc = auc_calculate(model_res$pred_rt,model_res$true_rt))
  #auc_2 = auc_calculate_new(model_res$pred_rt,model_res$true_rt))
  return(error_df)
}


get_error_ride_df = function(model_res){
  error_df =
    data.frame(
      type = "ride",
      rmse = rmse(model_res$rt_temp_RIDE ,model_res$true_rt),
      mape = mape(model_res$rt_temp_RIDE ,model_res$true_rt))
  #auc_2 = auc_calculate_new(model_res$pred_rt,model_res$true_rt))
  return(error_df)
}



get_error_bp_df = function(model_res){
  error_df =
    data.frame(
      type = "bp",
      rmse = rmse(model_res$rt_temp_BP ,model_res$true_rt),
      mape = mape(model_res$rt_temp_BP ,model_res$true_rt))
  #auc_2 = auc_calculate_new(model_res$pred_rt,model_res$true_rt))
  return(error_df)
}





get_error_final_df = function(model_res){
  split_list = split(model_res,f = model_res$proj)
  error_7d_df = 
    cbind(lapply(split_list,function(df){
      get_error_df(df) }) %>% list.rbind(),
      proj = min(model_res$proj):max(model_res$proj))  %>% as.data.frame()
  return(error_7d_df)
}




get_RIDEtemp_error_final_df = function(model_res){
  split_list = split(model_res,f = model_res$proj)
  error_7d_df = 
    cbind(lapply(split_list,function(df){
      get_error_ride_df(df)}) %>% list.rbind(),
      proj = min(model_res$proj):max(model_res$proj)) %>% as.data.frame()
  return(error_7d_df)
} 





get_BPtemp_error_final_df = function(model_res){
  split_list = split(model_res,f = model_res$proj)
  error_7d_df = 
    cbind(lapply(split_list,function(df){
      get_error_bp_df(df)}) %>% list.rbind(),
      proj =min(model_res$proj):max(model_res$proj)) %>% as.data.frame()
  return(error_7d_df)
} 




get_Epinow2_temp_error_final_df = function(model_res){
  split_list = split(model_res,f = model_res$proj)
  error_7d_df = 
    cbind(lapply(split_list,function(df){
      error_df = data.frame( rmse = rmse(df$rt_temp_epinow2,df$true_rt),
                             mape = mape(df$rt_temp_epinow2,df$true_rt),
                             auc = auc_calculate(df$rt_temp_epinow2,df$true_rt))
      #auc_2 = auc_calculate_new(df$epinow2_temp_rt,df$true_rt))
      return(error_df)}) %>% list.rbind(),
      proj = min(model_res$proj):7) %>% as.data.frame()
  return(error_7d_df)
} 
