para_summary <- function(mcmc,a,b,print){
  y <- matrix(NA,ncol(mcmc),4)
  for (i in 1:ncol(mcmc)){
    y[i,1:3] <- quantile(mcmc[,i],c(0.5,0.025,0.975))
    y[i,4] <- sum(diff(mcmc[,i])!=0)/nrow(mcmc)
  }
  layout(matrix(1:(a*b),nrow=a,byrow=T))
  par(mar=c(2,4,1,1))
  if (print==1){
    for (i in 1:ncol(mcmc)){
      plot(mcmc[,i],type="l")
    }
  }
  return(y)
}


scale_fun = function(vec,train_index){
  train.mean = mean(vec[1:train_index],na.rm = T)
  train.sd = sd(vec[1:train_index],na.rm = T)
  vec_scale = (vec - train.mean)/train.sd
}


mape <- function(predicted,true){
  mape_value<- mean(abs((true - predicted)/true),na.rm = T)
  return (mape_value)
}

rmse <- function(predicted,true){
  rmse_value<- sqrt(sum((true-predicted)^2,na.rm = T)/length(true))
  return(rmse_value)
}




classify_score = function(pred,true){
  
  true_fct = factor(ifelse(true>1,1,0),levels = c(0,1))
  pred_fct = factor(ifelse(pred>1,1,0),levels = c(0,1))
  
  res = confusionMatrix(pred_fct, true_fct, mode = "everything", positive="1")
  #score = res$byClass[7]
  score = res$overall[1]
  return(score)
}


auc_calculate = function(pred,true){
  true_fct = ifelse(true>=1,1,0)  ##factor(ifelse(true>1,1,0),levels = c(0,1))
  pred_fct = ifelse(pred>=1,1,0) ##factor(ifelse(pred>1,1,0),levels = c(0,1))
  #roc_1 = roc(true_fct,pred_fct)
  if(length(unique(true_fct)) == 1 | length(unique(pred_fct)) == 1){
    true_fct = factor(ifelse(true>1,1,0),levels = c(0,1))
    pred_fct = factor(ifelse(pred>1,1,0),levels = c(0,1))
    
    res = confusionMatrix(pred_fct, true_fct, mode = "everything", positive="1")
    #score = res$byClass[7]
    score = res$overall[1] %>% as.numeric()
    auc_res = score
  }else{
    auc_res = auc(true_fct,pred_fct,quiet = T) %>% as.numeric()
  }
  return(auc_res)
}






get_error_df_7d = function(model_res){
  sub1 = model_res %>% dplyr::select(date,true_rt) %>% distinct()
  sub2 = model_res %>% dplyr::select(-true_rt) %>% dplyr::rename("date_nowcast" = "date","date" = "date_proj")
  sub3 = left_join(sub1,sub2,by = "date")
  split_list = split(sub3,f = sub3$proj)

  error_7d_df = lapply(split_list,function(df){
    get_error_df(df)
  }) %>% list.rbind() %>% as.data.frame()
  #print(colSums(res))
  return(error_7d_df)
}

get_final_df_7d_train = function(model_res_df,true_rt_dat){
  forecast_train = model_res_df %>% filter(proj>=0)
  forecast_train = left_join(
    left_join(forecast_train,ride_rt, by = c("date","date_analysis")),
    bp_rt, by = c("date","date_analysis"))
  forecast_train$rt_temp_RIDE <- na.locf(forecast_train$rt_temp_RIDE)
  forecast_train$rt_temp_BP <- na.locf(forecast_train$rt_temp_BP)

  df_train = forecast_train%>% arrange(date_analysis,date)

  sub1 = true_rt_dat
  sub2 = df_train
  sub3 = left_join(sub2,sub1,by = "date") %>% arrange(date_analysis)
  return(sub3)
}




get_final_df_7d_test = function(model_res_df,true_rt_dat){
  nowcast_test = model_res_df %>% filter(proj<0)
  forecast_test = model_res_df %>% filter(proj>=0)
  nowcast_test =
    left_join(
      left_join(nowcast_test,ride_rt_thatday, by = c("date")),
      bp_rt_thatday, by = c("date"))
  forecast_test = left_join(
    left_join(forecast_test,ride_rt, by = c("date","date_analysis")),
    bp_rt, by = c("date","date_analysis"))

  forecast_test$rt_temp_RIDE <- na.locf(forecast_test$rt_temp_RIDE)
  forecast_test$rt_temp_BP <- na.locf(forecast_test$rt_temp_BP)

  df_test = rbind(nowcast_test,forecast_test) %>% arrange(date_analysis,date)

  sub1 = true_rt_dat
  sub2 = df_test
  sub3 = left_join(sub2,sub1,by = "date") %>% arrange(date_analysis)
  return(sub3)
}




get_combine_pred_df = function(model_res_df,ride_bp_rt_pred14,true_rt_dat){
  df_test =  left_join(model_res_df,ride_bp_rt_pred14, by = c("date_analysis","date","proj"))
  sub1 = true_rt_dat
  sub2 = df_test
  sub3 = left_join(sub2,sub1,by = "date") %>% arrange(date_analysis)
  return(sub3)
}




shift_data_cov = function(dat,t,var_final){
  sub1<- dat
  sub2<-sub1
  sub2$date<-sub2$date + t
  sub<-left_join(sub1[,c("date","true_rt")],sub2[,c("date","temp_rt",all_of(var_final))], by = c("date"))
  sub[is.na(sub$temp_rt),c("temp_rt",all_of(var_final))] = sub1[is.na(sub$temp_rt),c("temp_rt",all_of(var_final))]
  sub<-sub %>% drop_na(all_of(var_final)) %>%
    as.data.frame()
  return(sub)
}

quant2 <- function(x, a){
  quantile(x,a,na.rm = TRUE)
}

normalize <- function(x) {
  return ((x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T)))
}






get_final_df_predylag14 = function(model_res_df,true_rt_lag){
  nowcast_test = model_res_df %>% filter(proj<0)
  forecast_test = model_res_df %>% filter(proj>=0)
  nowcast_test =
    left_join(
      left_join(nowcast_test,ride_rt_thatday, by = c("date")),
      bp_rt_lag, by = c("date"))
  forecast_test = left_join(
    left_join(forecast_test,ride_rt, by = c("date","date_analysis")),
    bp_rt, by = c("date","date_analysis"))
  forecast_test$rt_temp_RIDE <- na.locf(forecast_test$rt_temp_RIDE)
  forecast_test$rt_temp_BP <- na.locf(forecast_test$rt_temp_BP)

  df_test = rbind(nowcast_test,forecast_test) %>% arrange(date_analysis,date)

  sub1 = true_rt_lag
  sub2 = df_test
  sub3 = left_join(sub2,sub1,by = "date") %>% arrange(date_analysis)
  return(sub3)
}

