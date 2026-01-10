
library(lubridate)
library(tidyverse)
library(neuralnet)
library(nnet)
library(caret)
library(tidyverse)
library(forecast)
library(data.table)
library(rlist)
library(randomForest)
library(gbm)
library(glmnet)
library(xgboost) 
library(kernlab)
library(e1071)
library(mgcv)
library(pROC)
library(zoo)
library(rugarch)
library(mvtnorm)
library(incidental)
library(surveillance)
library(Rcpp)
library(RcppParallel)

file.sources = list.files( c("./functions","./run_model/models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)

data_case = read_rds("./data/data_case.rds")
other_cov = read_rds("./data/other_cov.rds")

true_dat = data_case


epi_vec <<- c("log_case","ct_sm_7")
stringency<<-c("StringencyIndex_WeightedAverage","GovernmentResponseIndex_WeightedAverage",
               "ContainmentHealthIndex_WeightedAverage")
meteorology<<- c("temperature","humidity","wind_speed")
pollutant <<-c("ozone","nitrogen_dioxide","nitrogen_oxides","carbon_monoxide",
               "fine_suspended_particulates",'respirable_suspended_particulates',"sulphur_dioxide") 
vec_need_scale  = c("ct_sm_7",meteorology,pollutant,stringency)
test_start_period = as.Date("2022-07-01")
ahead_horizon_case = 14


Fit_functions = list(nn = nn_fit,arima = arima_fit,garch = garch_fit,gbm = gbm_fit,
                     lm = lm_fit, gpr = gpr_fit,svr = svr_fit,gam = gam_fit)


get_train_beforetest = function(modelname){
  model_fit = Fit_functions[[modelname]]
  print("-----------------------")
  print(test_start_period[1])
  
  test_start_date = test_start_period[1]
  test_end_date = test_start_date + ahead_horizon_case
  train_end_date <- test_start_date -1
  train_start_date <-as.Date("2022-02-15")
  
  
  dt_case = data_case %>% filter(date>= train_start_date & date<test_start_date)
  dt_case2 = dt_case 
  
  ## observed_data till 0527
  observed_data  = left_join(dt_case2,other_cov,by = "date") 
  raw_data = observed_data 
  train_res_all = tibble()
  
  
  for (h in 0:14) {
    print(paste("horizon = ", h ))
    C0_raw = raw_data %>%
      mutate(log_case = lead(log_case, h),
             scaled_case = log_case,
             y_pred = scaled_case)
    
    
    ## do scale for all observed data till 0527:
    ## scale for dataset
    train_cov_date = seq(train_start_date,train_end_date ,by = "day")
    train_index = length(train_cov_date)
    need_scale_cov = C0_raw[,vec_need_scale]
    scale_cov = data.frame(C0_raw[,c("date","report_case","log_case","y_pred","scaled_case")],
                           apply(need_scale_cov, 2, scale_fun,train_index = train_index))
    
    epi_vec_alllags  = c("scaled_case","ct_sm_7")
    mypredictors = c(epi_vec_alllags, meteorology, pollutant, stringency)
    cov_lag_weather_policy= obtain_data_alllag(scale_cov,mypredictors)
    
    cov_lag_df = left_join(cov_lag_weather_policy,scale_cov[,c("date","report_case","log_case","scaled_case")],by = "date") %>% 
      dplyr::select(date,report_case,log_case,y_pred,scaled_case,everything())
    
    
    
    full_date = data.frame(date = seq(train_start_date,test_start_date,by = "day"))
    cov_lag_NA_df= full_join(full_date, cov_lag_df ,by = "date") 
    
    
    cov_lag_final_df = extrapolate_fun(cov_lag_NA_df) %>% 
      dplyr::select(date,report_case,log_case,y_pred,scaled_case, everything()) 
    
    
    
    
    ## for h = 1, use observed x till 0526 and y(t+1) till 0527 to select covariates and run models
    train_run_date = seq(train_start_date,train_end_date - h, by = "day")
    C0_select = C0_raw %>% filter(date %in% train_run_date)
    training_overall= cov_lag_final_df %>% filter(date %in% train_run_date)
    
    epi_vec = c("scaled_case","ct_sm_7")
    stringency<<-c("StringencyIndex_WeightedAverage","GovernmentResponseIndex_WeightedAverage",
                   "ContainmentHealthIndex_WeightedAverage")
    meteorology<<- c("temperature","humidity","wind_speed")
    pollutant <<-c("ozone","nitrogen_dioxide","nitrogen_oxides","carbon_monoxide",
                   "fine_suspended_particulates",'respirable_suspended_particulates',"sulphur_dioxide") 
    
    
    
    if(modelname  %in% c("arima","garch","gam","lm","svr","lasso","gpr")){
      reduced_vars = get_reduced_vars_single(C0_select, c("scaled_case","ct_sm_7"),meteorology,pollutant,stringency)
      vars = c(reduced_vars)
    }else if(modelname  %in% c("x")){
      reduced_vars = get_reduced_vars_tillOptimal(C0_select, c("log_case","ct_sm_7"),meteorology,pollutant,stringency)
      vars = c(reduced_vars)
    }else{
      vars = names(training_overall[,-c(1:5)])
    }
    
    vars_final = vars
    #print(vars_final)
    
    # Check if any column contains -Inf
    has_inf <- apply(cov_lag_final_df, 2, function(x) any(x == -Inf))
    # Get the column names with -Inf values
    inf_columns <- colnames(cov_lag_final_df)[has_inf]
    vars_noinf = vars_final[!vars_final %in% inf_columns]
    vars_final =  vars_noinf # c(vars,"true_rt_lagahead_horizon_case")
    
      
    ###########----------------------------------train --------------------------------------
    training_overall = training_overall %>%  drop_na()
    training.set.date = training_overall %>% 
      dplyr::select(date,y_pred,any_of(vars_final)) %>% drop_na()
    
    #print(tail(training.set.date,1))
    training.set = training.set.date %>% dplyr::select(y_pred,any_of(vars_final))
    
    nowcast_model = model_fit(training.set,vars_final)
    train_dat = training.set %>% dplyr::select(-y_pred)
    
    if(modelname %in% c("ridge","lasso")){
      nowcast_res =  predict(nowcast_model, s = "lambda.min", newx = as.matrix(train_dat))%>% as.numeric()
    }else if(modelname %in% c("arima","garch")){
      nowcast_res = fitted(nowcast_model)
    }else{
      nowcast_res =  predict(nowcast_model,train_dat)
    }
    
    fit_case_raw = nowcast_res
  
    
    
    train_res = data.frame(mytestdate = training.set.date$date,
                           pred = as.numeric(nowcast_res),
                           true = training_overall$log_case,
                           modelname = modelname,
                           proj = h,
                           date = training.set.date$date +h)
    
    train_res_all = rbind(train_res_all,train_res)
  }

  write.csv(train_res_all,paste("./train_choose_ws/",modelname,"_train.csv",sep = ""),row.names = F)
  return(train_res_all)
}




ahead_horizon_case = 14
### train res
model_name_list = c("arima","garch","gam","lm","svr","gpr")
for (modelname in model_name_list){
  print(modelname)
  get_train_beforetest(modelname)
}


const_train_res = read.csv("./train_choose_ws/lm_train.csv") %>% 
  mutate(pred = true,
         modelname = "const")
write.csv(const_train_res,"./train_choose_ws/const_train.csv")
