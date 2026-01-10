library(lubridate)
library(tidyverse)
library(neuralnet)
library(nnet)
library(caret)
library(tidyverse)
library(forecast)
library(data.table)
library(rlist)
library(kernlab)
library(e1071)
library(mgcv)
library(pROC)
library(zoo)
library(rugarch)
library(mvtnorm)
library(xgboost)
library(surveillance)
library(Rcpp)
library(RcppParallel)

file.sources = list.files( c("./functions","./run_model/models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)


Fit_functions = list(nn = nn_fit,xgb = xgb_fit,xgb = xgb_fit,gbm = gbm_fit,
                     xgb = xgb_fit, xgb = xgb_fit,svr_L = svr_L_fit,xgb = xgb_fit)

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
test_start_period <<- seq(as.Date("2022-05-28"),as.Date("2022-12-31"),by = "day")
ahead_horizon_case = 14


modelname <<- "xgb"
run_model = function(modelname){
  modelname <<- modelname
  model_fit = Fit_functions[[modelname]]
  print(modelname)
  test_start_period = seq(as.Date("2022-05-28"),as.Date("2022-12-31"),by = "3 weeks")
  res = NULL
  save_train_res = NULL
  coef_all_df = NULL
  m =1
  
  save_all_reportcase = data.frame()
  test_reportcase_all = data.frame()
  
  for(m in 1:length(test_start_period)){
    print("-----------------------")
    print(test_start_period[m])
    
    test_start_date = test_start_period[m]
    test_end_date = test_start_date + ahead_horizon_case
    train_end_date <- test_start_date -1
    train_start_date <-as.Date("2022-02-15")
    
    
    dt_case = data_case %>% filter(date>= train_start_date & date<test_start_date)
    dt_case2 = dt_case 
    
    ## observed_data till 0527
    observed_data  = left_join(dt_case2,other_cov,by = "date") 
    raw_data = observed_data 
    
    test_pred = c()
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
      
      
      
      if(modelname  %in% c("xx")){
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
      
      
      
      ###########----------------------------------train -------------------------------------
      training_overall = training_overall %>%  drop_na()
      training.set.date = training_overall %>% 
        dplyr::select(date,y_pred,any_of(vars_final)) %>% drop_na()
      
      #print(tail(training.set.date,1))
      training.set = training.set.date %>% dplyr::select(y_pred,any_of(vars_final))
      
      nowcast_model = xgb_fit(training.set,vars_final)
      train_dat = training.set %>% dplyr::select(-y_pred)
      
      nowcast_res =  predict(nowcast_model,as.matrix(train_dat))
      fit_case_raw = nowcast_res 
      
      data.frame(training_overall$log_case,fit_case_raw)
      data.frame(exp(training_overall$log_case),exp(fit_case_raw))
      
      
      
      
      
      # coef_df = varImp(nowcast_model)$importance %>%
      #   dplyr::rename("coef" = "Overall")
      # coef_df$var = rownames(coef_df)
      # coef_df$modelname = modelname
      # coef_df$proj = h
      # coef_df$mytestdate = test_start_date
      # coef_df = coef_df %>% arrange(desc(coef)) %>%  head(50)
      # coef_all_df = rbind(coef_all_df,coef_df)
      
      varimp = xgb.importance(feature_names = vars_final, model = nowcast_model) %>% as.data.frame()
      coef_df = varimp %>%
        dplyr::rename("var" = "Feature","coef" = "Frequency")

      coef_df$var = rownames(coef_df)
      coef_df$modelname = modelname
      coef_df$proj = h
      coef_df$mytestdate = test_start_date
      coef_df = coef_df %>% arrange(desc(coef)) %>%  head(50)
      coef_all_df = rbind(coef_all_df,coef_df)
      
      
      # #############################################################################
      # # -----------------------------test  of forecast ----------------------------
      # #############################################################################
      test_date = as.Date(test_start_date)
      testing_set_date = cov_lag_final_df %>% filter(date %in% test_start_date)
      newdata_x = testing_set_date[,vars_final] %>% as.matrix()
      test_h = predict(nowcast_model,newdata_x)
      test_pred = c(test_pred,test_h)
    }
    
    
    forecast_date = seq(test_start_date,test_start_date+ahead_horizon_case, by = "day")
    test_reportcase= data.frame(mytestdate = test_start_date,
                                date = test_start_date + c(0:ahead_horizon_case),
                                pred_reportcase = exp(test_pred),
                                true_reportcase = true_dat[true_dat$date %in% forecast_date,]$report_case,
                                proj = 0:ahead_horizon_case,
                                const_reportcase = raw_data$report_case[raw_data$date == train_end_date])
    
    rmse_df =
      test_reportcase %>%
      group_by(proj) %>%
      summarise(rmse1 = rmse(pred_reportcase,true_reportcase),
                rmse2 = rmse(const_reportcase,true_reportcase)) %>%
      ungroup() %>%
      as.data.frame()
    
    print(paste("rmse of pred: "))
    print(rmse_df)
    
    test_reportcase_all = rbind(test_reportcase_all, test_reportcase)
  }
  
  write.csv(coef_all_df,paste("./train_coef/",modelname,"_coef_all.csv",sep = ""),row.names = F)
  write.csv(test_reportcase_all,paste("./pred_reportcase_point/",modelname,"_reportcase_point.csv",sep = ""),row.names = F)
  return(test_reportcase_all)
}



xgb_reportcase_extrapo = run_model("xgb")
xgb_reportcase_extrapo = read.csv("./pred_reportcase_point/xgb_reportcase_point.csv") %>% 
  mutate(date = as.Date(date),
         mytestdate = as.Date(mytestdate))

test = xgb_reportcase_extrapo %>% 
  mutate(diff = (pred_reportcase-true_reportcase)/true_reportcase) %>% 
  mutate(proj = as.numeric(date - mytestdate)) %>% 
  filter(mytestdate >= "2022-05-25")


test %>% filter(proj<=7) %>% 
  dplyr::summarise(v1 = rmse(pred_reportcase,true_reportcase),
                   v2 = rmse(const_reportcase,true_reportcase),
                   rel_value = v1/v2) %>% 
  as.data.frame()



test %>% 
  group_by(proj) %>% 
  dplyr::summarise(v1 = rmse(pred_reportcase,true_reportcase),
                   v2 = rmse(const_reportcase,true_reportcase),
                   rel_value = v1/v2) %>% 
  as.data.frame()

test %>% 
  ggplot(aes(x = date))+
  geom_line(aes(y = pred_reportcase,group = mytestdate ,color = factor(proj)),lwd = 0.8)+
  geom_line(aes(y = true_reportcase), lwd = 0.8,color = "darkgrey")+
  scale_x_date(date_labels = "%m")+
  theme_bw()+
  labs(
    y = "Case"
  )
