library(lubridate)
library(tidyverse)
library(neuralnet)
library(nnet)
library(caret)
library(tidyverse)
library(forecast)
library(data.table)
library(rlist)
#library(garimanet)
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

sourceCpp("./rt.cpp")
file.sources = list.files( c("./functions","./run_model/models"),
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)
#
#
# save_all_reportcase = read_rds("./pred_reportcase_point/lm_reportcase_extrapo.rds")
# save_all_reportcase$pred_reportcase = save_all_reportcase$const_reportcase
# saveRDS(save_all_reportcase,"./pred_reportcase_point/baseline_reportcase_extrapo.rds")
# 

test_start_period  = seq(as.Date("2022-05-20"),as.Date("2022-12-31"),by = "day")

ride_bp_rt_pred14 <- readRDS("./data/ride_bp_rt_pred14.rds")
true_rt_dat <- readRDS("./data/true_rt_dat.rds")
test_rt_res_all = data.frame()


modelname = "const"
save_all_reportcase = read_rds("./pred_reportcase_point/const_reportcase_extrapo.rds")

for (t in 1:length(test_start_period)){
  mydate = test_start_period[t]
  mydate_dt = save_all_reportcase %>%
    filter(mytestdate == mydate)

  predict_reportcase = mydate_dt$pred_reportcase
  pred_infs = BP_deconv_fun(predict_reportcase)
  data_to_calRt = data.frame(date = mydate_dt$date, pred_infs = pred_infs) %>%
    filter(row_number() <= n()-1)


  print("-------------------------")
  print(mydate)
  rt_dataout = Rt_calc_fun(data_to_calRt %>% tail(50))



  ## look at 14 days
  max_prediction = 14
  test_period = seq(as.Date(mydate), as.Date(mydate)+ max_prediction, by = "day")        ## mydate_dt$date[mydate_dt$type == "test"]

  test_rt_res = rt_dataout %>%
    filter(date %in% test_period) %>%
    mutate(date_analysis = as.Date(mydate), proj = 0:max_prediction,  modelname = modelname) %>%
    dplyr::rename("pred_rt" = "local.rt.mean")

  print(as.Date(mydate))

  test_rt_res_cbind= get_combine_pred_df(test_rt_res,ride_bp_rt_pred14,true_rt_dat)
  print(rbind(get_error_df(test_rt_res_cbind),get_error_bp_df(test_rt_res_cbind)))
  test_rt_res_all = rbind(test_rt_res_all,test_rt_res)
}

saveRDS(test_rt_res_all,paste("./pred_rt_point/",modelname,"_pred_rt_extrapo.rds",sep = ""))

test_rt_res_all_cbind= get_combine_pred_df(test_rt_res_all,ride_bp_rt_pred14,true_rt_dat)
print(rbind(get_error_df(test_rt_res_all_cbind),get_error_bp_df(test_rt_res_all_cbind)))
print(rbind(get_error_final_df(test_rt_res_all_cbind),get_BPtemp_error_final_df(test_rt_res_all_cbind)))



# #
# train_all_reportcase = read_rds("./pred_reportcase_point/lm_reportcase_extrapo.rds") %>% filter(type == "train") %>% 
#   filter(mytestdate  %in% seq(as.Date("2022-06-01"),as.Date("2022-12-31"),by = "day")) %>% 
#   dplyr::select(-const_reportcase)
# 
# test_const_reportcase = read.csv("./pred_reportcase_point/const_reportcase_point_onlytest.csv") %>% 
#   dplyr::select(-proj,-modelname) %>% 
#   dplyr::rename("true_reportcase" = "report_case")
# save_all_reportcase = rbind(train_all_reportcase,test_const_reportcase) %>% 
#   mutate(modelname = "const")
# saveRDS(save_all_reportcase,"./pred_reportcase_point/const_reportcase_extrapo.rds")

