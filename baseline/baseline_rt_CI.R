####################
library(plyr)
library(dplyr)
library(tidyverse)
library(ISOweek)
library(locpol)
library(forecast)
library(scoringutils)
library(tseries)
library(cowplot)
library(tidyr)


data_rt<-readRDS("./Data/data_full_new_bp.rds")
dates_analysis = seq(as.Date("2022-06-01"),as.Date("2022-12-31"),by="day")

# number of removed data points (not consolidated) # for HK 1 day delay is OK
remove_last_n <- 1
# maximum prediction horizon
max_prediction_horizon<-14+remove_last_n
date_max<-max(dates_analysis)+max_prediction_horizon-remove_last_n
model_name = 'const1'

library(msm)

# function taken from https://github.com/sbfnk/covid19.forecasts.uk 
null_model_forecast_quantiles <- function(values, horizon, truncation = 0)
{
  values = dat_train$rt_temp
  horizon = max_prediction_horizon
  truncation = 0
  
  if (truncation > 0) {
    values <- head(values, -truncation)
  }
  
  tnorm_unc_fit <- function(x, mean, true) {
    return(-sum(dtnorm(x = true, mean = mean, sd = x, lower = 0, log = TRUE)))
  }
  
  quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  ts <- tail(values, min(horizon, length(values)))
  
  
  
  if (length(ts) > 1) {
    ## null model
    null_ts <- rep(last(ts), horizon + truncation)
    interval <- c(0, max(abs(diff(ts))))
    
    if (diff(interval) > 0 ) {
      
      ## get sigma
      tnorm_sigma <-
        optimise(tnorm_unc_fit, interval = interval,
                 mean = head(ts, -1),
                 true = tail(ts, -1))
      
      quant <- qtnorm(p = quantiles, mean = last(ts),
                      sd = tnorm_sigma$minimum, lower = 0)
    } else {
      quant <- rep(last(ts), length(quantiles))
    }
  } else {
    quant <- rep(last(ts), length(quantiles))
  }
  
   #return the true inverse-log rate
  names(quant) <- quantiles
  
  return(quant)
}


library(tibble)
res <- tibble()
library(dplyr)

for (iDate in seq_along(dates_analysis)) {
  #iDate = 1
  print(dates_analysis[iDate])
  dat_train<-data_rt %>%
    filter(date_analysis==dates_analysis[iDate]-1)  %>%
    as.data.frame()
  
  quant <-
    null_model_forecast_quantiles(dat_train$rt_temp, horizon = max_prediction_horizon,
                                  truncation = 0)
  
  names(quant)<-c(paste0("lower_",c(2,5,seq(10,90,by=10))),"point",paste0("upper_",rev(c(2,5,seq(10,90,by=10)))))
  
  
  res_tmp <- data.frame(
    date = dates_analysis[iDate],
    prediction_horizon = seq(1,max_prediction_horizon)-remove_last_n,
    t(quant)
  )
  res <- rbind(res, res_tmp)
}


model = 'const1'
res1 <- res %>%
  mutate(date_analysis=as.Date(date)) %>%
  mutate(date = date_analysis+prediction_horizon,
         proj = prediction_horizon) %>%
  mutate(modelname=model,
         pred_rt = point) %>% 
  dplyr::select(-prediction_horizon)

true_rt_dat <- readRDS("./data/true_rt_dat.rds")
res2 = res1 %>% left_join(true_rt_dat,by = "date")
write.csv(res2, './pred_rt_interval/interval_const1.csv',row.names = F)


res3 = res2 %>% dplyr::select(date,date_analysis,proj,pred_rt,modelname,true_rt)
write.csv(res3, './pred_rt_point/const1_pred_rt_point.csv',row.names = F)


