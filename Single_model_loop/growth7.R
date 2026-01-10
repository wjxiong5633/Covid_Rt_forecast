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
dates_analysis = seq(as.Date("2022-05-25"),as.Date("2022-12-31"),by="day")

# number of removed data points (not consolidated) # for HK 1 day delay is OK
remove_last_n <- 1
# maximum prediction horizon
max_prediction_horizon<-14+remove_last_n
date_max<-max(dates_analysis)+max_prediction_horizon-remove_last_n
model_name = "growth7"

library(msm)

# function taken from https://github.com/sbfnk/covid19.forecasts.uk 
null_model_forecast_quantiles <- function(values, horizon, truncation = 0)
{
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

remove_last_n = 1
regression_window = 7

for (iDate in seq_along(dates_analysis)) {
  #iDate = 1
  print(dates_analysis[iDate])
  dat_train<-data_rt %>%
    filter(date_analysis==dates_analysis[iDate]-1)  %>%
    as.data.frame() 
  
  
  # estimate growth rate
  
  tmp<-dat_train[which(dat_train$date>=dates_analysis[iDate]-remove_last_n-regression_window+1),]
  mod<-glm(confirm~date,data=tmp,family=quasipoisson(link="log"))
  r_hat <- as.numeric(mod$coefficients["date"])
  se_r <- sqrt(vcov(mod)["date", "date"])
  
  # Bootstrap r from normal distribution
  n_boot <- 1000
  r_boot <- rnorm(n_boot, mean = r_hat, sd = se_r)
  
  t <- 1:max_prediction_horizon
  last_confirm <- dat_train[nrow(dat_train), "confirm"]
  pred_matrix <- sapply(r_boot, function(r_i) last_confirm * exp(r_i * t))
  
  # Define quantile probabilities
  quant_probs <-  c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  
  # Compute quantiles for each row (forecast horizon)
  quant_matrix <- t(apply(pred_matrix, 1, function(x) quantile(x, probs = quant_probs)))
  
  # Define column names
  colnames(quant_matrix)<-c(paste0("lower_",c(2,5,seq(10,90,by=10))),"point",paste0("upper_",rev(c(2,5,seq(10,90,by=10)))))

  
  res_tmp <- data.frame(
    date = dates_analysis[iDate],
    prediction_horizon = seq(1,max_prediction_horizon)-remove_last_n,
    quant_matrix
  )
  
  res <- rbind(res, res_tmp)
}


model = 'growth7'
res1 <- res %>%
  mutate(date_analysis=as.Date(date)) %>%
  mutate(date = date_analysis+prediction_horizon,
         proj = prediction_horizon) %>%
  mutate(modelname=model,
         pred_reportcase = point) %>% 
  dplyr::rename("mytestdate" = "date_analysis") %>% 
  dplyr::select(-prediction_horizon)



data_case = read_rds("./data/data_case.rds")
true_case_dat = data_case
res2 = res1 %>% left_join(true_case_dat,by = "date") %>% 
  mutate(pred_logcase = log(pred_reportcase),
         true_logcase = log(report_case),
         true_reportcase = report_case)

write.csv(res2, './pred_reportcase_interval/interval_growth7.csv',row.names = F)


res3 = res2 %>% dplyr::select(date,mytestdate,proj,pred_reportcase,modelname,true_reportcase) %>% 
  mutate(type = "test")
write.csv(res3, './pred_reportcase_point/growth7_reportcase_point.csv',row.names = F)


