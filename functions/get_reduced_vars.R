

get_reduced_vars_single = function(C0_raw, epi_vec, meteorology,pollutant,stringency){
  mypredictors = c(epi_vec,meteorology,pollutant,stringency)
  best_lag_raw = obtain_best_lag(C0_raw,mypredictors) %>% arrange(desc(abs(pearson))) 
  ## best_lag2 = best_lag_raw %>% head(3)  ## %>% filter(abs(pearson)>=0.5)
  
  best_lag_case_ct = best_lag_raw %>% filter(var %in% epi_vec)
  best_lag_meteo = best_lag_raw %>% filter(var%in% meteorology) %>% head(1)
  best_lag_pollu = best_lag_raw %>% filter(var%in% pollutant) %>% head(1)
  best_lag_policy= best_lag_raw %>% filter(var%in% stringency) %>% head(1)
  best_lag2 = rbind(best_lag_meteo,best_lag_pollu,best_lag_policy,best_lag_case_ct) %>% filter(pearson !=0)
  
  best_lag = best_lag2
  
  cov_lag= obtain_data_bestlag(best_lag,C0_raw) %>% drop_na()
  # cov_lag_1 = cov_lag[,-c(1:2)]
  # 
  # hc = findCorrelation(cor(cov_lag_1),cutoff = 0.98)
  # if(length(hc)>=1){
  #   cov_lag_all = cbind(cov_lag[,1:2],cov_lag_1[,-hc] )
  # }else{
  #   cov_lag_all = cov_lag
  # }
  cov_lag_all = cov_lag
  reduced_vars = names(cov_lag_all)[-c(1:2)]
  #print(reduced_vars) 
  return(reduced_vars)
}





get_reduced_vars_more= function(C0_raw, epi_vec, meteorology,pollutant,stringency){
  mypredictors = c(epi_vec,meteorology,pollutant,stringency)
  best_lag_raw = obtain_best_lag(C0_raw,mypredictors) %>% arrange(desc(abs(pearson))) 
  ## best_lag2 = best_lag_raw %>% head(3)  ## %>% filter(abs(pearson)>=0.5)
  
  best_lag_case_ct = best_lag_raw %>% filter(var %in% epi_vec)
  best_lag_meteo = best_lag_raw %>% filter(var%in% meteorology)
  best_lag_pollu = best_lag_raw %>% filter(var%in% pollutant) 
  best_lag_policy= best_lag_raw %>% filter(var%in% stringency) 
  best_lag2 = rbind(best_lag_meteo,best_lag_pollu,best_lag_policy,best_lag_case_ct) %>% filter(pearson !=0)
  best_lag = best_lag2 
  
  cov_lag= obtain_data_bestlag(best_lag,C0_raw) %>% drop_na()
  cov_lag_1 = cov_lag[,-c(1:2)]
  
  hc = findCorrelation(cor(cov_lag_1),cutoff = 0.9)
  if(length(hc)>=1){
    cov_lag_all = cbind(cov_lag[,1:2],cov_lag_1[,-hc] )
  }else{
    cov_lag_all = cov_lag
  }
  
  reduced_vars = names(cov_lag_all)[-c(1:2)]
  #print(reduced_vars) 
  return(reduced_vars)
}



get_reduced_vars_tillOptimal = function(C0_raw, epi_vec, meteorology, pollutant, stringency){
  mypredictors = c(epi_vec,meteorology,stringency)
  best_lag_raw = obtain_best_lag(C0_raw,mypredictors) %>% arrange(desc(abs(pearson))) 
  best_lag_case_ct = best_lag_raw %>% filter(var %in% epi_vec)
  best_lag_meteo = best_lag_raw %>% filter(var%in% meteorology) %>% head(1)
  best_lag_pollu = best_lag_raw %>% filter(var%in% pollutant) %>% head(1)
  best_lag_policy= best_lag_raw %>% filter(var%in% stringency) %>% head(1)
  best_lag2 = rbind(best_lag_meteo,best_lag_policy,best_lag_case_ct) %>% filter(pearson !=0)
  
  best_lag = best_lag2
  cov_vars = c()
  for(i in 1:nrow(best_lag)){
    var = best_lag$var[i]
    lag = best_lag$lag[i]
    cov_vars = c(cov_vars,str_c(var, "_lag",1:lag))
  }

  reduced_vars = cov_vars
  return(reduced_vars)
}




get_reduced_vars_topcor = function(C0_raw, epi_vec, meteorology, pollutant, stringency){
  mypredictors = c(epi_vec,meteorology,pollutant,stringency)
  best_lag_raw = obtain_alllag(C0_raw,mypredictors) %>% arrange(desc(abs(pearson))) 
  
  best_lag = best_lag_raw %>% 
    filter(abs(pearson)>=0.5) %>% 
    arrange(desc(abs(pearson))) %>% 
    slice_head(n = 20)
          
  cov_lag= obtain_data_bestlag(best_lag,C0_raw) %>% drop_na()
  cov_lag_1 = cov_lag[,-c(1:2)]
  
  hc = findCorrelation(cor(cov_lag_1),cutoff = 0.95)
  if(length(hc)>=1){
    cov_lag_all = cbind(cov_lag[,1:2],cov_lag_1[,-hc] )
  }else{
    cov_lag_all = cov_lag
  }
  
  reduced_vars = names(cov_lag_all)[-c(1:2)]
  return(reduced_vars)
}

