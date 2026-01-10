

obtain_alllag = function(C0_raw,mypredictors){
  mypredictors = mypredictors 
  res_lag = tibble()
  for(i in c(1:length(mypredictors))){  ## each var
    for (lag in c(1:7)){
      sub1 = C0_raw
      sub2 = sub1
      sub2$date<-sub2$date + lag
      sub<-left_join(sub1[,c("date","y_pred")],sub2[,c("date",mypredictors[i])], by = c("date"),relationship = "many-to-many") %>% as.data.frame()
      cor<-cor.test(y=sub$y_pred,x=sub[,mypredictors[i]],method="pearson")$estimate
      res_lag_tmp<-data.frame(pearson=cor,lag=lag,var=mypredictors[i])
      res_lag<-bind_rows(res_lag,res_lag_tmp)
    }
  }
  
  all_lag<-res_lag %>% 
    arrange(desc(abs(pearson))) %>% 
    as.data.frame()
  
  return(all_lag)
}


obtain_best_lag = function(C0_raw,mypredictors){
  mypredictors = mypredictors 
  res_lag = tibble()
  for(i in c(1:length(mypredictors))){  ## each var
    for (lag in c(1:7)){
      sub1 = C0_raw
      sub2 = sub1
      sub2$date<-sub2$date + lag
      sub<-left_join(sub1[,c("date","y_pred")],sub2[,c("date",mypredictors[i])], by = c("date"),relationship = "many-to-many") %>% as.data.frame()
      cor<-cor.test(y=sub$y_pred,x=sub[,mypredictors[i]],method="pearson")$estimate
      res_lag_tmp<-data.frame(pearson=cor,lag=lag,var=mypredictors[i])
      res_lag<-bind_rows(res_lag,res_lag_tmp)
    }
  }
  
  best_lag<-res_lag %>% 
    group_by(var) %>% 
    dplyr::slice(which.max(abs(pearson))) %>% 
    as.data.frame() %>% 
    arrange(desc(abs(pearson)))
  
  return(best_lag)
}



obtain_data_alllag = function(data_raw,mypredictors){
  # stringency<<-c("StringencyIndex_WeightedAverage","GovernmentResponseIndex_WeightedAverage",
  #                "ContainmentHealthIndex_WeightedAverage","EconomicSupportIndex")
  mypredictors = mypredictors
  dt_update = data_raw[,c("date","y_pred")]
  for (i in 1: length(mypredictors)){
    #print(i)
    for(k in 1:7){
      dt_var<-data_raw[,c("date",mypredictors[i])]
      dt_var2 = dt_var
      dt_var2$date<-dt_var$date +  k
      colnames(dt_var2) = c("date",paste(mypredictors[i],"_lag",k,sep = ""))
      dt_update<-full_join(dt_update,dt_var2,by=c("date"))
    }
  }
  other_var_final = dt_update  ## for example 301, lag = 7, data start from 308
  
  return(other_var_final)
}




obtain_data_bestlag = function(best_lag, data_raw){
  mypredictors = unique(best_lag$var)#c("temp_rt",meteorology,stringency)
  dt_update = data_raw[,c("date","y_pred")]
  
  for (i in 1: length(mypredictors)){
    dt_var<-data_raw[,c("date",mypredictors[i])]
    dt_var2 = dt_var
    k_vec= best_lag$lag[best_lag$var == mypredictors[i]]
    for(k in k_vec){
      dt_var2$date<-dt_var$date + k 
      colnames(dt_var2) = c("date",paste(mypredictors[i],"_lag",k,sep = ""))
      dt_update<-left_join(dt_update,dt_var2,by=c("date"),relationship = "many-to-many")
    }
  }
  other_var_final = dt_update  ## for example 301, lag = 7, data start from 308
  
  return(other_var_final)
}


