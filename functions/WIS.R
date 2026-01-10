my.compute = function(df) {
  res<-df %>%
    mutate(in_interval95 = true >= lower_5 & true <= upper_5,
           in_interval50 = true >= lower_50 & true <= upper_50) %>%
    mutate(abs_error = abs(point - true)) #%>% 
  
  # mutate(bias = abs((point - true)/true))
  
  # WIS 
  res$IS_2<-scoringutils:::interval_score(observed = res$true,
                           lower=res$lower_2,
                           upper=res$upper_2,
                           interval_range=98,
                           weigh = TRUE,
                           separate_results = FALSE)
  
  res$IS_5<-scoringutils:::interval_score(observed = res$true,
                           lower=res$lower_5,
                           upper=res$upper_5,
                           interval_range=95,
                           weigh = TRUE,
                           separate_results = FALSE)
  
  res$IS_10<-scoringutils:::interval_score(observed = res$true,
                            lower=res$lower_10,
                            upper=res$upper_10,
                            interval_range=90,
                            weigh = TRUE,
                            separate_results = FALSE)
  
  res$IS_20<-scoringutils:::interval_score(observed = res$true,
                            lower=res$lower_20,
                            upper=res$upper_20,
                            interval_range=80,
                            weigh = TRUE,
                            separate_results = FALSE)
  
  res$IS_30<-scoringutils:::interval_score(observed = res$true,
                            lower=res$lower_30,
                            upper=res$upper_30,
                            interval_range=70,
                            weigh = TRUE,
                            separate_results = FALSE)
  
  res$IS_40<-scoringutils:::interval_score(observed = res$true,
                            lower=res$lower_40,
                            upper=res$upper_40,
                            interval_range=60,
                            weigh = TRUE,
                            separate_results = FALSE)
  
  res$IS_50<-scoringutils:::interval_score(observed = res$true,
                            lower=res$lower_50,
                            upper=res$upper_50,
                            interval_range=50,
                            weigh = TRUE,
                            separate_results = FALSE)
  
  res$IS_60<-scoringutils:::interval_score(observed = res$true,
                            lower=res$lower_60,
                            upper=res$upper_60,
                            interval_range=40,
                            weigh = TRUE,
                            separate_results = FALSE)
  
  res$IS_70<-scoringutils:::interval_score(observed = res$true,
                            lower=res$lower_70,
                            upper=res$upper_70,
                            interval_range=30,
                            weigh = TRUE,
                            separate_results = FALSE)
  
  res$IS_80<-scoringutils:::interval_score(observed = res$true,
                            lower=res$lower_80,
                            upper=res$upper_80,
                            interval_range=20,
                            weigh = TRUE,
                            separate_results = FALSE)
  
  res$IS_90<-scoringutils:::interval_score(observed = res$true,
                            lower=res$lower_90,
                            upper=res$upper_90,
                            interval_range=10,
                            weigh = TRUE,
                            separate_results = FALSE)
  
  res$IS_100<-abs(res$point-res$true)
  
  res <- res %>%
    mutate(wis = (IS_2+IS_5+IS_10+IS_20+IS_30+IS_40+IS_50+
                    IS_60+IS_70+IS_80+IS_90+0.5*IS_100)/11.5 )  
  
  return(res)
}

