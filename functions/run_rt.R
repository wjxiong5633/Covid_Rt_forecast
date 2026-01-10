library(Rcpp)
library(RcppParallel)
library(chron)
library(mvtnorm)
library(incidental)
library(surveillance)


########
## function to compute the mcmc output
para_summary <- function(mcmc,a,b,print){
  y <- matrix(NA,ncol(mcmc),4)
  for (i in 1:ncol(mcmc)){
    y[i,1:3] <- quantile(mcmc[,i],c(0.5,0.025,0.975))
    y[i,4] <- sum(diff(mcmc[,i])!=0)/nrow(mcmc)
  }
  y[i,1] <- mean(mcmc[,i])
  layout(matrix(1:(a*b),nrow=a,byrow=T))
  par(mar=c(2,4,1,1))
  if (print==1){
    for (i in 1:ncol(mcmc)){
      plot(mcmc[,i],type="l")
    }
  }
  return(y)
}

# 
# run_rt_ride = function(data1){
#   meanvec <- c(1.4341,0.6613)
#   incpara <- meanvec
#   incvec <- (plnorm(1:20,incpara[1],incpara[2])-plnorm(0:19,incpara[1],incpara[2]))/plnorm(20,incpara[1],incpara[2])
#   ## infectiousness, shifted gamma distribution f_c(x) = f(x+c) with parameters 
#   infpara <- c(20.52,1.59,12.27)
#   infvec <- (pgamma(1:40,infpara[1],infpara[2])-pgamma(0:39,infpara[1],infpara[2]))
#   
#   w_dis1 <- rep(0,20)
#   for (i in 1:20){
#     for (j in 1:40){
#       if (i+j-12>0 &(i+j-12<=20)){  
#         w_dis1[i+j-12] <- w_dis1[i+j-12] + incvec[i]*infvec[j]  
#       }
#     }  
#   }
#   w_dis2 <- w_dis1
#   w_dis2[1:5] <- 0
#   w_dis <- cbind(w_dis2/sum(w_dis2),w_dis1/sum(w_dis1))
#   
#   
#   p_num = c(1,1,5,5,10,10,15,15,10,10,5,5,1,1)
#   prop <<- p_num/sum(p_num)
#   
#   delay_dist = prop
#   ### Get incidence
#   incidence_model = fit_incidence(
#     reported = data1$report_case,
#     delay_dist = delay_dist
#   )
# 
#   inci_dat = matrix(0,nrow(data1),4)
#   inci_dat[,3] <- incidence_model$Ihat
# 
# 
#   inci_dat1 <- inci_dat
#   for (i in 1:nrow(inci_dat)){
#     for (j in 1:ncol(inci_dat)){
#       inci_dat1[i,j] <- round(inci_dat[i,j])
#     }
#   }
# 
#   smooth <- 7
#   startpt <- 24
# 
#   # first 10 is to resevere for parameters other than Rt
#   para <- c(rep(0,10),rep(1,nrow(data1)-startpt+1))
#   # for para2, also restricted to <= 153
#   para2 <- c(0.1)
#   move <- rep(1,length(para))
# 
#   #move[1:(length(para)-40)] <- 0
#   move[1:10] <- 0
#   para[1:10] <- 0
# 
#   sigma <- (abs(para)+0.1)/5
#   sigma2 <- (abs(para2)+0.1)/5
# 
#   #sourceCpp("rt.cpp")
#   aaaaa1 <- Sys.time()
#   tt <- mcmc(inci_dat1,para,para2,w_dis,smooth,startpt,10000,move,sigma,sigma2)
#   aaaaa2 <- Sys.time()
#   print(aaaaa2-aaaaa1)
# 
#   inc <- 1000+1:9000
#   z1 <- para_summary((tt[[1]][inc,]),4,3,0)
#   print(z1)
#   
# 
#   data2 <- data1[24:nrow(data1),] ## 14 day more
#   
#   rest <- z1
#   totaln <- nrow(rest)-10
#   data2[1:nrow(data2),c("rt1","rt1.1","rt1.2")] <- rest[10+1:totaln,1:3]
#   
#   dataout <- data2[,c("date","rt1","rt1.1","rt1.2")]
#   names(dataout) <- c("data.onset","local.rt.mean","local.rt.lower","local.rt.upper")
#   dataout$date <- as.Date(as.character(dataout$data.onset))
#   dataout <- dataout[!is.na(dataout$local.rt.mean),]
#   return(dataout)
# }






run_rt_true = function(data1){
  ##delay <- read.csv("./decon/delay_Omicron.csv") 
  ## here sample the incubation meanlog and sdlog
  meanvec <- c(1.07,0.65)
  incpara <- meanvec
  incvec <- (plnorm(1:20,incpara[1],incpara[2])-plnorm(0:19,incpara[1],incpara[2]))/plnorm(20,incpara[1],incpara[2])
  ## infectiousness, shifted gamma distribution f_c(x) = f(x+c) with parameters 
  infpara <- c(20.52,1.59,12.27)
  infvec <- (pgamma(1:40,infpara[1],infpara[2])-pgamma(0:39,infpara[1],infpara[2]))
  
  w_dis1 <- rep(0,20)
  for (i in 1:20){
    for (j in 1:40){
      if (i+j-12>0 &(i+j-12<=20)){  
        w_dis1[i+j-12] <- w_dis1[i+j-12] + incvec[i]*infvec[j]  
      }
    }  
  }
  w_dis2 <- w_dis1
  w_dis2[1:5] <- 0
  w_dis <- cbind(w_dis2/sum(w_dis2),w_dis1/sum(w_dis1))
  
  data11 = matrix(0,nrow(data1),4)
  
  ### 
  data11[,3] <- data1$newcase
  
  smooth <- 7
  startpt <- 24
  
  # first 10 is to resevere for parameters other than Rt
  para <- c(rep(0,10),rep(1,nrow(data1)-startpt+1))
  # for para2, also restricted to <= 153
  para2 <- c(0.1)
  move <- rep(1,length(para))
  
  #move[1:(length(para)-40)] <- 0
  move[1:10] <- 0
  para[1:10] <- 0
  
  sigma <- (abs(para)+0.1)/5
  sigma2 <- (abs(para2)+0.1)/5
  
  #sourceCpp("rt.cpp")
  aaaaa1 <- Sys.time()
  tt <- mcmc(data11,para,para2,w_dis,smooth,startpt,10000,move,sigma,sigma2)
  aaaaa2 <- Sys.time()
  print(aaaaa2-aaaaa1)
  
  inc <- 1000+1:9000
  z1 <- para_summary((tt[[1]][inc,]),4,3,0)
  
  
  data2 <- data1[24:nrow(data1),] ## 14 day more
  
  rest <- z1
  totaln <- nrow(rest)-10
  data2[1:nrow(data2),c("rt1","rt1.1","rt1.2")] <- rest[10+1:totaln,1:3]
  
  dataout <- data2[,c("date","rt1","rt1.1","rt1.2")]
  names(dataout) <- c("data.onset","local.rt.mean","local.rt.lower","local.rt.upper")
  dataout <- dataout[!is.na(dataout$local.rt.mean),]
  dataout$date = dataout$data.onset
  print(dataout)
  return(dataout)
}

