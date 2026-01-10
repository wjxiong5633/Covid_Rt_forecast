


Rt_calc_fun = function(df){
  
  data22 = matrix(0,nrow(df),4)
  data22[,3] = df$pred_infs
  
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
  
  
  
  smooth <- 14
  startpt <- 24
  
  # first 10 is to resevere for parameters other than Rt
  para <- c(rep(0,10),rep(1,nrow(data22)-startpt+1))
  # for para2, also restricted to <= 153
  para2 <- c(0.1)
  move <- rep(1,length(para))
  #move[1:(length(para)-50)] <- 0
  move[1:10] <- 0
  para[1:10] <- 0
  
  sigma <- (abs(para)+0.1)/5
  sigma2 <- (abs(para2)+0.1)/5
  #sourceCpp("rt.cpp")
  aaaaa1 <- Sys.time()
  tt <- mcmc(data22,para,para2,w_dis,smooth,startpt,5000,move,sigma,sigma2)
  aaaaa2 <- Sys.time()
  ##print(aaaaa2-aaaaa1)
  
  inc <- 1000+1:4000
  z1 <- para_summary((tt[[1]][inc,]),4,3,0)
  
  
  rest <- z1
  totaln <- nrow(rest)-10
  dataout = data.frame(date = df$date[startpt:nrow(df)])
  
  dataout[1:nrow(dataout),c("local.rt.mean","local.rt.lower","local.rt.upper")] <- rest[10+1:totaln,1:3]
  return(dataout)
}
