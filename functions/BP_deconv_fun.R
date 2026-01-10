




BP_deconv_fun = function(report_case){
  delay <- read.csv("delay.csv")
  
  deconv_100 = sapply(1:100,function(i){
    ## here sample the incubation meanlog and sdlog
    meanvec <- c(1.07,0.65) 
    incpara <- meanvec
    incvec <- (plnorm(1:20,incpara[1],incpara[2])-plnorm(0:19,incpara[1],incpara[2]))/plnorm(20,incpara[1],incpara[2])
    
    ## infectiousness, shifted gamma distribution f_c(x) = f(x+c) with parameters 
    infpara <- c(20.52,1.59,12.27)
    infvec <- (pgamma(1:40,infpara[1],infpara[2])-pgamma(0:39,infpara[1],infpara[2]))
    
    ## generate convolution
    rint <- matrix(0,20,4)
    for (k in 1:4){
      for (i in 1:20){
        for (j in 1:20){
          if (i+j>0&(i+j<=20)){
            rint[i+j-1,k] <- rint[i+j,k] + incvec[i]*delay[j,k]
          }
        }
      }
    }
    rint[,3] <- rint[,3]/sum(rint[,3])
    
    
    temp <- sts(report_case)
    bpnp.control <- list(k=0,eps=rep(1,2),iter.max=rep(250,2),B=-1,verbose=TRUE)
    
    
    temp2 <- backprojNP(temp ,incu.pmf=c(rint[1:14,3]/sum(rint[1:14,3])),
                        control=modifyList(bpnp.control,list(eq3a.method="C")), ylim=c(0,max(X,Y)), verbose = FALSE)
    #print(sum(temp2@upperbound)-sum(report_case))
    output <- (temp2@upperbound)/sum(temp2@upperbound)*sum(report_case)
    decondata <- output
    decondata1 <- rpois(length(decondata),decondata)
    return(decondata1)
  })
  
  deconv_mean = round(rowMeans(deconv_100))
  return(deconv_mean)
}
