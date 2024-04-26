f_sim<-function(n,mean,sd,alpha0, alpha1, alpha2, beta0, beta1, beta2, eta, delta,  gamma){
  USUBJID      <- seq(1,n,1)
  TRT          <- sample(c(1,0), n, replace= TRUE)
  X1           <- rnorm(n,mean,sd)
  X2           <- rnorm(n,mean,sd)     
  U            <- rnorm(n,mean,sd)
  p            <- exp(alpha0+ alpha1*X1+alpha2*U)/(1+exp(alpha0+alpha1*X1+alpha2*U))
  S1_          <- rbinom(n, 1, p)
  mu0          <- beta0 + beta1 * X1 + beta2 * X2 + gamma * S1_  
  mu1          <- beta0 + beta1 * X1 + beta2 * X2 +   eta * S1_ + delta 
  Y0_          <- rnorm(n, mu0, 1)
  Y1_          <- rnorm(n, mu1, 1)
  data_        <- data.frame(USUBJID,TRT,X1,X2,U,S1_,Y1_,Y0_,mu0,mu1,p) 
  data         <- data_ %>%   
    mutate(Y0 = ifelse(TRT == 1, NA,  Y0_)) %>% 
    mutate(Y1 = ifelse(TRT == 1, Y1_, NA))  %>% 
    mutate(S1 = ifelse(TRT == 1, S1_, NA))  %>%  
    mutate(Y  = ifelse(TRT == 1, Y1,  Y0))  %>% 
    mutate(Z  = ifelse(TRT == 1,  1,  0)) %>%
    droplevels()
  return(data)
}  
