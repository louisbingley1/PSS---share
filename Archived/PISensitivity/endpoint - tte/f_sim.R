# Function to simulate data
f_sim<-function(n,mean,sd,alpha0, alpha1, alpha2, beta0, beta1, beta2, eta, delta, cutoff, gamma){
  USUBJID      <- seq(1,n,1)
  TRT          <- sample(c(1,0), n, replace= TRUE)
  X1           <- rnorm(n,mean,sd)
  X2           <- rnorm(n,mean,sd)     
  U            <- rnorm(n,mean,sd)
  p            <- exp(alpha0+ alpha1*X1+alpha2*U)/(1+exp(alpha0+alpha1*X1+alpha2*U))
  S1_          <- rbinom(n, 1, p)
  S0_          <- rbinom(n, 1, p)
  lambda0      <- exp(beta0 + beta1 * X1 + beta2 * X2 + gamma * S1_         )   
  lambda1      <- exp(beta0 + beta1 * X1 + beta2 * X2 +   eta * S1_ + delta ) 
  Y1_t         <- rexp(n, lambda1)
  Y0_t         <- rexp(n, lambda0)
  data_        <- data.frame(USUBJID,TRT,X1,X2,U,S1_,S0_,Y1_t,Y0_t,lambda0,lambda1,p) 
  for(i in 1: nrow(data_)){
    data_$c1_[i] <- ifelse(data_$Y1_t[i]<=cutoff,  1,            0     )
    data_$c0_[i] <- ifelse(data_$Y0_t[i]<=cutoff,  1,            0     )  
    data_$Y1_[i] <- ifelse(data_$Y1_t[i]<=cutoff, data_$Y1_t[i], cutoff)
    data_$Y0_[i] <- ifelse(data_$Y0_t[i]<=cutoff, data_$Y0_t[i], cutoff)
  }
  data         <- data_ %>%   
    mutate(S0 = ifelse(TRT == 0, S0_, NA),
           S1 = ifelse(TRT == 1, S1_, NA),
           Y0 = ifelse(TRT == 1, NA, Y0_),
           Y1 = ifelse(TRT == 0, NA, Y1_),
           c0 = ifelse(TRT == 0, c0_, NA), 
           c1 = ifelse(TRT == 1, c1_, NA),
           c  = ifelse(TRT == 1, c1,  c0),
           Y  = ifelse(TRT == 1, Y1,  Y0))  %>% 
    droplevels()
  return(data)
}  # example: dat<-simulate(n=500, mean=0,sd= 1, alpha0 = -1.78, alpha1 =  2.00, alpha2 =  0.00, beta0 = -5.00, beta1=0.50,beta2 = -0.50, eta= 0.47, delta =  -0.69, gamma=0.00)
