
{
  gen_Z            <- function(num = 1){
    return(rbinom(1, 1, prob = 0.5))
  }
  gen_X            <- function(num = 1){
    res <- as.matrix(c(1, rnorm(4, mean = 0, sd = 1), rbinom(1, size = 1, prob = 1/2)), ncol = 1)
    return(res)
  }
  multi_logit_ss   <- function(Xi, theta_logit_ss, theta_logit_s1s1, theta_logit_ss1){
    Xi <- t(Xi)
    res_ss <- exp(theta_logit_ss %*% Xi) / (exp(theta_logit_ss %*% Xi) + 
                                              exp(theta_logit_s1s1 %*% Xi) +
                                              exp(theta_logit_ss1 %*% Xi))
    return(res_ss)
  }
  multi_logit_s1s1 <- function(Xi, theta_logit_ss, theta_logit_s1s1, theta_logit_ss1){
    Xi <- t(Xi)
    res_s1s1 <- exp(theta_logit_s1s1 %*% Xi) / (exp(theta_logit_ss %*% Xi) + 
                                                  exp(theta_logit_s1s1 %*% Xi) +
                                                  exp(theta_logit_ss1 %*% Xi))
    return(res_s1s1)
  }
  multi_logit_ss1  <- function(Xi, theta_logit_ss, theta_logit_s1s1, theta_logit_ss1){
    Xi <- t(Xi)
    res_ss1 <- exp(theta_logit_ss1 %*% Xi) / (exp(theta_logit_ss %*% Xi) + 
                                                exp(theta_logit_s1s1 %*% Xi) +
                                                exp(theta_logit_ss1 %*% Xi))
    return(res_ss1)
  }
  prob_U           <- function(Xi, theta_logit_ss, theta_logit_s1s1,   theta_logit_ss1){
    
    Xi <- t(Xi)
    prob <- c(multi_logit_ss1(  Xi,   theta_logit_ss,   theta_logit_s1s1, theta_logit_ss1),
              multi_logit_ss(   Xi,   theta_logit_ss,  theta_logit_s1s1,  theta_logit_ss1),
              multi_logit_s1s1( Xi,   theta_logit_ss,  theta_logit_s1s1,  theta_logit_ss1) 
              
    )
    return(prob)
  }
  sample_strata    <- function(prob){  return(sample(c(1,2,3), size = 1, prob = prob))  }
  gen_S            <- function(Z, strata){
    if(strata == 1){  return(Z)  }else   # complier
      if(strata == 2){ return(c(1)) }else   # always taker
                       { return(c(0))}         # never taker 
  }
  gen_Y0           <- function(strata, X){
    mu_Y0 <- sum(X[2:6]) + 1*(strata == 2) + 1 # strata = 2 - always-taker
    res <- rnorm(1, mu_Y0, 1)
    return(res)
  }
  gen_Y1           <- function(strata, X){
    mu_Y1 <- sum(X[2:6]) - 1*(strata == 3) + 4 # strata = 2 - never-taker
    res <- rnorm(1, mu_Y1, 1)
    return(res)
  }
  gen_Y              <- function(strata, Z, Y0, Y1){
    if(Z==0){  
      return(c(Y0))
    }else{  
      return(c(Y1))
    } 
  }
  # gen_X_list <- function(){
  #   res <- as.matrix(c(1, rnorm(4, mean = 0, sd = 1), rbinom(1, size = 1, prob = 1/2)), ncol = 1)
  #   return(list(res))
  # } 
}


########################################
# simu data funtion
########################################

sim_data <- function(n = 500,                                        # sample size of simulated data
                     random_seed = 1,                                # random seed to generate different simulated data
                     theta_seq = c(-1, -0.5, 0, 0.5, 1),             # the sequence of all possible theta for theta_logit
                     index_theta = 1,                                # range from 1 to 5, index for which theta from theta_seq will be added to the end of theta_logit
                     theta_logit_ss = c(0.25, 0.5, 0.5, 1, 1),       # the first 5 coefficients for logit model for ss strata - always-taker
                     theta_logit_s1s1 = c(-0.25, 1, 1, 0.5, 0.5),    # the first 5 coefficients for logit model for s1s1 strata - never-taker
                     theta_logit_ss1 = rep(0, 6)                     # the first 5 coefficients for logit model for ss1 strata - complier
){
  
  set.seed(random_seed)
  Z                <- mapply(gen_Z, num = 1:n) # treatment regime
  X_matrix         <- mapply(gen_X, num = 1:n)
  X                <- lapply(seq_len(ncol(X_matrix)), function(i) X_matrix[,i])  # X <- sapply(1:n, gen_X_list) # covariate
  theta            <- theta_seq[index_theta]
  theta_logit_ss   <- c(theta_logit_ss, theta)
  theta_logit_s1s1 <- c(theta_logit_s1s1, theta)
  prob_U_list      <- lapply(X,   prob_U,  theta_logit_ss1, theta_logit_ss,  theta_logit_s1s1) # Calculate probability for each strata: ss1, ss, s1s1 
  Strata           <- mapply(sample_strata, prob = prob_U_list)                                 # Sample the strata: 1 - ss - always-taker, 2 - s1s1 - never-taker, 3 - ss1 - complier
  S                <- mapply(gen_S, Z = Z, strata = Strata)                                     # Generate the intercurrent event S
  Y0               <- mapply(gen_Y0, strata = Strata, X = X)
  Y1               <- mapply(gen_Y1, strata = Strata, X = X)
  Y                <- mapply(gen_Y, strata = Strata, Z = Z, Y0 = Y0, Y1 = Y1)
  
  Simu_data <- data.frame( X1 = X_matrix[2, ],
                           X2 = X_matrix[3, ],
                           X3 = X_matrix[4, ],
                           X4 = X_matrix[5, ],
                           X5 = X_matrix[6, ],
                           Z = Z,
                           S = S,
                           Y = Y,
                           Y0 = Y0,
                           Y1 = Y1,
                           U = Strata)
  return(Simu_data)
}


# 
# temp <- sim_data(random_seed = 1)
# temp1 <- sim_data(random_seed = 1)
# all.equal(temp, temp1)
# temp2 <- sim_data(random_seed = 2)


# 1
temp <-sim_data(n = 500,   random_seed = 1,  theta_seq = c(-1, -0.5, 0, 0.5, 1),  index_theta = 1, theta_logit_ss = c(0.25, 0.5, 0.5, 1, 1),   theta_logit_s1s1 = c(-0.25, 1, 1, 0.5, 0.5),  theta_logit_ss1 = rep(0, 6))
res_temp <- PSPS_M_weighting(Z = temp$Z, 
                             D = temp$S, 
                             X = as.matrix(temp[, 1:5]), 
                             Y = temp$Y,
                             trc = FALSE, 
                             ep1 = 1, 
                             ep0 = 1,
                             beta.a = NULL,
                             beta.n = NULL)


# 2
data          = temp
data          = data %>% mutate(indexZD=paste0(Z,D), D=S,USUBJID  = seq(1,nrow(data),1))

f_PS = function(data, ep1, ep0,beta.a= NULL,beta.n=NULL, iter.max,error0){  
  
  N         =  nrow(data) 
  X         = as.matrix(temp[, 1:5]) 
  X         =  cbind(rep(1, N), X)
  V         =  ncol(X)
  Z         =  data %>%  pull(Z) %>% as.numeric()   
  Y         =  data %>%  pull(Y)  
  D         =  data %>%  pull(S) 
  USUBJID   =  data %>%  pull(USUBJID)
  indexZD   =  data %>%  pull(indexZD)
  
  # pi      : computed from observed data
  pilist    = f_pi(Z,D)
  pi.n      = pilist$pi.n
  pi.a      = pilist$pi.a
  pi.c      = pilist$pi.c
  
  # betas:     beta.a, beta.n 
  #            coefficients of the weighted multinomial logistic regression model.
  #            computed from EM algorithm.
  betalist  = f_EM_betas_vali( data,Z,D,X,N,V,USUBJID,beta.a = NULL,beta.n=NULL,iter.max,error0,Trace = T )
  beta.a    = betalist$beta.a
  beta.n    = betalist$beta.n
  AugData   = betalist$AugData # head(AugData);AugData %>% arrange(USUBJID);dim(AugData)
  
  # merge
  boot_     = data %>% distinct(Z,D,Y,U,USUBJID,indexZD)   # head(AugData);head(boot_)
  AugData   = left_join(AugData, boot_, by="USUBJID") 
  for(i in 1: nrow(AugData)){  AugData$Utrue[i] <- AugData$U[i] }
  
  
  # ps score: computed from betas
  ps.score  = f_PROB_vali(AugData ,beta.a,beta.n)
  colnames(ps.score) = c("ps.score.1","ps.score.2","ps.score.3")
  AugData   = cbind.data.frame(AugData,ps.score);head(AugData)
  
  # W
  AugData   = f_w(AugData,pi.a,pi.c,pi.n,ep1,ep0);head(AugData)
  
  # PCP - prob of correct prediction
  #AugData   <- AugData %>% mutate( IU   = as.numeric(Uhat == Utrue),PCP  = w_Uhat*IU) 
  
  head(AugData)
  
  # fit
  coefflist   = f_coeff_vali(AugData);  
  coeff_tb   = coefflist$coeff
  coeff.1c   = coefflist$coeff.1c
  coeff.0c   = coefflist$coeff.0c
  coeff.1a   = coefflist$coeff.1a
  coeff.0a   = coefflist$coeff.0a
  coeff.1n   = coefflist$coeff.1n
  coeff.0n   = coefflist$coeff.0n   
  coeff.1ca   = coefflist$coeff.1ca
  coeff.0ca   = coefflist$coeff.0ca
  AugData = left_join(AugData, coeff_tb,by="indexZU" );head(AugData)
  
  # ACEs
  
  if(pi.n>0 & pi.a>0 & pi.c>0){
    
    # CACE, NACE and AACE
    {
      AugData$Yw    = AugData$Y * AugData$w
      CACE          = mean(AugData %>% filter(PS==1 & Z==1) %>% pull(Yw)  , na.rm=T ) - mean(AugData %>% filter(PS==1 & Z==0) %>% pull(Yw) , na.rm=T )                                   
      AACE          = mean(AugData %>% filter(PS==2 & Z==1) %>% pull(Yw)  , na.rm=T ) - mean(AugData %>% filter(PS==2 & Z==0) %>% pull(Yw) , na.rm=T )                           
      NACE          = mean(AugData %>% filter(PS==3 & Z==1) %>% pull(Yw)  , na.rm=T ) - mean(AugData %>% filter(PS==3 & Z==0) %>% pull(Yw) , na.rm=T ) 
      CAACE         = mean(AugData %>% filter( (PS==1|PS==2) & (Z==1) ) %>% pull(Yw)  , na.rm=T ) - mean(AugData %>% filter((PS==1|PS==2) & (Z==0)) %>% pull(Yw) , na.rm=T )                           
      
    }
    
    # CACE.adj, NACE.adj and AACE.adj 
    {AugData       = AugData %>% mutate(
      one_w = one * w,
      X_1_w = X_1 * w,
      X_2_w = X_2 * w,
      BASE_w = BASE * w
      
    )
      AugData$Yhat  = AugData$coeff.intercept + AugData$X_1 * AugData$coeff.X_1 + AugData$X_2*AugData$coeff.X_2 + AugData$BASE * AugData$coeff.BASE
      AugData$Rw    = AugData$w * (AugData$Y - AugData$Yhat )
      C.adj.data    = as.matrix( rbind.data.frame(AugData %>% filter(indexZU=='1c') %>% select(one_w,X_1_w,X_2_w,BASE_w),
                                                  AugData %>% filter(indexZU=='0c') %>% select(one_w,X_1_w,X_2_w,BASE_w)  ) 
      )
      A.adj.data    = as.matrix( rbind.data.frame(AugData %>% filter(indexZU=='1a') %>% select(one_w,X_1_w,X_2_w,BASE_w), 
                                                  AugData %>% filter(indexZU=='0a') %>% select(one_w,X_1_w,X_2_w,BASE_w)  ) 
      )
      N.adj.data    = as.matrix( rbind.data.frame(AugData %>% filter(indexZU=='1n') %>% select(one_w,X_1_w,X_2_w,BASE_w), 
                                                  AugData %>% filter(indexZU=='0n') %>% select(one_w,X_1_w,X_2_w,BASE_w)  ) 
      )
      CA.adj.data    = as.matrix( rbind.data.frame(AugData %>% filter(indexZU=='1c' | indexZU=='1a' ) %>% select(one_w,X_1_w,X_2_w,BASE_w), 
                                                   AugData %>% filter(indexZU=='0c' | indexZU=='0a' ) %>% select(one_w,X_1_w,X_2_w,BASE_w)  ) 
      )
      CACE.adj      = mean(AugData %>% filter(indexZU=='1c') %>% pull(Rw)) - mean(AugData %>% filter(indexZU=='0c') %>% pull(Rw)) + mean(C.adj.data %*% as.numeric(coeff.1c-coeff.0c) )
      AACE.adj      = mean(AugData %>% filter(indexZU=='1a') %>% pull(Rw)) - mean(AugData %>% filter(indexZU=='0a') %>% pull(Rw)) + mean(A.adj.data %*% as.numeric(coeff.1a-coeff.0a) )
      NACE.adj      = mean(AugData %>% filter(indexZU=='1n') %>% pull(Rw)) - mean(AugData %>% filter(indexZU=='0n') %>% pull(Rw)) + mean(N.adj.data %*% as.numeric(coeff.1n-coeff.0n) )
      CAACE.adj      = mean(AugData %>% filter(indexZU=='1c' | indexZU=='1a' ) %>% pull(Rw)) - mean(AugData %>% filter(indexZU=='0c' | indexZU=='0a') %>% pull(Rw)) + mean(CA.adj.data %*% as.numeric(coeff.1ca-coeff.0ca) )
    }
    
    # CPCP, NPCP,APCP
    {
      nC   = sum(AugData %>% filter(Utrue == "C") %>%pull(w_Uhat))
      nN   = sum(AugData %>% filter(Utrue == "N") %>%pull(w_Uhat))
      nA   = sum(AugData %>% filter(Utrue == "A") %>%pull(w_Uhat))
      CPCP = sum( AugData %>% filter(Utrue == "C") %>% pull(PCP) )/nC;CPCP
      NPCP = sum( AugData %>% filter(Utrue == "N") %>% pull(PCP) )/nN;NPCP
      APCP = sum( AugData %>% filter(Utrue == "A") %>% pull(PCP) )/nA;APCP
    }
    
    #results
    ACE = list( CACE   = CACE  , CACE.adj = CACE.adj, 
                NACE   = NACE  , NACE.adj = NACE.adj, 
                AACE   = AACE  , AACE.adj = AACE.adj,  
                CAACE  = CAACE , CAACE.adj = CAACE.adj,
                beta.a = beta.a, beta.n   = beta.n  ,
                pi.a   = pi.a  , pi.n     = pi.n    , pi.c = pi.c,
                nC     = nC    , nA  = nA, nN=nN, CPCP = CPCP, NPCP = NPCP, APCP = APCP,
                AugData = AugData)
    
  }else{    
    stop(paste("At least one of pi.n,pi.a,pi.c has negative value: pi.n=",pilist[1],"pi.a=",pilist[2], "pi.c=",pilist[3]))
  }
  
  return( ACE )
  
}

# debug

res_temp

library(dplyr)
View(temp)
temp$d=temp$Y1-temp$Y0

strata1=temp %>% filter(U==1) ;View(strata1)
strata2=temp %>% filter(U==2) ;View(strata2)
strata3=temp %>% filter(U==3)

mean(strata1$d)
mean(strata2$d)
mean(strata3$d)
