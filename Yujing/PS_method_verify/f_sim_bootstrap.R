rm(list=ls())

# Ding's estimation code
{
  ##################################################################
  ######################## EM Algorithm for ########################
  #### Principal Stratification Analysis using Propensity Score ####
  ######################## With Monotonicity #######################
  ###################### Ding and Lu 2015 Oct ######################
  ##################################################################
  
  
  ##propensity score method, with covariate adjustment and sensitivity analysis for GPI
  #the package used for multivariate logistic regression
  library(nnet)
  
  #Preliminary function: principal score calculation 
  #Z: randomization
  #D: treatment received
  #X: pretreatment covaraites, 11111 in the first column
  #beta.a, beta.n: initial values for the paramaters in the multiple logistic regression
  #iter.max: total number of iterations
  #error0: convergence error rate
  #Trace: if TRUE then trace each EM iteration
  #fitting multinomial logistic regression model with principal stratification variable as missing data
  PS_pred = function(Z, D, X, 
                     beta.a = NULL, beta.n = NULL, 
                     iter.max = 200, error0 = 10^-6, Trace = FALSE) {  
    V = dim(X)[2]
    N = length(Z)
    if(is.null(beta.a)) beta.a = rep(0, V)
    if(is.null(beta.n)) beta.n = rep(0, V)  
    
    iter = 1         
    repeat{
      
      ##initial values of iteration
      beta.a_old = beta.a
      beta.n_old = beta.n
      
      if(Trace == T) {
        print(paste("The ", iter, "-th EM iteration!", sep=""))
      }
      
      #E step: posterior probabilities
      #and the augmented data set with weights
      #creat a null matrix for the augmented data set AugData
      AugData = NULL
      #each individual correspond to 1 or 2 individuals in the augmented data set
      for(i in 1:N) {
        if(Z[i]==1&D[i]==1) {
          #posterior probabilities
          prob.c = 1/(1 + exp(t(beta.a_old)%*%X[i, ]))
          prob.a = 1 - prob.c
          
          AugData = rbind(AugData, c(1, X[i, ], prob.c))
          AugData = rbind(AugData, c(2, X[i, ], prob.a))
        }
        
        if(Z[i]==1&D[i]==0) {
          AugData = rbind(AugData, c(3, X[i, ], 1))  
        }
        
        if(Z[i]==0&D[i]==1) {
          AugData = rbind(AugData, c(2, X[i, ], 1))  
        }
        
        if(Z[i]==0&D[i]==0) {
          #posterior probabilities
          prob.c = 1/(1 + exp(t(beta.n_old)%*%X[i, ]))
          prob.n = 1 - prob.c
          
          AugData = rbind(AugData, c(1, X[i, ], prob.c))
          AugData = rbind(AugData, c(3, X[i, ], prob.n))  
          
        }#for if
        
      }#for "for"
      #make AugData into a dataframe
      #AugData = data.frame(AugData)
      #colnames(AugData) = c("U", "X", "Weight")
      #Multinomial logistic regression using "nnet" package
      
      fit = multinom(AugData[, 1] ~ AugData[, (3:(V+1))], weights = AugData[, (V+2)], trace = FALSE)
      betas  = coef(fit)
      beta.a = betas[1, ]
      beta.n = betas[2, ]
      
      iter = iter + 1
      error = sum((beta.a - beta.a_old)^2)  + sum((beta.n - beta.n_old)^2)
      if(iter>iter.max||error<error0)   break           
      
    }#for repeat
    
    #the predicted probabilities
    #three columns corresponding to complier, always taker and never taker
    PROB = matrix(0, N, 3)
    for(i in 1:N) {
      prob.c = 1
      prob.a = exp(t(beta.a)%*%X[i, ])
      prob.n = exp(t(beta.n)%*%X[i, ])
      sum = prob.c + prob.a + prob.n
      
      PROB[i,] = c(prob.c, prob.a, prob.n)/sum
    }
    
    results = list(PROB=PROB, beta.a=beta.a, beta.n=beta.n)
    return(results)
  }
  
  #Main function
  #Z: randomization
  #D: treatment received
  #X: covariate matrix: the first column is NOT 11111
  #U: (latent) principal stratification variable, 1 complier, 2 always taker, 3 never taker
  #Y: outcome of interest
  #trc: truncation by death indicator, default FALSE. If TRUE only SACE (i.e. AACE) is calculated.
  #ep1, ep0: sensitivity parameters in Proposition 4, Section 6.1.
  #beta.a, beta.n: initial values for the paramaters in the multiple logistic regression
  PSPS_M_weighting = function(Z, D, X, Y, 
                              trc = FALSE, ep1 = 1, ep0 = 1,
                              beta.a = NULL, beta.n = NULL) {
    #augment the design X
    N = length(Z)
    X = cbind(rep(1, N), X)
    
    #estimate the propensity scores using Multinomial Logistic Regression
    #PS_pred returns three columns: c, a, n
    ps.score.fit = PS_pred(Z, D, X, beta.a = beta.a, beta.n = beta.n)
    ps.score     = ps.score.fit$PROB
    pr.n = sum(Z*(1 - D))/sum(Z)
    pr.a = sum((1 - Z)*D)/sum(1-Z)
    pr.c = 1 - pr.n - pr.a
    
    #indices
    index11 = (1:N)[Z==1&D==1]
    index10 = (1:N)[Z==1&D==0]
    index01 = (1:N)[Z==0&D==1]
    index00 = (1:N)[Z==0&D==0]
    
    #weights
    if (trc == F) {
      w1c = ep1*ps.score[index11, 1]/(ep1*ps.score[index11, 1] + ps.score[index11, 2])/pr.c*(pr.c + pr.a)
      w0c = ep0*ps.score[index00, 1]/(ep0*ps.score[index00, 1] + ps.score[index00, 3])/pr.c*(pr.c + pr.n)
      w0n = ps.score[index00, 3]/(ep0*ps.score[index00, 1] + ps.score[index00, 3])/pr.n*(pr.c + pr.n)
    }
    w1a = ps.score[index11, 2]/(ep1*ps.score[index11, 1] + ps.score[index11, 2])/pr.a*(pr.c + pr.a)
    
    #model assisted regression estimator 
    if (trc == F) {
      r1c = lm(Y[index11] ~ 0 + X[index11, ], weights = w1c)$coef
      r0c = lm(Y[index00] ~ 0 + X[index00, ], weights = w0c)$coef
      r1n = lm(Y[index10] ~ 0 + X[index10, ])$coef
      r0n = lm(Y[index00] ~ 0 + X[index00, ], weights = w0n)$coef
    }
    r1a = lm(Y[index11] ~ 0 + X[index11, ], weights = w1a)$coef
    r0a = lm(Y[index01] ~ 0 + X[index01, ])$coef
    
    #weighted outcomes
    if (trc == F) {
      weighted.Y.c1 = Y[index11]*w1c
      weighted.Y.c0 = Y[index00]*w0c
      weighted.Y.n0 = Y[index00]*w0n
    }
    weighted.Y.a1 = Y[index11]*w1a
    
    #CACE, NACE and AACE
    if (trc == F) {
      CACE = mean(weighted.Y.c1) - mean(weighted.Y.c0)
      NACE = mean(Y[index10]) - mean(weighted.Y.n0)
    }
    AACE = mean(weighted.Y.a1) - mean(Y[index01])
    
    #weighted outcomes for regression estimator
    if (trc == F) {
      weighted.Y1c = (Y[index11]-X[index11, ]%*%r1c)*w1c
      weighted.Y0c = (Y[index00]-X[index00, ]%*%r0c)*w0c
      weighted.Y1n = Y[index10]-X[index10, ]%*%r1n
      weighted.Y0n = (Y[index00]-X[index00, ]%*%r0n)*w0n
      weighted.rc = rbind(X[index11, ]*w1c, X[index00, ]*w0c) %*% (r1c - r0c)
      weighted.rn = rbind(X[index10, ], X[index00, ]*w0n) %*% (r1n - r0n)
    }
    weighted.Y1a = (Y[index11]-X[index11, ]%*%r1a)*w1a
    weighted.Y0a = Y[index01]-X[index01, ]%*%r0a
    weighted.ra = rbind(X[index11, ]*w1a, X[index01, ]) %*% (r1a - r0a)
    
    #CACE, NACE and AACE, regression estimates
    if (trc == F) {
      CACE.reg = mean(weighted.Y1c) - mean(weighted.Y0c) + mean(weighted.rc)
      NACE.reg = mean(weighted.Y1n) - mean(weighted.Y0n) + mean(weighted.rn)
    }
    AACE.reg = mean(weighted.Y1a) - mean(weighted.Y0a) + mean(weighted.ra)
    
    #results
    if (trc == F) {
      ACE = list(CACE = CACE, CACE.reg = CACE.reg, 
                 NACE = NACE, NACE.reg = NACE.reg, 
                 AACE = AACE, AACE.reg = AACE.reg,  
                 beta.a = ps.score.fit$beta.a, beta.n = ps.score.fit$beta.n)
    }
    else {
      ACE = list(AACE = AACE, AACE.reg = AACE.reg,  
                 beta.a = ps.score.fit$beta.a, beta.n = ps.score.fit$beta.n)
    }
    return(ACE)
    
  }
}

# Bing's estimation code - this code matches with CITIES simulator
{
  # load Bing's functions
  source("C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Documents/GitHub/PSS---share/Principal Score/main/f_pi.R")
  source("C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Documents/GitHub/PSS---share/Principal Score/main/f_EM_betas.R")
  source("C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Documents/GitHub/PSS---share/Principal Score/main/f_prob.R")
  source("C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Documents/GitHub/PSS---share/Principal Score/main/f_w.R")
  source("C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Documents/GitHub/PSS---share/Principal Score/main/f_coeff.R")
  source("C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Documents/GitHub/PSS---share/Principal Score/main/f_augdata.R")
  
  f_PS = function(data, ep1, ep0, beta.a= NULL, beta.n=NULL, iter.max,error0){  
    
    N         =  nrow(data)
    X_in      =  data %>%  dplyr::select("X_1","X_2") %>% cbind(data %>% dplyr::select(BASE)) %>% as.matrix       
    X         =  cbind(rep(1, N), X_in)
    V         =  ncol(X)
    Z         =  data %>%  pull(Z) %>% as.numeric()   
    Y         =  data %>%  pull(Y)  
    D         =  data %>%  pull(D) 
    USUBJID   =  data %>%  pull(USUBJID)
    indexZD   =  data %>%  pull(indexZD)
    # X1: continuous, X2: binary, BASE: continuous
    
    # pi      : computed from observed data
    pilist    = f_pi(Z,D)
    pi.n      = pilist$pi.n
    pi.a      = pilist$pi.a
    pi.c      = pilist$pi.c
    
    # betas:     beta.a, beta.n 
    #            coefficients of the weighted multinomial logistic regression model.
    #            computed from EM algorithm.
    betalist  = f_EM_betas( data,Z,D,X,N,V,USUBJID,beta.a = NULL,beta.n=NULL,iter.max,error0,Trace = T )
    beta.a    = betalist$beta.a
    beta.n    = betalist$beta.n
    AugData   = betalist$AugData # head(AugData);AugData %>% arrange(USUBJID);dim(AugData)
    
    # merge
    boot_     = data %>% distinct(Z,D,Y,U,USUBJID,indexZD)   # head(AugData);head(boot_)
    AugData   = left_join(AugData, boot_, by="USUBJID") 
    for(i in 1: nrow(AugData)){  AugData$Utrue[i] <- strsplit(AugData$U[i],"/")[[1]][3] }
    
    
    # ps score: computed from betas
    ps.score  = f_PROB(AugData ,beta.a,beta.n)
    colnames(ps.score) = c("ps.score.1","ps.score.2","ps.score.3")
    AugData   = cbind.data.frame(AugData,ps.score);head(AugData)
    
    # W
    AugData   = f_w(AugData,pi.a,pi.c,pi.n,ep1,ep0);head(AugData)
    
    # PCP - prob of correct prediction
    AugData   <- AugData %>% mutate( IU   = as.numeric(Uhat == Utrue),
                                     PCP  = w_Uhat*IU) 
    head(AugData)
    
    # fit
    coefflist   = f_coeff(AugData);  
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
  
}

# Generate binary case simulated data
{
  library(dplyr)
  ########################################
  # Other basic information
  # Ding, 2017 generated 1000 simulated datasets
  # Sample size is 500 for each simulated dataset
  # They used the bootstrap to construct the 95% CI
  ########################################
  
  ########################################
  # Basic functions for generating simulated data
  ########################################
  
  {
    gen_Z <- function(num = 1){
      return(rbinom(1, 1, prob = 1/2))
    }
    
    gen_X <- function(num = 1){
      res <- as.matrix(c(1, rnorm(4, mean = 0, sd = 1), rbinom(1, size = 1, prob = 1/2)), ncol = 1)
      return(res)
    }
    
    multi_logit_ss <- function(Xi,
                               theta_logit_ss,
                               theta_logit_s1s1,
                               theta_logit_ss1){
      Xi <- t(Xi)
      res_ss <- exp(theta_logit_ss %*% Xi) / (exp(theta_logit_ss %*% Xi) + 
                                                exp(theta_logit_s1s1 %*% Xi) +
                                                exp(theta_logit_ss1 %*% Xi))
      return(res_ss)
    }
    
    multi_logit_s1s1 <- function(Xi,
                                 theta_logit_ss,
                                 theta_logit_s1s1,
                                 theta_logit_ss1){
      Xi <- t(Xi)
      res_s1s1 <- exp(theta_logit_s1s1 %*% Xi) / (exp(theta_logit_ss %*% Xi) + 
                                                    exp(theta_logit_s1s1 %*% Xi) +
                                                    exp(theta_logit_ss1 %*% Xi))
      return(res_s1s1)
    }
    
    multi_logit_ss1 <- function(Xi,
                                theta_logit_ss,
                                theta_logit_s1s1,
                                theta_logit_ss1){
      Xi <- t(Xi)
      res_ss1 <- exp(theta_logit_ss1 %*% Xi) / (exp(theta_logit_ss %*% Xi) + 
                                                  exp(theta_logit_s1s1 %*% Xi) +
                                                  exp(theta_logit_ss1 %*% Xi))
      return(res_ss1)
    }
    
    prob_U <- function(Xi,
                       theta_logit_ss,
                       theta_logit_s1s1,
                       theta_logit_ss1){
      Xi <- t(Xi)
      prob <- c(multi_logit_ss(Xi,
                               theta_logit_ss,
                               theta_logit_s1s1,
                               theta_logit_ss1),
                multi_logit_s1s1(Xi,
                                 theta_logit_ss,
                                 theta_logit_s1s1,
                                 theta_logit_ss1),
                multi_logit_ss1(Xi,
                                theta_logit_ss,
                                theta_logit_s1s1,
                                theta_logit_ss1)
      )
      return(prob)
    }
    
    sample_strata <- function(prob){
      return(sample(c(1,2,3), size = 1, prob = prob))
    }
    
    gen_S <- function(Z, strata){
      if(strata == 1){ # always-taker
        return(c(1))
      }else if(strata == 2){ # never-taker
        return(c(0))
      }else{ # complier 
        return(Z)
      }
    }
    
    gen_Y0 <- function(strata, X){
      mu_Y0 <- 0.3 * sum(X[2:6]) + 0.25 * (1 - (strata == 1)) # strata = 1 - always-taker
      prob_Y0 <- exp(mu_Y0) / (1 + exp(mu_Y0))
      res <- sample(c(0, 1), size = 1, prob = c(1-prob_Y0, prob_Y0))
      return(res)
    }
    
    gen_Y1 <- function(strata, X){
      mu_Y1 <- 0.3 * sum(X[2:6]) + 0.25 * ((strata == 2) - 1)  # strata = 2 - never-taker
      prob_Y1 <- exp(mu_Y1) / (1 + exp(mu_Y1))
      res <- sample(c(0, 1), size = 1, prob = c(1-prob_Y1, prob_Y1))
      return(res)
    }
    
    gen_Y <- function(Z, Y0, Y1){
      return(Z*Y1 + (1-Z)*Y0)
    }
    
  }
  
  
  ########################################
  # simu data function
  ########################################
  
  sim_data_binary <- function(n = 500, # sample size of simulated data
                              random_seed = 3, # random seed to generate different simulated data
                              theta_seq = c(-1, -0.5, 0, 0.5, 1), 
                              # the sequence of all possible theta for theta_logit
                              index_theta = 3, 
                              # range from 1 to 5, index for which theta from theta_seq will be added to the end of theta_logit
                              theta_logit_ss = c(0.25, 0.5, 0.5, 1, 1),
                              # the first 5 coefficients for logit model for ss strata - always-taker
                              theta_logit_s1s1 = c(-0.25, 1, 1, 0.5, 0.5),
                              # the first 5 coefficients for logit model for s1s1 strata - never-taker
                              theta_logit_ss1 = rep(0, 6)
                              # the first 5 coefficients for logit model for ss1 strata - complier
  ){
    
    set.seed(random_seed)
    
    # Generate X for simulated data
    Z <- mapply(gen_Z, num = 1:n) # treatment regime
    X_matrix <- mapply(gen_X, num = 1:n)
    # X <- sapply(1:n, gen_X_list) # covariate
    X <- lapply(seq_len(ncol(X_matrix)), function(i) X_matrix[,i])
    
    theta <- theta_seq[index_theta]
    theta_logit_ss <- c(theta_logit_ss, theta)
    theta_logit_s1s1 <- c(theta_logit_s1s1, theta)
    
    # Calculate probability for each strata: ss, s1s1, ss1
    prob_U_list <- lapply(X, 
                          prob_U, 
                          theta_logit_ss,
                          theta_logit_s1s1,
                          theta_logit_ss1)
    # Sample the strata: 1 - ss - always-taker, 2 - s1s1 - never-taker, 3 - ss1 - complier
    Strata <- mapply(sample_strata, prob = prob_U_list)
    
    # Generate the intercurrent event S
    S <-  mapply(gen_S, Z = Z, strata = Strata)
    
    # Generate the potential outcome
    Y0 <- mapply(gen_Y0, strata = Strata, X = X)
    Y1 <- mapply(gen_Y1, strata = Strata, X = X)
    Y <- mapply(gen_Y, Z = Z, Y0 = Y0, Y1 = Y1)
    
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
  
  
  
}


# Bootstrap process - combine all bootstrap process together -- no use
{
  # general function for parallel process
  call_function_with_seed_all <- function(seed1,
                                          fun1,
                                          fun2, ...){
    # generate one simulated data
    data_full <- sim_data_binary(random_seed = seed1)
    
    # generate random seed for bootstrap data
    set.seed(seed1)
    Seeds <- sample(1:5000000, B, replace = F)
    
    # generate B bootstrap data
    boot_data_all <- sapply(Seeds, sim_bootstrap, data_full = data_full)
    
    # res1 <- lapply(boot_data_all, fun1)
    # res2 <- lapply(boot_data_all, fun2) 
    #  # return a list
    res1 <- lapply(boot_data_all, fun1) # 500 boots costs 4.2 mins using Ding's function
    res2 <- lapply(boot_data_all, fun2) 
    #  # return a matrix, each column is the estimated result for each bootstrap data
    
    # start <- Sys.time()
    # res1 <- lapply(boot_data_all1, fun1)
    # end <- Sys.time()
    # end - start
    
    return(list(Orig = data_full,
                res1,
                res2))
  }
  
  # Generate bootstrap data
  sim_bootstrap <- function(seed,
                            data_full, 
                            N = 500){
    set.seed(seed)
    # N is bootstrap sample size
    boot_samp <- sample(N, N, replace = TRUE)
    boot_data <- data_full[boot_samp, ]
    return(list(boot_data))
  }

  # Function to call for PSPS_M_weighting()
  PSPS_M_weighting_boot <- function(data){
    res_temp <- PSPS_M_weighting(Z = data$Z, 
                                 D = data$S, 
                                 X = as.matrix(data[, 1:5]), 
                                 Y = data$Y,
                                 trc = FALSE, 
                                 ep1 = 1, 
                                 ep0 = 1,
                                 beta.a = NULL,
                                 beta.n = NULL)
    res <- c(res_temp$CACE, res_temp$CACE.reg,
             res_temp$NACE, res_temp$NACE.reg,
             res_temp$AACE, res_temp$AACE.reg)
    return(res)
  }
  
  
  # Function to call for f_PS()
  f_PS_boot <- function(data){
    data <- data |> rename(D = S, X_1 = X1, X_2 = X5, BASE = X3)
    data$USUBJID <- seq(1, nrow(data), 1)
    data <- data |> mutate(indexZD=paste0(Z,D))
    
    # need to check the data structure with Bing
    res_temp <- f_PS(data, 
                     ep1 = 1,
                     ep0 = 1, 
                     beta.a, 
                     beta.n,
                     iter.max = 200,
                     error0 = 10^-6)
  }
  f_PS_boot(data_full)
  # there's still some error with f_PS() function
  
  
  
  library(parallel)
  detectCores() # 12 cores
  
  # Set bootstrap size and random seed for bootstrap data
  B <- 500 
  Seeds1 <- c(1:500)

  start <- Sys.time()
  temp1 <- call_function_with_seed_all(Seeds[1], 
                                   fun1 = PSPS_M_weighting_boot,
                                   fun2 = PSPS_M_weighting_boot,
                                   B)
  end <- Sys.time()
  end - start
  
  
  # parallel case
  start <- Sys.time()
  res <- parallel::mclapply(Seeds, 
                            call_function_with_seed_all,
                            PSPS_M_weighting_boot,
                            PSPS_M_weighting_boot)
  end <- Sys.time()
  end - start
  
}


# Bootstrap process - apply parallel to each bootstrap process
{
  ## Define functions
  # General function
  call_function_with_data <- function(index, boot_data_all){
    data <- boot_data_all[[index]]
    
    # call for Ding's function
    source("C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Desktop/Development R code/1. PS method/Original code/PS_M_weighting.R")
    
    # Function to call for PSPS_M_weighting()
    PSPS_M_weighting_boot <- function(data){
      res_temp <- PSPS_M_weighting(Z = data$Z, 
                                   D = data$S, 
                                   X = as.matrix(data[, 1:5]), 
                                   Y = data$Y,
                                   trc = FALSE, 
                                   ep1 = 1, 
                                   ep0 = 1,
                                   beta.a = NULL,
                                   beta.n = NULL)
      res <- c(res_temp$CACE, res_temp$CACE.reg,
               res_temp$NACE, res_temp$NACE.reg,
               res_temp$AACE, res_temp$AACE.reg)
      return(res)
    }
    
    res <- PSPS_M_weighting_boot(data)
  }
  
  # Generate bootstrap data
  sim_bootstrap <- function(seed,
                            data_full, 
                            N = 500){
    set.seed(seed)
    # N is bootstrap sample size
    boot_samp <- sample(N, N, replace = TRUE)
    boot_data <- data_full[boot_samp, ]
    return(list(boot_data))
  }
  
  # Load Ding's function
  source("C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Desktop/Development R code/1. PS method/Original code/PS_M_weighting.R")
  
  # Bootstrap percentile CI
  boot_perct_CI_fun <- function(x, alpha){
    res <- c(
      quantile(x, 1 - alpha, na.rm = T),
      quantile(x, alpha, na.rm = T)
    )
    return(res)
  }
    
  # Bootstrap standard error
  boot_sd_fun <- function(x){
    res <- sd(x, na.rm = T)
    return(list(res))
  }
  
  boot_sd_CI_fun <- function(boot_sd, orig_mean, Zstar){
    boot_sd <- boot_sd[[1]]
    res <- c(orig_mean - Zstar * boot_sd,
             orig_mean + Zstar * boot_sd)
    return(res)
  }
  

  # Define cores before runing
  library(parallel)
  library(doParallel)
  library(foreach)
  
  
  # detectCores() # 12 cores
  
  # no_cores <- detectCores(logical = TRUE) 
  # cl <- makeCluster(no_cores-1, type='PSOCK')  
  my.cluster <- parallel::makeCluster(
    11, 
    type = "PSOCK"
  )
  
  clusterExport(my.cluster, 
                c("call_function_with_data",
                  "sim_bootstrap", 
                  "boot_perct_CI_fun", 
                  "boot_sd_fun",
                  "boot_sd_CI_fun",
                  "PS_pred",
                  "PSPS_M_weighting"))
  
  #register it to be used by %dopar%
  doParallel::registerDoParallel(cl = my.cluster)
  
  #check if it is registered (optional)
  foreach::getDoParRegistered()
  
  #how many workers are available? (optional)
  foreach::getDoParWorkers()
  
  set.seed(123)
  N <- 500 # simulation size
  Seeds1 <- sample(1:5000000, N, replace = F)
  
  boot_fun <- function(seed,
                       alpha = 0.975,
                       B = 300){
    
    data_orig <- sim_data_binary(random_seed = seed)
    
    # Estimate the simulated data 
    Orig_est <- PSPS_M_weighting(Z = data_orig$Z, 
                                 D = data_orig$S, 
                                 X = as.matrix(data_orig[, 1:5]), 
                                 Y = data_orig$Y,
                                 trc = FALSE, 
                                 ep1 = 1, 
                                 ep0 = 1,
                                 beta.a = NULL,
                                 beta.n = NULL)[1:6]
    orig_mean <- unlist(Orig_est)
    
    # generate B bootstrap data
    set.seed(seed)
    Seeds <- sample(1:5000000, B, replace = F)
    boot_data_all <- sapply(Seeds, sim_bootstrap, data_full = data_orig)
    
    # parallel case
    
    # Estimate B bootstrap data using parallel
    system.time(boot_est <- foreach::foreach(i = 1:B,
                                             .export = c("boot_data_all",
                                                         "call_function_with_data",
                                                         "sim_bootstrap",
                                                         "boot_perct_CI_fun",
                                                         "boot_sd_fun",
                                                         "boot_sd_CI_fun",
                                                         "PS_pred",
                                                         "PSPS_M_weighting"),
                                             .combine=cbind) %dopar% {
                                               # combine = rbind # for row vector
                                               # combind = 'c' # for list
      index <- i
      call_function_with_data(index, boot_data_all = boot_data_all)
    })

   
    # Estimate without using parallel
    # system.time(for(i in 1:50){
    #   index <- i
    #   call_function_with_data(index, boot_data_all = boot_data_all)
    # })

    # bootstrap percentile CI
    boot_perct <- apply(boot_est, 1, boot_perct_CI_fun, alpha = alpha)

    # bootstrap sd
    boot_sd <- apply(boot_est, 1, boot_sd_fun)
    # calculate boot CI and boot-t using boot_est
    boot_sd_est <- unlist(boot_sd)

    # bootstrap_sd_cI
    Zstar <- qnorm(alpha)
    boot_sd_CI <- mapply(boot_sd_CI_fun,
                         boot_sd = boot_sd,
                         orig_mean = Orig_est,
                         Zstar = Zstar)


    Res <- as.data.frame(orig_mean) |> mutate(boot_sd_est,
                                              boot_perct_l = boot_perct[1, ],
                                              boot_perct_u = boot_perct[2, ],
                                              boot_sd_l = boot_sd_CI[1, ],
                                              boot_sd_u = boot_sd_CI[2, ])


    return(list(Res))
  }
  
  # parallel::stopCluster(cl = my.cluster)
  
  # Find one case for i = 1, boot - 327
  # There will be NA value
  
  # The following part not works
  # First run 1:50
  Res <- c()
  start <- Sys.time()
  for(i in 251:300){
    print(i)
    seed <- Seeds1[i]
    system.time(temp1 <- boot_fun(seed))
    Res <- c(Res, temp1)
  }
  end <- Sys.time()
  end - start
  # system.time(boot_fun(Seeds1[1]))
  # # 2.5 mins for one sample using 11 cores
  
  
  # Without using function boot_fun()
  {
    # Res <- c()
    # start <- Sys.time()
    # alpha = 0.975
    # B = 300
    # for(i in 1:2){
    #   print(i)
    #   seed <- Seeds1[i]
    #   data_orig <- sim_data_binary(random_seed = seed)
    #   
    #   # Estimate the simulated data 
    #   Orig_est <- PSPS_M_weighting(Z = data_orig$Z, 
    #                                D = data_orig$S, 
    #                                X = as.matrix(data_orig[, 1:5]), 
    #                                Y = data_orig$Y,
    #                                trc = FALSE, 
    #                                ep1 = 1, 
    #                                ep0 = 1,
    #                                beta.a = NULL,
    #                                beta.n = NULL)[1:6]
    #   orig_mean <- unlist(Orig_est)
    #   
    #   # generate B bootstrap data
    #   set.seed(seed)
    #   Seeds <- sample(1:5000000, B, replace = F)
    #   boot_data_all <- sapply(Seeds, sim_bootstrap, data_full = data_orig)
    #   
    #   # parallel case
    #   
    #   # Estimate B bootstrap data using parallel
    #   system.time(boot_est <- foreach::foreach(i = 1:B, 
    #                                            .export = c("boot_data_all",
    #                                                        "call_function_with_data",
    #                                                        "sim_bootstrap", 
    #                                                        "boot_perct_CI_fun", 
    #                                                        "boot_sd_fun",
    #                                                        "boot_sd_CI_fun",
    #                                                        "PS_pred",
    #                                                        "PSPS_M_weighting"),
    #                                            .combine=cbind) %dopar% {
    #                                              index <- i
    #                                              call_function_with_data(index, boot_data_all = boot_data_all)
    #                                            })
    #   
    #   # bootstrap percentile CI
    #   boot_perct <- apply(boot_est, 1, boot_perct_CI_fun, alpha = alpha)
    #   
    #   # bootstrap sd
    #   boot_sd <- apply(boot_est, 1, boot_sd_fun)
    #   # calculate boot CI and boot-t using boot_est
    #   boot_sd_est <- unlist(boot_sd)
    #   
    #   # bootstrap_sd_cI
    #   Zstar <- qnorm(alpha)
    #   boot_sd_CI <- mapply(boot_sd_CI_fun,
    #                        boot_sd = boot_sd,
    #                        orig_mean = Orig_est,
    #                        Zstar = Zstar)
    #   
    #   
    #   temp1 <- as.data.frame(orig_mean) |> mutate(boot_sd_est,
    #                                               boot_perct_l = boot_perct[1, ],
    #                                               boot_perct_u = boot_perct[2, ],
    #                                               boot_sd_l = boot_sd_CI[1, ],
    #                                               boot_sd_u = boot_sd_CI[2, ])
    #   
    #   temp1 <- list(temp1)
    #   Res <- c(Res, temp1)
    # }
    # end <- Sys.time()
    # end - start
  }
  
  saveRDS(Res, file = "C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Desktop/Development R code/1. PS method/Binary_251_300")

  
  
    
  registerDoSEQ()
  stopCluster(my.cluster)
  
}




