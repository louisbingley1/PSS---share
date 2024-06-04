
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
    if(strata == 1){  return(Z)  }else          # complier (1)
      if(strata == 2){ return(c(1)) }else       # always taker (2)
                       { return(c(0))}          # never taker (3)
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
  gen_U   <- function(Strata){ 
    if(Strata==1){ return("C")}else
      if(Strata==2){return("A")}else
                    {return("N")}
  } 
  
  
  
}



########################################
# simu data funtion
########################################

f_sim_vali_Bing <- function(n,                                 # sample size of simulated data
                     random_seed ,                             # random seed to generate different simulated data
                     theta_seq ,                               # the sequence of all possible theta for theta_logit
                     index_theta  ,                            # range from 1 to 5, index for which theta from theta_seq will be added to the end of theta_logit
                     theta_logit_ss  ,                         # the first 5 coefficients for logit model for ss strata - always-taker
                     theta_logit_s1s1  ,                       # the first 5 coefficients for logit model for s1s1 strata - never-taker
                     theta_logit_ss1                           # the first 5 coefficients for logit model for ss1 strata - complier
){
  
  set.seed(random_seed)
  USUBJID          <- seq(1,n,1)
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
  d                <- Y1-Y0
  U                <- mapply(gen_U, Strata=Strata)
  Simu_data <- data.frame( USUBJID=USUBJID,
                           X1 = X_matrix[2, ],
                           X2 = X_matrix[3, ],
                           X3 = X_matrix[4, ],
                           X4 = X_matrix[5, ],
                           X5 = X_matrix[6, ],
                           Z = Z,
                           D = S,
                           Y = Y,
                           Y0 = Y0,
                           Y1 = Y1,
                           d  = d,
                           U  = U)
  
   #---------------
  # true ACE at Tm
  #---------------
  {
    true_d_Tm_adhact  = mean( Simu_data %>% filter( U=='A') %>% pull(d) )
    true_d_Tm_adhpbo  = mean( Simu_data %>% filter( U=='N') %>% pull(d) )
    true_d_Tm_adhboth = mean( Simu_data %>% filter( U=='C') %>% pull(d) )
    true_d_Tm_adhnei  = mean( Simu_data %>% filter( U=='D') %>% pull(d) )
    true_d_Tm         = list( true_d_Tm_adhact  = true_d_Tm_adhact ,
                              true_d_Tm_adhpbo  = true_d_Tm_adhpbo ,
                              true_d_Tm_adhboth = true_d_Tm_adhboth,
                              true_d_Tm_adhnei  = true_d_Tm_adhnei )
  }   
  
  {
    true_d_Tm_adhact_nsl  = mean( Simu_data %>% filter(U=='A' & Z==1) %>% pull(Y) )- mean( Simu_data %>% filter(U=='A' & Z==0) %>% pull(Y) )
    true_d_Tm_adhpbo_nsl  = mean( Simu_data %>% filter(U=='N' & Z==1 )%>% pull(Y) )- mean( Simu_data %>% filter(U=='N' & Z==0) %>% pull(Y) )
    true_d_Tm_adhboth_nsl = mean( Simu_data %>% filter(U=='C' & Z==1 )%>% pull(Y) )- mean( Simu_data %>% filter(U=='C' & Z==0) %>% pull(Y) )
    true_d_Tm_nsl         = list( true_d_Tm_adhact_nsl  = true_d_Tm_adhact_nsl ,
                                  true_d_Tm_adhpbo_nsl  = true_d_Tm_adhpbo_nsl ,
                                  true_d_Tm_adhboth_nsl = true_d_Tm_adhboth_nsl  )
  } 
   
  return(list(data=Simu_data,
              true_d_Tm   = true_d_Tm,
              true_d_Tm_nsl=true_d_Tm_nsl))
}


 
