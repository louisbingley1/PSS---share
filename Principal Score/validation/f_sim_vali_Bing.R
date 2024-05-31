
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
              multi_logit_s1s1( Xi,   theta_logit_ss,  theta_logit_s1s1,  theta_logit_ss1),
              
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
  gen_Y            <- function(strata, Z, Y0, Y1){
    if(strata == 2){ return(c(Y1))}else             # always-taker
      if(strata == 3){ return(c(Y0))}else           # never-taker
                         {return(Z*Y1 + (1-Z)*Y0) } # complier 
      
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




