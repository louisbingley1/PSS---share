rm(list=ls())
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
                            index_theta = 5, 
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
    
    

# temp <- sim_data_binary(random_seed = 1)
# temp1 <- sim_data_binary(random_seed = 1)
# all.equal(temp, temp1)

# Use a large simulated dataset to approximate the true value
temp <- sim_data_binary( n = 2000000,
                         random_seed = 2)

# Check the potential value in the simulated data
AACE_true <- mean(temp$Y1[temp$U == 1] - temp$Y0[temp$U == 1])# ss
NACE_true <- mean(temp$Y1[temp$U == 2] - temp$Y0[temp$U == 2])# s1s1
CACE_true <- mean(temp$Y1[temp$U == 3] - temp$Y0[temp$U == 3])# ss1
# They have the mean value as what we want 


source("C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Desktop/Development R code/1. PS method/Original code/PS_M_weighting.R")


#Main function
#Z: randomization
#D: treatment received
#X: covariate matrix: the first column is NOT 11111
#U: (latent) principal stratification variable, 1 complier, 2 always taker, 3 never taker
#Y: outcome of interest
#trc: truncation by death indicator, default FALSE. If TRUE only SACE (i.e. AACE) is calculated.
#ep1, ep0: sensitivity parameters in Proposition 4, Section 6.1.
#beta.a, beta.n: initial values for the paramaters in the multiple logistic regression
res_temp <- PSPS_M_weighting(Z = temp$Z, 
                             D = temp$S, 
                             X = as.matrix(temp[, 1:5]), 
                             Y = temp$Y,
                             trc = FALSE, 
                             ep1 = 1, 
                             ep0 = 1,
                             beta.a = NULL,
                             beta.n = NULL)


Seeds <- sample(59999999, size = 200, replace = F)

CACE_all <- rep(0, 200)
CACE.reg_all <- rep(0, 200)
NACE_all <- rep(0, 200)
NACE.reg_all <- rep(0, 200)
AACE_all <- rep(0, 200)
AACE.reg_all <- rep(0, 200)

for(i in 1:200){
  print(i)
  temp <- sim_data_binary(random_seed = Seeds[i])
  res_temp <- PSPS_M_weighting(Z = temp$Z, 
                               D = temp$S, 
                               X = as.matrix(temp[, 1:5]), 
                               Y = temp$Y,
                               trc = FALSE, 
                               ep1 = 1, 
                               ep0 = 1,
                               beta.a = NULL,
                               beta.n = NULL)
  
  CACE_all[i] <- res_temp$CACE
  CACE.reg_all[i] <- res_temp$CACE.reg
  NACE_all[i] <- res_temp$NACE
  NACE.reg_all[i] <- res_temp$NACE.reg
  AACE_all[i] <- res_temp$AACE
  AACE.reg_all[i] <- res_temp$AACE.reg
}

boxplot(CACE_all - CACE_true)
boxplot(NACE_all - NACE_true)
boxplot(AACE_all - AACE_true)

boxplot(CACE.reg_all - CACE_true)
boxplot(NACE.reg_all - NACE_true)
boxplot(AACE.reg_all - AACE_true)


