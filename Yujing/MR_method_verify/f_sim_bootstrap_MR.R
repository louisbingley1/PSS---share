rm(list=ls())

# Yang's estimation code - MR method
{
  library(pace)
  # out.flu <- pace::pace(X,Z,S,Y,family.Y="binomial",nboot=200)
  
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


# Estimation process - Yang's code already have bootstrap process
#  No need to use parallel process for this function
# When we use Yang's code, we set:
#    simulation times = 300 (the paper's size is 1000)
#    simulated data sample size = 500 (this is same as the paper)
#    bootstrap size = 300 (the paper's size is 500)
{
  ## Define functions
  # General function
  call_function_with_data <- function(data, B){
   
    # Function to call for PSPS_M_weighting()
    MR_weighting <- function(data){
      res_temp <-  pace::pace(X = as.matrix(data[, 1:5]),
                              Z = data$Z,
                              S = data$S,
                              Y = data$Y,
                              family.Y="binomial",
                              nboot = B)
        
      res <- as.data.frame(res_temp)
      return(res)
    }
    
    res <- MR_weighting(data)
  }
  
  # Bootstrap sd-based CI
  boot_sd_CI_fun <- function(boot_sd, orig_mean, Zstar){
    res <- c(orig_mean - Zstar * boot_sd,
             orig_mean + Zstar * boot_sd)
    return(res)
  }
  
  set.seed(123)
  N <- 500 # simulation size
  Seeds1 <- sample(1:5000000, N, replace = F)
  
  MR_Est_fun <- function(seed,
                         alpha = 0.975,
                         B = 300){
      
      data_orig <- sim_data_binary(random_seed = seed)
      
      # Estimate the simulated data 
      Orig_est <-  call_function_with_data(data = data_orig,
                                           B = 300)
      return(Orig_est)
  }
  
  
  Res <- c()
  start <- Sys.time()
  for(i in 401:500){
    print(i)
    seed <- Seeds1[i]
    temp1 <- MR_Est_fun(seed)
    Res <- rbind(Res, temp1)
  }
  end <- Sys.time()
  end - start
  
  saveRDS(Res, file = "C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Desktop/Development R code/2. MR method/Binary_401_500")
  
  
}




