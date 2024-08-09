rm(list=ls())
# library(simsurv)

################################
##### data generation code #####
{
  # We follow the simulation setting in Chao Cheng's paper
  
  gen_X <- function(index){
    
    set.seed(index)
    
    X1 <-  rbinom(n = 1, size = 1, prob = 0.5)
    
    X2 <- rnorm(n = 1)
    X3 <- rnorm(n = 1)
    
    X4 <- X2^2 - 1
    X5 <- X3^2 - 1
    
    return(c(X1, X2, X3, X4, X5))
    
  }
  
  gen_Z <- function(x){
    
    prob <- 1 / (1 + exp( - 0.5 * x[4] - 0.4 * x[5]))
    Z <- rbinom(n = 1, size = 1, prob = prob)
    return(Z)
    
  }
  
  gen_S <- function(z, x){
    
    prob <- exp(-0.5 + z + 0.5 * x[4] + 0.4 * x[5]) / (1 + exp(-0.5 + z + 0.5 * x[4] + 0.4 * x[5]) )
    S <- rbinom(n = 1, size = 1, prob = prob)
    return(S)
    
  }
  
  gen_surv <- function(x, z, s){
    
    if(z == 0 & s == 0){
      psi <- c(0, 0, 0.2, 0.4, 0.5)
    }else if(z == 0 & s == 1){
      psi <- c(0, 0, 0, 0.4, 0.2)
    }else if(z == 1 & s == 0){
      psi <- c(0, 0, 0, 0.4, -0.3)
    }else{
      psi <- c(0, 0, 0, -0.3, 0.2)
    }
    
    psi_C <- c(0, 0, 0, 0.3, 0.2)
    
    # Exp latent event times
    v1 <- runif(n = 1)
    Tlat <- (- log(v1) / exp(-1 + 0.5 * s + psi %*% x) )
    
    # censoring times
    v2 <- runif(n = 1)
    C <- (- log(v2) / exp(-2 + psi_C %*% x) )
    
    # follow-up times and event indicators
    time <- pmin(Tlat, C)
    status <- as.numeric(Tlat <= C)
    
    return(c(time, status))
  }
  
  f_sim <- function(index = 123,
                    n = 1000){
    
    set.seed(index)
    Index_X <- sample(1:5000000, n, replace = F)
    
    X_list <- lapply(Index_X, gen_X)
    X <- mapply(gen_X, Index_X)
    
    Z <- apply(X, 2, FUN = gen_Z)
    
    S <- mapply(gen_S, z = Z, x = X_list)
    
    Surv_info <- mapply(gen_surv, x = X_list, z = Z, s = S)
    
    X <- data.frame(t(X))
    colnames(X) <- c("X1", "X2", "X3", "X4", "X5")
    
    data <- cbind(X, Z) 
    data <- cbind(data, S)
    
    Surv <- data.frame(t(Surv_info))
    colnames(Surv) <- c("Time", "Status")
    
    data <- cbind(data, Surv)
    
    return(data) 
  }
  
  data <- f_sim(index = 123)
  
}



###########################
##### data simulation #####
{
  source("C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Desktop/Development R code/3. Time-to-event method/pace_time.R")
  
  temp <- estforboot_time(X = data[, 1:5],
                  Z = data$Z,
                  S = data$S,
                  surv_time = data$Time,
                  surv_status = data$Status,
                  family = "time",
                  u = 1)
  
  pace_time(X = data[, 1:5],
            Z = data$Z,
            S = data$S,
            surv_time = data$Time,
            surv_status = data$Status,
            family = "time",
            u = 1,
            nboot = 10)
  
  
}

