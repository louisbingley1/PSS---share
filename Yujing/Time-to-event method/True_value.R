library(simsurv)

# We follow the simulation setting in Chao Cheng's paper


#### data generation function ####
{
  
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
    Index_X <- sample(1:n*2, n, replace = F)
    
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
  
  data_popu <- f_sim(index = 456, n = 10^6)
  
}


#### Calculate the true value for PCE ####
# the true value is not stable in this strata
{
  
  logit <- function(y){
    # exp(1+2*x)/ (exp(1+2*x) + 1)
    exp(y)/ (exp(y) + 1)
  }
  
  f_surv <- function(betax, u){
    res <- exp(- u * exp(betax))
    return(res)
  }
  
  tau10 <- function(data, u){
    Z <- data$Z
    S <- data$S
    surv_time <- data$Time
    surv_status <- data$Status
    X1 <- data$X1
    X2 <- data$X2
    X3 <- data$X3
    X4 <- data$X4
    X5 <- data$X5
    
    pi_X <-  1 / (1 + exp( - 0.5 * X4 - 0.4 * X5))
    
    p1_X <- logit(-0.5 + 1 + 0.5 * X4 + 0.4 * X5)
    p0_X <- logit(-0.5 + 0.5 * X4 + 0.4 * X5)
    
    p1 <- mean(p1_X)
    p0 <- mean(p0_X)
    
    e_10_X <- p1_X - p0_X
    e_00_X <- 1 - p1_X
    e_11_X <- p0_X
    
    
    surv_11_X <- f_surv(betax = -1 + 0.5 - 0.3 * X4 + 0.2 * X5, 
                        u)
    surv_00_X <- f_surv(betax = -1 + 0 + 0.2 * X3 + 0.4 * X4 + 0.5 * X5,
                        u)
    
    censor_11_X <- f_surv(betax = -2 + 0.3 * X4 + 0.2 * X5, 
                          u)
    censor_00_X <- f_surv(betax = -2 + 0.3 * X4 + 0.2 * X5, 
                          u)
    status <- (surv_status >= u)
    
    
    # res1 <- mean(e_10_X*S*Z*status / (p1-p0) / p1_X / pi_X / censor_11_X - e_10_X * (1-S) * (1-Z) * status / (p1-p0) / (1-p0_X)/ (1 - pi_X)/ censor_00_X)
    res2 <-  mean(((p1_X - p0_X) / (p1 - p0)) * (surv_11_X - surv_00_X))
    res3 <- 1 / (p1-p0) * mean( (S*Z / pi_X - S*(1-Z)/(1 - pi_X)) * (surv_11_X - surv_00_X) )
    
    return(c(res2, res3))
  }
  
  tau00 <- function(data, u){
    Z <- data$Z
    S <- data$S
    surv_time <- data$Time
    surv_status <- data$Status
    X1 <- data$X1
    X2 <- data$X2
    X3 <- data$X3
    X4 <- data$X4
    X5 <- data$X5
    
    pi_X <-  1 / (1 + exp( - 0.5 * X4 - 0.4 * X5))
    
    p1_X <- logit(-0.5 + 1 + 0.5 * X4 + 0.4 * X5)
    p0_X <- logit(-0.5 + 0.5 * X4 + 0.4 * X5)
    
    p1 <- mean(p1_X)
    p0 <- mean(p0_X)
    
    e_10_X <- p1_X - p0_X
    e_00_X <- 1 - p1_X
    e_11_X <- p0_X
    
    
    surv_10_X <- f_surv(betax = -1 + 0.4 * X4 - 0.3 * X5, 
                        u)
    surv_00_X <- f_surv(betax = -1 + 0.2 * X3 + 0.4 * X4 + 0.5 * X5,
                        u)
    
    censor_10_X <- f_surv(betax = -2 + 0.3 * X4 + 0.2 * X5, 
                          u)
    censor_00_X <- f_surv(betax = -2 + 0.3 * X4 + 0.2 * X5, 
                          u)
    
    status <- (surv_status >= u)
    
    # res1 <- mean(( 1 - S) * Z * status / (1 - p1) / pi_X / censor_10_X) - mean(e_00_X * (1-S)*(1-Z)*status / (1-p1) / (1-p0_X)/ (1 - pi_X)/ censor_00_X )
    res2 <- mean( e_00_X / (1-p1) * (surv_10_X - surv_00_X) )
    res3 <- 1 / (1 - p1) * mean( (1 - S*Z/pi_X) *  (surv_10_X - surv_00_X)  )
    
    return(c(res2, res3))
  }
  
  tau11 <- function(data, u){
    Z <- data$Z
    S <- data$S
    surv_time <- data$Time
    surv_status <- data$Status
    X1 <- data$X1
    X2 <- data$X2
    X3 <- data$X3
    X4 <- data$X4
    X5 <- data$X5
    
    pi_X <-  1 / (1 + exp( - 0.5 * X4 - 0.4 * X5))
    
    p1_X <- logit(-0.5 + 1 + 0.5 * X4 + 0.4 * X5)
    p0_X <- logit(-0.5 + 0.5 * X4 + 0.4 * X5)
    
    p1 <- mean(p1_X)
    p0 <- mean(p0_X)
    
    e_10_X <- p1_X - p0_X
    e_00_X <- 1 - p1_X
    e_11_X <- p0_X
    
    surv_11_X <- f_surv(betax = -1 + 0.5 - 0.3 * X4 + 0.2 * X5, 
                        u)
    surv_01_X <- f_surv(betax = -1 + 0.5 + 0.4 * X4 + 0.2 * X5,
                        u)
    
    censor_11_X <- f_surv(betax = -2 + 0.3 * X4 + 0.2 * X5, 
                          u)
    censor_01_X <- f_surv(betax = -2 + 0.3 * X4 + 0.2 * X5, 
                          u)
    status <- (surv_status >= u)
    
    # res1 <-  mean(e_11_X*S*Z*status / p0 / p1_X / pi_X / censor_11_X ) - 
    #   mean( S * (1-Z) * status / p0 / (1-pi_X)/ censor_01_X)
    res2 <- mean( e_11_X / p0 * (surv_11_X - surv_01_X) )
    res3 <- 1 / p0 * mean( S*(1-Z)/(1 - pi_X) *  (surv_11_X - surv_01_X)  )
    
    return(c(res2, res3))
  }
  
}


#### example code to estimate and compare with the true value ####
{
  
  source("C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Desktop/Development R code/3. Time-to-event method/pace_time.R")
  
  data <- f_sim(index = 456, n = 10^5)
  
  
  temp <- estforboot_time(X = data[, 1:5],
                          Z = data$Z,
                          S = data$S,
                          surv_time = data$Time,
                          surv_status = data$Status,
                          family = "time",
                          u = 1)
  
  
  true10 <- tau10(data=data_popu, u =1)
  true00 <- tau00(data=data_popu, u =1)
  true11 <- tau11(data=data_popu, u =1)
  
  
  true10
  temp$tau10w
  temp$tau10reg
  temp$tau10reg2
  temp$tau10sw
  
  
  true00
  temp$tau00w
  temp$tau00reg
  temp$tau00reg2
  temp$tau00sw
  
  
  true11
  temp$tau11w
  temp$tau11reg
  temp$tau11reg2
  temp$tau11sw
  
  
  pace_time(X = data[, 1:5],
            Z = data$Z,
            S = data$S,
            surv_time = data$Time,
            surv_status = data$Status,
            family = "time",
            u = 1,
            nboot = 10)
  
  
}


