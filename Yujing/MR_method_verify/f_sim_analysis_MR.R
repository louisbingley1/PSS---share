rm(list=ls())
library(purrr)
library(tidyr)
library(ggplot2)


setwd("C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Desktop/Development R code/2. MR method")


# list all files in data_temp
base_dir <- "C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Desktop/Development R code/2. MR method/data_temp/"
all_files <- paste0(base_dir, list.files(base_dir, recursive = TRUE))
data <-  map(all_files, readRDS)
data2 <-  do.call(rbind, data)

data2_list <- split(data2, seq(nrow(data2)))

# Write a function to verify the true value
AACE_true <-  -0.05754559 # 11 in Yang's code
NACE_true <- -0.05727229 # 00 in Yang's code
CACE_true <- -0.1145532 # 10 in Yang's code


# There are 5 estimators for each strata


###################################
########  Define functions ########
# Absolute bias function
Abs_bias_fun <- function(CACE_true, 
                         NACE_true, 
                         AACE_true,
                         input){
  bias_tau10 <- input[1:5] - CACE_true
  bias_tau00 <- input[6:10] - NACE_true
  bias_tau11 <- input[11:15] - AACE_true
  
  bias_res <- cbind(bias_tau10, 
                    bias_tau00,
                    bias_tau11)
  return(list(bias_res))
}

# Relative bias function
Rlt_bias_fun <- function(CACE_true, 
                         NACE_true, 
                         AACE_true,
                         input){
  bias_tau10 <- (input[1:5] - CACE_true) / CACE_true
  bias_tau00 <- (input[6:10] - NACE_true) / NACE_true
  bias_tau11 <- (input[11:15] - AACE_true) / AACE_true
  
  bias_res <- cbind(bias_tau10, 
                    bias_tau00,
                    bias_tau11)
  return(list(bias_res))
}

# Bootstrap sd-based CI
cov_boot_sd_check <- function(boot_sd,
                              orig_mean,
                              Zstar,
                              true){
  left <- orig_mean - Zstar * boot_sd
  right <- orig_mean + Zstar * boot_sd
  res <- (left <= true) * (true <= right)
  return(res)
}


Cov_bootsd_fun <- function(CACE_true,
                           NACE_true, 
                           AACE_true,
                           input,
                           alpha = 0.975){
  Est_tau10 <- input[1:5]
  Est_tau00 <- input[6:10]
  Est_tau11 <- input[11:15]
  
  Sd_tau10 <- sqrt(input[16:20])
  Sd_tau00 <- sqrt(input[21:25])
  Sd_tau11 <- sqrt(input[26:30])
  
  Zstar <- qnorm(alpha)
  
  Cov_tau10 <- mapply(cov_boot_sd_check,
                      boot_sd = Sd_tau10,
                      orig_mean = Est_tau10,
                      Zstar,
                      true = CACE_true)
  
  Cov_tau00 <- mapply(cov_boot_sd_check,
                      boot_sd = Sd_tau00,
                      orig_mean = Est_tau00,
                      Zstar,
                      true = NACE_true)
  
  Cov_tau11 <- mapply(cov_boot_sd_check,
                      boot_sd = Sd_tau11,
                      orig_mean = Est_tau11,
                      Zstar,
                      true = AACE_true)
  
  Res <- c(Cov_tau10, 
           Cov_tau00,
           Cov_tau11)
  
  
  return(list(Res))
  
  
}



#################################
########  Calculate Bias ########

# Calculate the absolute bias and make the boxplot 
{
  Abs_bias_res <- mapply(Abs_bias_fun,
                         input = data2_list,
                         CACE_true,
                         NACE_true,
                         AACE_true)
  
  Abs_bias_res1 <- matrix(unlist(Abs_bias_res), ncol = 15, byrow = T)
  colnames(Abs_bias_res1) <- colnames(Abs_bias_res[[1]])
  
  # check the boxplot for each bias 
  Abs_bias <- as.data.frame(Abs_bias_res1)
  
  # check the average bias value
  apply(Abs_bias, 2, mean)
  
  # Complier Strata
  {
    Abs_bias_tau10 <- Abs_bias[, 1:5]
    Abs_bias_tau10_box <- Abs_bias_tau10 %>% 
      pivot_longer(
        cols = "tau10w":"tau10aw", 
        names_to = "Estimator",
        values_to = "absolute_bias"
      )
    Abs_bias_tau10_box$Estimator <- factor(Abs_bias_tau10_box$Estimator, 
                                           levels = colnames(Abs_bias_res[[1]])[1:5])
    
    p1 <- ggplot(data = Abs_bias_tau10_box, aes(x = Estimator, y = absolute_bias)) +
      geom_boxplot()+
      labs(title="Absolute Bias for Each Estimator in Complier Strata")
    p1
    
    ggsave("binary_simu_res/abs_bias_10.png", 
           plot=p1, 
           width=7, height=6, 
           dpi=500)
    
  }
  
  # Never-taker Strata
  {
    Abs_bias_tau00 <- Abs_bias[, 6:10]
    Abs_bias_tau00_box <- Abs_bias_tau00 %>% 
      pivot_longer(
        cols = "tau00w":"tau00aw", 
        names_to = "Estimator",
        values_to = "absolute_bias"
      )
    Abs_bias_tau00_box$Estimator <- factor(Abs_bias_tau00_box$Estimator, 
                                           levels = colnames(Abs_bias_res[[1]])[6:10])
    
    p2 <- ggplot(data = Abs_bias_tau00_box, aes(x = Estimator, y = absolute_bias)) +
      geom_boxplot()+
      labs(title="Absolute Bias for Each Estimator in Never-taker Strata")
    p2
    
    ggsave("binary_simu_res/abs_bias_00.png", 
           plot=p2, 
           width=7, height=6, 
           dpi=500)
  }
  
  # Always-taker Strata
  {
    Abs_bias_tau11 <- Abs_bias[, 11:15]
    Abs_bias_tau11_box <- Abs_bias_tau11 %>% 
      pivot_longer(
        cols = "tau11w":"tau11aw", 
        names_to = "Estimator",
        values_to = "absolute_bias"
      )
    Abs_bias_tau11_box$Estimator <- factor(Abs_bias_tau11_box$Estimator, 
                                           levels = colnames(Abs_bias_res[[1]])[11:15])
    
    p3 <- ggplot(data = Abs_bias_tau11_box, aes(x = Estimator, y = absolute_bias)) +
      geom_boxplot()+
      labs(title="Absolute Bias for Each Estimator in Always-taker Strata")
    p3
    
    ggsave("binary_simu_res/abs_bias_11.png", 
           plot=p3, 
           width=7, height=6, 
           dpi=500)
  }
 
  
}


# Calculate the relative bias and make the boxplot 
{
  
  Rlt_bias_res <- mapply(Rlt_bias_fun,
                         input = data2_list,
                         CACE_true,
                         NACE_true,
                         AACE_true)
  
  Rlt_bias_res1 <- matrix(unlist(Rlt_bias_res), ncol = 15, byrow = T)
  colnames(Rlt_bias_res1) <- colnames(Rlt_bias_res[[1]])
  
  # check the boxplot for each bias 
  Rlt_bias <- as.data.frame(Rlt_bias_res1)
  
  # check the average bias value
  apply(Rlt_bias, 2, mean)
  
  
  # Complier Strata
  {
    Rlt_bias_tau10 <- Rlt_bias[, 1:5]
    Rlt_bias_tau10_box <- Rlt_bias_tau10 %>% 
      pivot_longer(
        cols = "tau10w":"tau10aw", 
        names_to = "Estimator",
        values_to = "relative_bias"
      )
    Rlt_bias_tau10_box$Estimator <- factor(Rlt_bias_tau10_box$Estimator, 
                                           levels = colnames(Rlt_bias_res[[1]])[1:5])
    
    p4 <- ggplot(data = Rlt_bias_tau10_box, aes(x = Estimator, y = relative_bias)) +
      geom_boxplot()+
      labs(title="Relative Bias for Each Estimator in Complier Strata")
    p4
    
    ggsave("binary_simu_res/rlt_bias_10.png", 
           plot=p4, 
           width=7, height=6, 
           dpi=500)
    
  }
  
  # Never-taker Strata
  {
    Rlt_bias_tau00 <- Rlt_bias[, 6:10]
    Rlt_bias_tau00_box <- Rlt_bias_tau00 %>% 
      pivot_longer(
        cols = "tau00w":"tau00aw", 
        names_to = "Estimator",
        values_to = "relative_bias"
      )
    Rlt_bias_tau00_box$Estimator <- factor(Rlt_bias_tau00_box$Estimator, 
                                           levels = colnames(Rlt_bias_res[[1]])[6:10])
    
    p5 <- ggplot(data = Rlt_bias_tau00_box, aes(x = Estimator, y = relative_bias)) +
      geom_boxplot()+
      labs(title="Relative Bias for Each Estimator in Never-taker Strata")
    p5
    
    ggsave("binary_simu_res/rlt_bias_00.png", 
           plot=p5, 
           width=7, height=6, 
           dpi=500)
    
  }
  
  # Always-taker Strata
  {
    Rlt_bias_tau11 <- Rlt_bias[, 11:15]
    Rlt_bias_tau11_box <- Rlt_bias_tau11 %>% 
      pivot_longer(
        cols = "tau11w":"tau11aw", 
        names_to = "Estimator",
        values_to = "relative_bias"
      )
    Rlt_bias_tau11_box$Estimator <- factor(Rlt_bias_tau11_box$Estimator, 
                                           levels = colnames(Rlt_bias_res[[1]])[11:15])
    
    p6 <- ggplot(data = Rlt_bias_tau11_box, aes(x = Estimator, y = relative_bias)) +
      geom_boxplot()+
      labs(title="Relative Bias for Each Estimator in Always-taker Strata")
    p6
    
    ggsave("binary_simu_res/rlt_bias_11.png", 
           plot=p6, 
           width=7, height=6, 
           dpi=500)
  }
  
  
  
  
}


#####################################
########  Calculate Coverage ########
{
  # For the bootstrap percentile CI
  Cov_bootsd_res <- mapply(Cov_bootsd_fun, 
                         input = data2_list,
                         CACE_true, 
                         NACE_true, 
                         AACE_true)
  
  Cov_bootsd_res1 <- matrix(unlist(Cov_bootsd_res), ncol = 15, byrow = T)
  Cov_bootsd <- as.data.frame(Cov_bootsd_res1)
  colnames(Cov_bootsd) <- colnames(data2_list[[1]])[1:15]
  
  # check the average bias value
  apply(Cov_bootsd, 2, mean)
  
}



