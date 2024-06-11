rm(list=ls())
library(purrr)
library(tidyr)
library(ggplot2)


setwd("C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Desktop/Development R code/1. PS method")


# list all files in data_temp
base_dir <- "C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Desktop/Development R code/1. PS method/data_temp/"
all_files <- paste0(base_dir, list.files(base_dir, recursive = TRUE))
data <-  map(all_files, readRDS)
data2 <- do.call(c, data)


# Write a function to verify the true value
AACE_true <-  -0.05754559
NACE_true <- -0.05727229
CACE_true <- -0.1145532


###################################
########  Define functions ########
# absolute bias function
Abs_bias_fun <- function(CACE_true, 
                         NACE_true, 
                         AACE_true,
                         input){
  bias_CACE <- input$orig_mean[1] - CACE_true
  bias_CACE.reg <- input$orig_mean[2] - CACE_true
  bias_NACE <- input$orig_mean[3] - NACE_true
  bias_NACE.reg <- input$orig_mean[4] - NACE_true
  bias_AACE <- input$orig_mean[5] - AACE_true
  bias_AACE.reg <- input$orig_mean[6] - AACE_true
  
  bias_res <- data.frame(bias_CACE,
                          bias_CACE.reg,
                          bias_NACE,
                          bias_NACE.reg,
                          bias_AACE,
                          bias_AACE.reg)
  return(list(bias_res))
}

# relative bias function
Rlt_bias_fun <- function(CACE_true, 
                         NACE_true, 
                         AACE_true,
                         input){
  bias_CACE <- (input$orig_mean[1] - CACE_true) / CACE_true
  bias_CACE.reg <- (input$orig_mean[2] - CACE_true) / CACE_true
  bias_NACE <- (input$orig_mean[3] - NACE_true) / NACE_true
  bias_NACE.reg <- (input$orig_mean[4] - NACE_true) / NACE_true
  bias_AACE <- (input$orig_mean[5] - AACE_true) / AACE_true
  bias_AACE.reg <- (input$orig_mean[6] - AACE_true) / AACE_true
  
  bias_res <- data.frame(bias_CACE,
                         bias_CACE.reg,
                         bias_NACE,
                         bias_NACE.reg,
                         bias_AACE,
                         bias_AACE.reg)
  return(list(bias_res))
}

Cov_perct_fun <- function(CACE_true,
                          NACE_true, 
                          AACE_true,
                          input){
        cov_CACE <- (input$boot_perct_l[1] <= CACE_true) * (CACE_true <= input$boot_perct_u[1])
        cov_CACE.reg  <- (input$boot_perct_l[2] <= CACE_true) * (CACE_true <= input$boot_perct_u[2])
        cov_NACE <-  (input$boot_perct_l[3] <= NACE_true) * (NACE_true <= input$boot_perct_u[3])
        cov_NACE.reg  <-  (input$boot_perct_l[4] <= NACE_true) * (NACE_true <= input$boot_perct_u[4])
        cov_AACE  <- (input$boot_perct_l[5] <= AACE_true) * (AACE_true <= input$boot_perct_u[5])
        cov_AACE.reg  <-  (input$boot_perct_l[6] <= AACE_true) * (AACE_true <= input$boot_perct_u[6])
        
        cov_res <- data.frame(cov_CACE,
                              cov_CACE.reg,
                              cov_NACE,
                              cov_NACE.reg,
                              cov_AACE,
                              cov_AACE.reg)
        return(cov_res)
}


Cov_bootsd_fun <- function(CACE_true,
                          NACE_true, 
                          AACE_true,
                          input){
  cov_CACE <- (input$boot_sd_l[1] <= CACE_true) * (CACE_true <= input$boot_sd_u[1])
  cov_CACE.reg  <- (input$boot_sd_l[2] <= CACE_true) * (CACE_true <= input$boot_sd_u[2])
  cov_NACE <-  (input$boot_sd_l[3] <= NACE_true) * (NACE_true <= input$boot_sd_u[3])
  cov_NACE.reg  <-  (input$boot_sd_l[4] <= NACE_true) * (NACE_true <= input$boot_sd_u[4])
  cov_AACE  <- (input$boot_sd_l[5] <= AACE_true) * (AACE_true <= input$boot_sd_u[5])
  cov_AACE.reg  <-  (input$boot_sd_l[6] <= AACE_true) * (AACE_true <= input$boot_sd_u[6])
  
  cov_res <- data.frame(cov_CACE,
                        cov_CACE.reg,
                        cov_NACE,
                        cov_NACE.reg,
                        cov_AACE,
                        cov_AACE.reg)
  return(cov_res)
}

#################################
########  Calculate Bias ########

# Calculate the absolute bias and make the boxplot 
{
  Abs_bias_res <- mapply(Abs_bias_fun, 
                         input = data2,
                         CACE_true, 
                         NACE_true, 
                         AACE_true)
  
  Abs_bias_res1 <- matrix(unlist(Abs_bias_res), ncol = 6 ,byrow = T)
  
  
  # check the boxplot for each bias 
  Abs_bias <- as.data.frame(Abs_bias_res1)
  colnames(Abs_bias) <- c("bias_CACE",
                          "bias_CACE.reg",
                          "bias_NACE",
                          "bias_NACE.reg",
                          "bias_AACE",
                          "bias_AACE.reg")
  # check the average bias value
  apply(Abs_bias, 2, mean)
  
  Abs_bias_box <- Abs_bias %>% 
    pivot_longer(
      cols = "bias_CACE":"bias_AACE.reg", 
      names_to = "Estimator",
      values_to = "absolute_bias"
    )
  Abs_bias_box$Estimator <- factor(Abs_bias_box$Estimator, 
                                   levels = c("bias_CACE",
                                              "bias_CACE.reg",
                                              "bias_NACE",
                                              "bias_NACE.reg",
                                              "bias_AACE",
                                              "bias_AACE.reg"))
  
  p1 <- ggplot(data = Abs_bias_box, aes(x = Estimator, y = absolute_bias)) +
    geom_boxplot()+
    labs(title="Absolute Bias for Each Estimator in Each Strata")
  p1
  
  ggsave("binary_simu_res/abs_bias.png", 
         plot=p1, 
         width=9, height=6.5, 
         dpi=500)
}


# Calculate the relative bias and make the boxplot 
{
  Rlt_bias_res <- mapply(Rlt_bias_fun, 
                         input = data2,
                         CACE_true, 
                         NACE_true, 
                         AACE_true)
  
  Rlt_bias_res1 <- matrix(unlist(Rlt_bias_res), ncol = 6 ,byrow = T)
  
  # check the boxplot for each bias 
  Rlt_bias <- as.data.frame(Rlt_bias_res1)
  colnames(Rlt_bias) <- c("bias_CACE",
                          "bias_CACE.reg",
                          "bias_NACE",
                          "bias_NACE.reg",
                          "bias_AACE",
                          "bias_AACE.reg")
  
  # check the average bias value
  apply(Rlt_bias_res1, 2, mean)
  
  Rlt_bias_box <- Rlt_bias %>% 
    pivot_longer(
      cols = "bias_CACE":"bias_AACE.reg", 
      names_to = "Estimator",
      values_to = "relative_bias"
    )
  
  Rlt_bias_box$Estimator <- factor(Rlt_bias_box$Estimator, 
                                   levels = c("bias_CACE",
                                              "bias_CACE.reg",
                                              "bias_NACE",
                                              "bias_NACE.reg",
                                              "bias_AACE",
                                              "bias_AACE.reg"))
  
  p2 <- ggplot(data = Rlt_bias_box, aes(x = Estimator, y = relative_bias)) +
    geom_boxplot()+
    labs(title="Relative Bias for Each Estimator in Each Strata")
  p2
  
  
  ggsave("binary_simu_res/rlt_bias.png", 
         plot=p2, 
         width=9, height=6, 
         dpi=500)
  
  
}


#####################################
########  Calculate Coverage ########
{
  # For the bootstrap percentile CI
  Cov_perct_res <- mapply(Cov_perct_fun, 
                         input = data2,
                         CACE_true, 
                         NACE_true, 
                         AACE_true)
  
  Cov_perct_res1 <- matrix(unlist(Cov_perct_res), ncol = 6 ,byrow = T)
  
  Cov_perct <- as.data.frame(Cov_perct_res1)
  colnames(Cov_perct) <- c("CACE",
                           "CACE.reg",
                           "NACE",
                           "NACE.reg",
                           "AACE",
                           "AACE.reg")
  
  # check the average bias value
  apply(Cov_perct, 2, mean)
  
  
  # For the bootstrap sd-based CI
  Cov_bootsd_res <- mapply(Cov_bootsd_fun, 
                          input = data2,
                          CACE_true, 
                          NACE_true, 
                          AACE_true)
  
  Cov_bootsd_res1 <- matrix(unlist(Cov_bootsd_res), ncol = 6 ,byrow = T)
  
  Cov_bootsd <- as.data.frame(Cov_bootsd_res1)
  colnames(Cov_bootsd) <- c("CACE",
                           "CACE.reg",
                           "NACE",
                           "NACE.reg",
                           "AACE",
                           "AACE.reg")
  
  # check the average bias value
  apply(Cov_bootsd, 2, mean)
  
}



