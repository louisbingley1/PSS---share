#rm(list=ls())
library(cities)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(ggthemes)
library(adace)
library(doParallel)

source("Simulation Study/original/splpl_utilities.R")
source("Data Simulator/cities simulator/scenarios/scen_vali.R")


total_data = 20
n_core = 1
starting_seed_val <- 1                 #bing
total_data_cores <- total_data/n_core

cl <- makeCluster(6)
registerDoParallel(cl)

data_out <- foreach (k=1:n_core,.packages = c('tidyverse','cities'), .combine='c', .multicombine=TRUE) %dopar% {
  data_core <- data_generator_loop1(n_patient_vector,
                                    p_loe_max,
                                    z_l_loe,
                                    z_u_loe,
                                    p_ee_max,
                                    z_l_ee,
                                    z_u_ee,
                                    timepoints,
                                    pacf_list,
                                    sigma_ar_vec,
                                    mean_list,
                                    beta_list,
                                    p_admin,
                                    rate_dc_ae,
                                    prob_ae,
                                    seed_val = starting_seed_val[k],
                                    reference_id,
                                    plot_po = FALSE,
                                    up_good,
                                    threshold,
                                    total_data = total_data_cores,
                                    delta_adjustment_in,
                                    covariate_df)
  list(data_core)
}

stopCluster(cl)

observed_df = po_df <- data.frame()
for (i in 1:n_core) {
  observed_df <- rbind(observed_df, data_out[[i]]$observed_df)
  po_df <- rbind(po_df, data_out[[i]]$po_df)
}

#Full potential outcomes data
po_mar <- po_df %>%
  dplyr::select(seed, subject, arm, timepoints, aval) %>%
  rename(aval_mar = aval)

# Full potential outcomes data
observed_mar <-  observed_df %>%
  left_join(po_mar, by=c("seed", "subject", "arm", "timepoints")) %>%
  rename(aval_mnar = aval)

# observed_out <- observed_mar %>%
#   ungroup() %>%
#   tidyr::complete(seed, subject, timepoints) %>%
#   ungroup() %>%
#   group_by(seed, subject, arm) %>%
#   dplyr::select(-aval_mnar, -aval_mar) %>%
#   zoo::na.locf() %>%
#   left_join(observed_mar %>%
#               dplyr::select(-dc_loe, -dc_admin, -dc_ee, -dc_ae, -continuous, -binary, -base),
#             by=c("seed", "subject", "arm", "timepoints")) %>%
#   left_join(data_out$po_df%>%
#               dplyr::select(-dc_loe, -dc_admin, -dc_ee, -dc_ae, -continuous, -binary, -base, -observed),
#             by=c("seed", "subject", "arm", "timepoints"))

data <- observed_mar
data_po <- po_df

A_pp <- list()
A_sp <- list()
true_pp = true_sp <- c()

cl <- makeCluster(10)
registerDoParallel(cl)

comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}


sims <- foreach (j             = 1:total_data,
                 .packages     = c('tidyverse','adace'), 
                 .combine      = 'comb', 
                 .multicombine = TRUE,
                 .init         = list(list(), list(), c(), c())) %dopar% {
  #print(j)
  ### change to adace format ###
  data_wide <- data %>%  
                dplyr::select(base, continuous, binary, subject, arm, timepoints, seed, aval_mar) %>% 
                filter(seed ==j) %>%
                pivot_wider(names_from = timepoints, values_from = aval_mar)


  times  <- c("12","24","48","55")
  Z      <- list()
  Z[[1]] <- matrix(NA, ncol = 1,nrow=1000)
  A      <-  matrix(NA, nrow = 1000, ncol = 4)
  A[,1]  <- ifelse(is.na(data_wide %>% pull(times[1])), 0, 1)


  for (i in 1:3) {
    Z[[i+1]] <- matrix(data_wide %>% pull(times[i]), ncol = 1)
    A[,i+1] <- ifelse(is.na(data_wide %>% pull(times[i+1])), 0, 1)
  }

  X   <- as.matrix(data.frame(data_wide$base,data_wide$continuous,data_wide$binary))
  Y   <- data_wide %>% pull(10)
  TRT <- data_wide %>% pull(arm)
  TRT <- ifelse(TRT == 1, 0, 1)

  A_pp[[j]] <- est_S_Plus_Plus_MethodA(X, A, Z, Y, TRT)
  A_sp[[j]] <- est_S_Star_Plus_MethodA(X, A, Z, Y, TRT)

  ## get all discontinuation and potential outcome data
  data_all <- data_po %>% dplyr::filter(seed == j & timepoints == 55)
  data_all$A <- ifelse(rowSums(data_all[,10:13])==0,1,0)

  # split data to different treatment arms
  data_all_1 <- data_all %>% filter(arm == 1)
  data_all_2 <- data_all %>% filter(arm == 2)

  true_pp[j] <- mean(data_all_2$aval[data_all_2$A==1 & data_all_1$A==1]) - mean(data_all_1$aval[data_all_2$A==1 &data_all_1$A==1])
  true_sp[j] <- mean(data_all_2$aval[data_all_2$A==1])                   - mean(data_all_1$aval[data_all_2$A==1])

  list(A_pp[[j]], A_sp[[j]], true_pp[j], true_sp[j])

}

stopCluster(cl)

A_pp <- sims[[1]]
A_sp <- sims[[2]]
true_pp <- unlist(sims[[3]])
true_sp <- unlist(sims[[4]])


App_tab <- c()
Asp_tab <- c()
for (i in 1:total_data) {
  App_res <- c()
  Asp_res <- c()
  for (j in 1:6) {
    App_res <- c(App_res,unlist(A_pp[[i]][j]))
    Asp_res <- c(Asp_res,unlist(A_sp[[i]][j]))
  }
  App_tab <- rbind(App_tab, App_res)
  Asp_tab <- rbind(Asp_tab, Asp_res)
}

colMeans(App_tab)
colMeans(Asp_tab)

mean(true_pp)
mean(true_sp)


