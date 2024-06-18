rm(list=ls())
library(cities)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(ggthemes)
library(adace)
library(tidyverse)
library(lubridate)
library(doParallel)

#====================================
#  Functions & Parameter Settings
#====================================
source("Simulation Study/original/splpl_utilities.R")
comb <- function(x, ...) {  lapply(seq_along(x), function(i) c(x[[i]], lapply(list(...), function(y) y[[i]]))) }


total_data        <- 20
nsim  = nSim      <- total_data
n_core            <- 10
nsim_1core        <- nsim/n_core
starting_seed_val <- seq(1, nsim-nsim_1core+1, nsim_1core)  # 0:(n_core-1)*nsim_1core + 1
source("Data Simulator/cities simulator/scenarios/scen_vali.R")


#====================================
# data simulation
#====================================
{
cl                <- makeCluster(n_core)
registerDoParallel(cl)

data_out <- foreach ( k        = 1:n_core,
                     .packages = c('tidyverse','cities'), 
                     .combine  ='c', 
                     .multicombine=TRUE) %dopar% { data_core <- data_generator_loop1(n_patient_vector,
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
                                                                                      total_data = nsim_1core,
                                                                                      delta_adjustment_in,
                                                                                      covariate_df)
  list(data_core)
}

stopCluster(cl)
}

# check - data_out ---------------------------------------------------
names(data_out[[1]]) 
# "estimand_mean" :  output the mean matrix of PP, S++, S*+, IR, Full.                   ---- data_out[[1]]$estimand_mean
# "estimand_sd"   :  output the sd   matrix of PP, S++, S*+, IR, Full.                   ---- data_out[[1]]$estimand_sd
# "dc_mean_list"  :  output the dc rate matrix for different dc reasons.                 ---- data_out[[1]]$dc_mean_list
# "observed_df"   :  output the observed data frame, nrow <= nsubj * timepoints * nseeds ---- data_out[[1]]$observed_df
# "po_df"         :  output the po data frame, nrow = nsubj*timepoints*nseeds *2         ---- data_out[[1]]$po_df
# "ir_df"         :  output the ____, nrow= nsubj*timepoints                             ---- data_out[[1]]$ir_df
#---------------------------------------------------------------------


# check observed_df, po_df ------------------------------------------ 
head(data_out[[1]]$observed_df);tail(data_out[[1]]$observed_df)
head(data_out[[2]]$observed_df);tail(data_out[[2]]$observed_df)
head(data_out[[3]]$observed_df);tail(data_out[[3]]$observed_df)
head(data_out[[4]]$observed_df);tail(data_out[[4]]$observed_df)
head(data_out[[10]]$observed_df);tail(data_out[[10]]$observed_df)
#head(data_out[[11]]$observed_df);tail(data_out[[11]]$observed_df)

head(data_out[[1]]$po_df); tail(data_out[[10]]$po_df)
#---------------------------------------------------------------------
 

# combine all cores' outputs
observed_df = po_df <- data.frame()
for (i in 1:n_core) {
                        observed_df <- rbind(observed_df, data_out[[i]]$observed_df)
                        po_df       <- rbind(po_df, data_out[[i]]$po_df)
}

# rename to _mar or _mnar
po_mar        <-  po_df %>%  dplyr::select(seed, subject, arm, timepoints, aval) %>%  rename(aval_mar = aval)    # nrow = 10*4*20*2
observed_mar  <-  observed_df %>% left_join(po_mar, by=c("seed", "subject", "arm", "timepoints")) %>% rename(aval_mnar = aval) # nrow <= 10*4*20



#----------------------
# ADACE
#----------------------

# Bing ----------------------------------------------------------------------------------------
observed_out <- observed_mar %>%
  ungroup() %>%
  tidyr::complete(seed, subject, timepoints) %>%
  ungroup() %>%
  group_by(seed, subject, arm) %>%
  dplyr::select(-aval_mnar, -aval_mar) %>%
  zoo::na.locf() %>%
  left_join(observed_mar %>%
              dplyr::select(-dc_loe, -dc_admin, -dc_ee, -dc_ae, -continuous, -binary, -base), 
            by=c("seed", "subject", "arm", "timepoints"))  
#----------------------------------------------------------------------------------------------

 
data              <- observed_out # observed_mar
data_po           <- po_df
jstart            <- starting_seed_val              
A_pp              <- list()
A_sp              <- list()
true_pp = true_sp <- c()

{
cl                <- makeCluster(n_core)
registerDoParallel(cl)
sims              <- foreach (j            = 1:nsim,
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
                                Z[[1]] <-data_wide$base                                                     # Z[[1]] <- matrix(NA, ncol = 1,nrow=n_total)                             
                                A      <- matrix(NA, nrow = n_total, ncol = 4)
                                A[,1]  <- ifelse(is.na(data_wide %>% pull(times[1])), 0, 1)   
                                
                                
                                for (i in 1:3) {
                                  Z[[i+1]] <- data_wide %>% pull(times[i])  # matrix(data_wide %>% pull(times[i]), ncol = 1)
                                  A[,i+1]  <- ifelse(is.na(data_wide %>% pull(times[i+1])), 0, 1)
                                }
                                
                                # Bing---------------------------------------------------------------------------------------
                                data_      = data %>% mutate(DC = ifelse( dc_loe+dc_ee+dc_admin+dc_ae > 0 ,1 ,0), A=1-DC ) ;  
                                data_wide_ = data_ %>%   
                                  filter(seed ==j) %>%
                                  select(timepoints,A) %>%                    
                                  pivot_wider(names_from = timepoints, values_from = A)  ;head(data_wide_)
                                for(i in 1:4){A[,i] = data_wide_ %>% pull(times[i])}
                                #--------------------------------------------------------------------------------------------
                                
                                
                                X   <- as.matrix(data.frame(data_wide$continuous,data_wide$binary))                      # X   <- as.matrix(data.frame(data_wide$base,data_wide$continuous,data_wide$binary))   
                                #   X   <- data_wide %>% ungroup %>% select(base,continuous, binary) %>% as.matrix()
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
                                
                                list2= list(A=A,Y=Y,Z=Z,X=X,TRT=TRT)
                                
                                list(A_pp[[j]], A_sp[[j]], true_pp[j], true_sp[j])
                                
                              }

stopCluster(cl)

}

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

