rm(list=ls())
library(cities)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(ggthemes)
source("Simulation Study/splpl_utilities.R")
source("Data Simulator/cities simulator/setting_scen_D.r")   # LoE & EE & Admin & AE settings

#-------------------
# parameter setting 
#-------------------

{
total_data          = 10
starting_seed_val   = 1
seed_val            = starting_seed_val
reference_id        = 1
threshold           = NA
timepoints          = c(0,12,24,48,55)
IR_display          = TRUE
delta_adjustment_in = NA
n_patient_ctrl      = 200
n_patient_expt      = 200
n_patient_vector    = c(n_patient_ctrl, n_patient_expt)
n_total             = sum(n_patient_vector)
mean_control        = c(0,0,0,0,0)
mean_treatment      = c(0,0.2,0.4,0.6,0.8)
mean_list           = list(mean_control, mean_treatment)
sigma_ar_vec        = c(3, 3)
pacf_list           = list(c(0.5),   c(0.5))
beta_list           = list(c(1.25, 1.25), c(1.25, 1.25))
covariate_df        = NA
static_output       = TRUE
plot_po             = FALSE
maxtime             = length(timepoints)-1
}

#-------------------
# 1 sim
#-------------------

data_out      = data_generator1(n_patient_vector, p_loe_max, z_l_loe,  z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list,  sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae,  prob_ae, seed_val, reference_id, plot_po, up_good,  threshold, delta_adjustment_in, covariate_df)
po_mar        = data_out$po_df %>% dplyr::select(seed, subject, arm, timepoints, aval) %>% rename(aval_mar = aval)
observed_mar  = data_out$observed_df %>% left_join(po_mar, by=c("seed", "subject", "arm", "timepoints")) %>% rename(aval_mnar = aval)
# rearrange observed_df by usubjid;
observed_out  = observed_mar %>%
                    ungroup() %>%
                    tidyr::complete(seed, subject, timepoints) %>%
                    ungroup() %>%
                    group_by(seed, subject, arm) %>%
                    dplyr::select(-aval_mnar, -aval_mar) %>%
                    zoo::na.locf() %>%
                    left_join(observed_mar %>%
                                dplyr::select(-dc_loe, -dc_admin, -dc_ee, -dc_ae, -continuous, -binary, -base), 
                              by=c("seed", "subject", "arm", "timepoints")) %>%
                    left_join(data_out$po_df%>%
                                dplyr::select(-dc_loe, -dc_admin, -dc_ee, -dc_ae, -continuous, -binary, -base, -observed), 
                     by=c("seed", "subject", "arm", "timepoints")) 

#-------------------
# 1sim true ace & dc
#-------------------
data_out$estimand_mean[[1]][,maxtime]
plusplus_true = mean(as.data.frame(data_out$estimand_mean[[2]])[,maxtime] )
starplus_true = mean(as.data.frame(data_out$estimand_mean[[3]])[,maxtime] )

#-------------------
# nsim true ace & dc
#-------------------

nsim_true_list      = f_nsim_true(total_data )
nsim_true_ace_table = nsim_true_list$true_ace_table
nsim_dc_out         = nsim_true_list$dc_out