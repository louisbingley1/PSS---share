rm(list=ls())
setwd("C:/Users/liubing8/OneDrive - Merck Sharp & Dohme LLC/Documents/Github Ripos/PSS---share/Simulation Study")
library(cities)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(ggthemes)
#source('original/splpl_utilities.R')
#source('original/splpl_utilities_BL.R')
source('splpl_utilities_BL_1.R')
source('f_settings.r')
 

data_out = data_generator_loop1(n_patient_vector,
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
                                seed_val<-starting_seed_val,
                                reference_id, 
                                plot_po = FALSE,
                                up_good,
                                threshold,
                                total_data,
                                delta_adjustment_in,
                                covariate_df
);data_out


# Full potential outcomes data
po_mar       <- data_out$po_df %>%
                  dplyr::select(seed, subject, arm, timepoints, aval) %>%
                  rename(aval_mar = aval)

# Full potential outcomes data
observed_mar <-  data_out$observed_df %>%
                    left_join(po_mar, by=c("seed", "subject", "arm", "timepoints")) %>%
                    rename(aval_mnar = aval)

observed_out <- observed_mar %>%
                    ungroup() %>%
                    tidyr::complete(seed, subject, timepoints) %>%
                    ungroup() %>%
                    group_by(seed, subject, arm) %>%
                    dplyr::select(-aval_mnar, -aval_mar) %>%
                    zoo::na.locf() %>%
                    left_join(observed_mar   %>% dplyr::select(-dc_loe, -dc_admin, -dc_ee, -dc_ae, -continuous, -binary, -base),             by=c("seed", "subject", "arm", "timepoints")) %>%
                    left_join(data_out$po_df %>% dplyr::select(-dc_loe, -dc_admin, -dc_ee, -dc_ae, -continuous, -binary, -base, -observed),  by=c("seed", "subject", "arm", "timepoints")) 

# a.	observed_out$aval_mar is the available outcomes with MAR missingness
observed_out$aval_mar

# b.	observed_out$aval_mnar is the available outcomes with MNAR missingness
observed_out$aval_mnar

# c.	observed_out$aval is the potential outcome
observed_out$aval
