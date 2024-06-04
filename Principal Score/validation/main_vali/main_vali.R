rm(list=ls())

library(nnet)
library(dplyr)

#======================
#  Functions
#======================
{
source("Principal Score/validation/main_vali/f_sim_vali_Bing.r")                                      # adace simulator: or source('Magnusson/endpoint - ctn/main/f_sim.r')                               
#source("Data Simulator/cities simulator/f_sim.r")                                    # cities simulatorsource("Principal Score/main/f_augdata.r")
source("Principal Score/validation/main_vali/f_EM_betas_vali.r")
source("Principal Score/validation/main_vali/f_pi.r")
source("Principal Score/validation/main_vali/f_w.r")
source("Principal Score/validation/main_vali/f_prob_vali.r")
source("Principal Score/validation/main_vali/f_coeff_vali.r")
source("Principal Score/validation/main_vali/f_PS_vali.r")
source("Principal Score/validation/main_vali/f_PS_MBoot_1sim_vali.r")
#source("Principal Score/main/f_D.r")
source("Principal Score/validation/main_vali/f_augdata_vali.r")
}

#=======================
#  Parameter Settings
#=======================

# [1.a] Parameters in adace simulator
# [1.b] Parameters in cities simulator
# [1.c] Parameters in original simulator
{
nSim =3
seed_0 = 2020
seed_v = seq(seed_0, seed_0+nSim-1, 1)
n = 500                                        # sample size of simulated data
theta_seq = c(-1, -0.5, 0, 0.5, 1)             # the sequence of all possible theta for theta_logit
index_theta = 1                                # range from 1 to 5, index for which theta from theta_seq will be added to the end of theta_logit
theta_logit_ss = c(0.25, 0.5, 0.5, 1, 1)       # the first 5 coefficients for logit model for ss strata - always-taker
theta_logit_s1s1 = c(-0.25, 1, 1, 0.5, 0.5)    # the first 5 coefficients for logit model for s1s1 strata - never-taker
theta_logit_ss1 = rep(0, 6)                    # the first 5 coefficients for logit model for ss1 strata - complier
}

# [2] Parameters in Principal Score
{
n_ps                 = 5000                                                            # update n with a larger n -- for bootstrap [adace simulator]
#n_patient_vector_ps  = 3*n_patient_vector                                             # update n with a larger n -- for bootstrap [cities simulator]     
seed_M_0             = 2020
M                    = 5
seed_M_v             = seq(seed_M_0, seed_M_0+M-1,1)
ep1                  = 1;
ep0                  = 1;  
Trace                =  T;
iter.max             = 200;
error0               = 10^-6
UtoRemove            = 'NeverAdhere/11/D'
}

 
#===============================
#  ACE from nSim Simulations
#===============================

result_df           = NULL #cbind.data.frame(sim=1:nsim,AACE=rep(NA,nsim))

for(i in 1:nSim){
  
  # DATA SIMULATION -- Re-Create data : Simulate a larger data pool (sample size: n_ps or n_patient_vector_ps) and bootstrap. 
  
  # adace simulator: # i=1
  # cities simulator : simulate 1 dataset and prepare for variables
  # original simulator
    sim          = f_sim_vali_Bing(n, random_seed <-seed_v[i],  theta_seq ,  index_theta , theta_logit_ss ,   theta_logit_s1s1 ,  theta_logit_ss1 )
    ps_in        = sim$data  %>%  mutate(indexZD=paste0(Z,D))                   # full %>% filter(U!=UtoRemove) %>%   rename( Z=TRT) %>%  mutate(indexZD=paste0(Z,D))
  
    
  # TRUE 
  
  trued_A      = sim$true_d_Tm$true_d_Tm_adhact
  trued_C      = sim$true_d_Tm$true_d_Tm_adhboth
  trued_N      = sim$true_d_Tm$true_d_Tm_adhpbo
#  trued_AC     = sim$true_d_Tm$true_d_Tm_sp
  trued_A_nsl  = sim$true_d_Tm_nsl$true_d_Tm_adhact_nsl
  trued_C_nsl  = sim$true_d_Tm_nsl$true_d_Tm_adhboth_nsl
  trued_N_nsl  = sim$true_d_Tm_nsl$true_d_Tm_adhpbo_nsl
#  trued_AC_nsl = sim$true_d_Tm_nsl$true_d_Tm_sp_nsl
  
  # ACE 
  
  ace          = f_PS_MBoot_1sim_vali(data_boot <-ps_in, ep0,ep1,M,iter.max,error0,seed_M_v)
  
  # STACK
  
  result_df    =  rbind.data.frame(result_df, 
                                    cbind.data.frame(Sim = i, 
                                                     trued_N_01      = trued_N,
                                                     trued_N_01_nsl  = trued_N_nsl,
                                                     trued_C_00      = trued_C,
                                                     trued_C_00_nsl  = trued_C_nsl,
                                                     trued_A_10      = trued_A,
                                                     trued_A_10_nsl  = trued_A_nsl,
 #                                                    trued_AC_sp     = trued_AC,
#                                                     trued_AC_sp_nsl = trued_AC_nsl,
                                                     ace)  
  )
  
}


result_df 

# ACE  
mean(result_df$AACE); mean(result_df$AACE.adj)
mean(result_df$NACE); mean(result_df$NACE.adj)
mean(result_df$CACE); mean(result_df$CACE.adj)
mean(result_df$CAACE);mean(result_df$CAACE.adj)

# causal/trt effect in simulated data (at last AVISITN)
mean(result_df$trued_A_10); mean(result_df$trued_A_10_nsl)
mean(result_df$trued_N_01); mean(result_df$trued_N_01_nsl)
mean(result_df$trued_C_00); mean(result_df$trued_C_00_nsl) 
mean(result_df$trued_AC_sp);mean(result_df$trued_AC_sp_nsl)

# true/theoretical value of causal/trt effect (defined in f_setting)
# TrtEff_adhact   = 1.5                               # A
# TrtEff_adhpbo   = 0.5                               # N
# TrtEff_adhboth  = 2                                 # C

# PCP - Prob of correct prediction
 
 

# source("Data Simulator/cities simulator/utility functions/Asymptotic Estimate of Estimand.R")

 