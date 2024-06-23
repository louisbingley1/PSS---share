rm(list = ls())

library(nnet)
library(dplyr)
library(R2jags)  ;
library(dplyr)
library(adace)
library(parallel)
library(doParallel)
library(foreach)
library(cities)
#======================
#  Functions
#======================

#source("Data Simulator/adace simulator/f_sim.R")                                     # adace simulator: or source('Magnusson/endpoint - ctn/main/f_sim.r')                                   # Function to simulate data  
source("Data Simulator/cities simulator/f_sim.R")                                     # cities simulator
source("Comparison/utility functions/f_doparallel_1sim_3m.R")
source("Comparison/utility functions/f_comparison_table.R")

{
  source("Principal Score/main/f_augdata.R")
  source("Principal Score/main/f_EM_betas.R")
  source("Principal Score/main/f_pi.R")
  source("Principal Score/main/f_w.R")
  source("Principal Score/main/f_prob.R")
  source("Principal Score/main/f_coeff.R")
  source("Principal Score/main/f_PS_MBoot_1sim.R")
  source("Principal Score/main/f_PS.R")
  source("Principal Score/main/f_D.R")
  
} 
{
  source('Magnusson/endpoint - ctn/main/f_mod.R')                                   # Function to define model and write to file
  source('Magnusson/endpoint - ctn/main/f_I.R')                                     # Function to create matrix I
  source('Magnusson/endpoint - ctn/main/f_pm.R')                                    # Function to calculate prior means of delta using sample treatment effect estimates 
  source('Magnusson/endpoint - ctn/main/f_datjags.R')                               # Function to define dat.jags 
  source('Magnusson/endpoint - ctn/main/f_inits.R')                                 # Function to define all initial values  - starting values for MCMC
  source('Magnusson/endpoint - ctn/main/f_postparam_jags.R')                        # Function to compute postparam
  source('Magnusson/endpoint - ctn/main/f_postparam_jagsmodel.R')                   # Function to compute postparam (another way)
  source('Magnusson/endpoint - ctn/main/f_ace_1sim.R')                              # Function to compute ACE within 1 dataset
  source('Magnusson/endpoint - ctn/main/f_Uhat_Utrue.R')
  source('Magnusson/endpoint - ctn/main/f_rubins_rule.R')
  
} 

#=======================
#  Parameter Settings
#=======================


# [1.b] Parameters in cities simulator [nSim = 300]
{
  nSim  = 300                                                                    # number of simulated trials
  n_patient_ctrl = 200
  n_patient_expt = 200
  
  source("Data Simulator/cities simulator/scenarios/FL2C.R")             # cities simulator: large  trt effect,  scenario B
  
}

# [2] Parameters in Bayeisan/Magnusson
{ 
  parSave        = c("delta","S0","S1","Y0","Y1","w")                                # argument of jags() 
  n.chains       = 1                                                                 # argument of jags()
  n.burnin       = 500                                                               # argument of jags()
  n.iter         = 1500                                                              # argument of jags()
  thin           = 2                                                                 # argument of jags()
  file           = "mod.txt"                                                         # argument of jags()
  #n.adapt       = 1000                                                             # argument of jags.model()
}  

# [3] Parameters in Principal Score [M = 200 ] 
{
  #n_ps                 = 5000                                                           # NA NOW: update n with a larger n -- for bootstrap [adace simulator]
  n_patient_vector_ps  =  n_patient_vector                                               # NA NOW: update n with a larger n -- for bootstrap [cities simulator]     
  seed_M_0             = 2020
  M                    = 200                                                                  
  seed_M_v             = seq(seed_M_0, seed_M_0+M-1,1)
  ep1                  = 1;
  ep0                  = 1;  
  Trace                =  T;
  iter.max             = 200;
  error0               = 10^-6
  UtoRemove            = 'NeverAdhere/11/D'
}


#===============================
#  Do Parallel
#===============================

timer_start <- proc.time()

n_core      <- detectCores()                                                 
cl          <- makeCluster(n_core-1)
registerDoParallel(cl)
data_out    <- foreach (i         = 1:nSim,
                        .export   = c("rep_row","rep_col","pacf_vec_to_acf", "p_ae_poisson","p_loe_ee_function","pivot_longer","jags" ), 
                        .packages = c("dplyr","cities","rjags","R2jags","adace","nnet"),
                        .combine  = 'rbind') %dopar% { f_1sim(i,seed_vec, n_patient_vector, p_loe_max, z_l_loe,  z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list,  sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae,  prob_ae,  reference_id, plot_po, up_good,  threshold, delta_adjustment_in, covariate_df) }
stopCluster(cl)

timer_stop  <- proc.time() 
dur         <- timer_stop - timer_start


#===============================
# Append
#===============================

result_df_PS = result_df_BS = result_df_AD= NULL
for(i in 1:nSim){
  result_df_PS = rbind.data.frame(result_df_PS, data_out[i,]$result_df_PS)
  result_df_BS = rbind.data.frame(result_df_BS, data_out[i,]$result_df_BS)
  result_df_AD = rbind.data.frame(result_df_AD, data_out[i,]$result_df_AD)
}
result_df_PS; result_df_BS; result_df_AD

#===============================
# outputs
#===============================
# nsim=300, M=200, iter=1500, burnin=500, nchain=1, thin=2, ScenB
# s1: trt large ScenB
# s2: FL1C
# s3: trt modest ScenB
# s4: FL2C

tb_1B = f_comparison_table(AD=result_df_AD,BS=result_df_BS,PS=result_df_PS)
# write out
write.csv(result_df_PS,"Comparison/JSM2024/outputs/s4_PS.csv")
write.csv(result_df_BS,"Comparison/JSM2024/outputs/s4_BS.csv")
write.csv(result_df_AD,"Comparison/JSM2024/outputs/s4_AD.csv")
write.csv(tb_1B,"Comparison/JSM2024/outputs/s4_tb.csv")
# write in
s1_PS = read.csv("Comparison/JSM2024/outputs/s4_PS.csv",header=T)
s1_BS = read.csv("Comparison/JSM2024/outputs/s4_BS.csv",header=T)
s1_AD = read.csv("Comparison/JSM2024/outputs/s4_AD.csv",header=T)
s1_tb = read.csv("Comparison/JSM2024/outputs/s4_tb.csv",header=T)

