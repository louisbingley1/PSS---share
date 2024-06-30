args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("No arguments supplied")
} else {
  for (i in 1:length(args)) {
    eval(parse(text = args[[i]]))
  }
}

#=============================
# Load Functions & Parameters
#=============================
{ #update.packages(ask = FALSE, checkBuilt = TRUE)
  library(nnet)
  library(dplyr)
 # library(R2jags)  ;
#  library(adace)
  library(parallel)
  library(doParallel)
  library(foreach)
  library(cities)
  library(tidyverse)


# Functions
{source("./Functions/Data Simulator/cities simulator/f_sim.R")                                     # cities simulator
 source("./Functions/Comparison/utility functions/f_doparallel_1sim_2m.R")
  source("./Functions/Comparison/utility functions/f_comparison_table.R")}
{
  source("./Functions/Principal Score/main/f_augdata.R")
  source("./Functions/Principal Score/main/f_EM_betas.R")
  source("./Functions/Principal Score/main/f_pi.R")
  source("./Functions/Principal Score/main/f_w.R")
  source("./Functions/Principal Score/main/f_prob.R")
  source("./Functions/Principal Score/main/f_coeff.R")
  source("./Functions/Principal Score/main/f_PS_MBoot_1sim.R")
  source("./Functions/Principal Score/main/f_PS.R")
  source("./Functions/Principal Score/main/f_D.R")
  
} 
{
  source('./Functions/Magnusson/endpoint - ctn/main/f_mod.R')                                   # Function to define model and write to file
  source('./Functions/Magnusson/endpoint - ctn/main/f_I.R')                                     # Function to create matrix I
  source('./Functions/Magnusson/endpoint - ctn/main/f_pm.R')                                    # Function to calculate prior means of delta using sample treatment effect estimates 
  source('./Functions/Magnusson/endpoint - ctn/main/f_datjags.R')                               # Function to define dat.jags 
  source('./Functions/Magnusson/endpoint - ctn/main/f_inits.R')                                 # Function to define all initial values  - starting values for MCMC
  source('./Functions/Magnusson/endpoint - ctn/main/f_postparam_jags.R')                        # Function to compute postparam
  source('./Functions/Magnusson/endpoint - ctn/main/f_postparam_jagsmodel.R')                   # Function to compute postparam (another way)
  source('./Functions/Magnusson/endpoint - ctn/main/f_ace_1sim.R')                              # Function to compute ACE within 1 dataset
  source('./Functions/Magnusson/endpoint - ctn/main/f_Uhat_Utrue.R')
  source('./Functions/Magnusson/endpoint - ctn/main/f_rubins_rule.R')
  
} 

# Parameter Settings
{
  nSim  = 1                                                                          # number of simulated trials
  n_patient_ctrl = 200
  n_patient_expt = 200
  
  source("./Functions/Data Simulator/cities simulator/scenarios/trt_large_scen_B.R")             # cities simulator: large  trt effect,  scenario B
  
} # [1.b] Parameters in cities simulator [nSim = 1]
{ 
  parSave        = c("delta","S0","S1","Y0","Y1","w")                                # argument of jags() 
  n.chains       = 1                                                                 # argument of jags()
  n.burnin       = 500                                                               # argument of jags()
  n.iter         = 1500                                                              # argument of jags()
  thin           = 2                                                                 # argument of jags()
  file           = "mod.txt"                                                         # argument of jags()
  #n.adapt       = 1000                                                             # argument of jags.model()
} # [2  ] Parameters in Bayeisan/Magnusson
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
} # [3  ] Parameters in Principal Score [M = 200 ] 

}

#=============================
# Send Job
#=============================
set.seed(b)
result <- f_1sim(b,seed_vec, n_patient_vector, p_loe_max, z_l_loe,  z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list,  sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae,  prob_ae,  reference_id, plot_po, up_good,  threshold, delta_adjustment_in, covariate_df) 
save(result, file = paste0("./Output/ex1_res_", b, ".rdata"))

# my 'result' is a list of 3 rows.