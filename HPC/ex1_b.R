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
#  library(dplyr)
#  library(R2jags)  
#  library(adace)
  library(parallel)
  library(doParallel)
  library(foreach)
#  library(cities)
#  library(tidyverse)


# Functions
source("HPC/f/hpc_f_sim.R")                                     # cities simulator
source("HPC/f/hpc_f_2m.R")                                     # cities simulator
  
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

# Parameter Settings
# [1.b] Parameters in cities simulator  
# [2  ] Parameters in Bayeisan/Magnusson  [50,150]
{ 
  parSave        = c("delta","S0","S1","Y0","Y1","w")                                # argument of jags() 
  n.chains       = 1                                                                 # argument of jags()
  n.burnin       = 50                                                               # argument of jags()
  n.iter         = 150                                                              # argument of jags()
  thin           = 2                                                                 # argument of jags()
  file           = "mod.txt"                                                         # argument of jags()
  #n.adapt       = 1000                                                             # argument of jags.model()
  }
# [3  ] Parameters in Principal Score [M = 3 ] 
{
  #n_ps                 = 5000                                                           # NA NOW: update n with a larger n -- for bootstrap [adace simulator]
  n_patient_vector_ps  =  n_patient_vector                                               # NA NOW: update n with a larger n -- for bootstrap [cities simulator]     
  seed_M_0             = 2020
  M                    = 3                                                                 
  seed_M_v             = seq(seed_M_0, seed_M_0+M-1,1)
  ep1                  = 1;
  ep0                  = 1;  
  Trace                =  T;
  iter.max             = 200;
  error0               = 10^-6
  UtoRemove            = 'NeverAdhere/11/D'
} 

}

#=============================
# Send Job
#=============================
load(file = "HPC/sim_list/sim_list.Rdata") # loads result

set.seed(b)
result <- hpc_f_2m(b,sim_list,seed_vec, n_patient_vector, p_loe_max, z_l_loe,  z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list,  sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae,  prob_ae,  reference_id, plot_po, up_good,  threshold, delta_adjustment_in, covariate_df) 
save(result, file = paste0("HPC/Output/ex1_res_", b, ".rdata"))

# load(file =  "HPC/Output/ex1_res_1.rdata") # loads result

# my 'result' is a list of 3 rows.