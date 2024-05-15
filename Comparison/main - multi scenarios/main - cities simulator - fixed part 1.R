rm(list = ls())

library(nnet)
library(dplyr)
library(R2jags)  ;
library(dplyr)
library(adace)

#======================
#  Functions
#======================

#source("Data Simulator/adace simulator/f_sim.r")                                     # adace simulator: or source('Magnusson/endpoint - ctn/main/f_sim.r')                                   # Function to simulate data  
source("Data Simulator/cities simulator/f_sim.r")                                     # cities simulator
{
  source("Principal Score/main/f_augdata.r")
  source("Principal Score/main/f_EM_betas.r")
  source("Principal Score/main/f_pi.r")
  source("Principal Score/main/f_w.r")
  source("Principal Score/main/f_prob.r")
  source("Principal Score/main/f_coeff.r")
  source("Principal Score/main/f_PS_MBoot_1sim.r")
  source("Principal Score/main/f_PS.r")
  source("Principal Score/main/f_D.r")
  
} 
{
  source('Magnusson/endpoint - ctn/main/f_mod.r')                                   # Function to define model and write to file
  source('Magnusson/endpoint - ctn/main/f_I.r')                                     # Function to create matrix I
  source('Magnusson/endpoint - ctn/main/f_pm.r')                                    # Function to calculate prior means of delta using sample treatment effect estimates 
  source('Magnusson/endpoint - ctn/main/f_datjags.r')                               # Function to define dat.jags 
  source('Magnusson/endpoint - ctn/main/f_inits.r')                                 # Function to define all initial values  - starting values for MCMC
  source('Magnusson/endpoint - ctn/main/f_postparam_jags.r')                        # Function to compute postparam
  source('Magnusson/endpoint - ctn/main/f_postparam_jagsmodel.r')                   # Function to compute postparam (another way)
  source('Magnusson/endpoint - ctn/main/f_ace_1sim.r')                              # Function to compute ACE within 1 dataset
  source('Magnusson/endpoint - ctn/main/f_Uhat_Utrue.r')
  source('Magnusson/endpoint - ctn/main/f_rubins_rule.r')
  
}

#=======================
#  Parameter Settings
#=======================

# [1.a] Parameters in adace simulator
{
  #  nSim  = 10                                                                        # number of simulated trials
  #  n     = 1000                                                                      # sample size per trial
  #  source("Data Simulator/adace simulator/setting_adace.r")                          # adace simulator
}

# [1.b] Parameters in cities simulator
{
  nSim  = 20                                                                        # number of simulated trials
  n_patient_ctrl = 200
  n_patient_expt = 200
}