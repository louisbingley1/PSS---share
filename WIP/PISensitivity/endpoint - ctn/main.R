
################################################################################################
###    Simulate data from Sensitivity Analysis Paper 
###          Type of Endpoint: Continuous
################################################################################################
rm(list=ls())
setwd("C:/Users/liubing8/OneDrive - Merck Sharp & Dohme LLC/Documents/Github Ripos/PSS---share/WIP/PISensitivity/endpoint - ctn")
library(rjags)
library(dplyr)
library(R2jags)

################################## 
# Functions
##################################
{
  source('f_sim.r')                     # Function to simulate data
  source('f_delta_XXX.r')               # Function to calculate the delta|strata of interest
  source('f_mod.r')                     # Function to define model
  source('f_datjags.r')                 # Function to define dat.jags
  source('f_pm.r')                      # Function to define prior means
  source('f_inits.r')                   # Function to define initial values
  source('f_postparam_jags.r')
  source('f_postparam_jagsmodel.r') 
  source('f_ace_1sim.r')
}

##################################
# set parameters
##################################
{ 
  d0                      = 0.5
  d1                      = 0.8
  gamma                   = 0
  gamma_in                = gamma
  delta                   = d0                                ;delta
  eta                     = d1 + gamma - delta                ;eta 
  n                       = 200         
  mean                    = 0 
  sd                      = 1
  alpha0                  = -1.78
  alpha1                  = 2
  alpha2                  = 0 
  beta0                   = 0           
  beta1                   = 0.5  
  beta2                   = -0.5 
  nSim                    = 10           
  n.chains                = 3            
  n.adapt                 = 1000
  n.burnin                = 30
  n.iter                  = 100
  thin                    = 2
  parSave                 = c("S1", "Y1","Y0" ,"beta", "alpha")
  file                    = "mod.txt"

}

##################################
# ACE 
##################################

result_df  =  NULL

for (i in 1:nSim) { 
  
  dat_in                    = f_sim(n,mean,sd,alpha0, alpha1, alpha2, beta0, beta1, beta2, eta, delta, gamma)    
  trued_S1eq0               = f_delta_S1eq0(data=dat_in)
  trued_S1eq1               = f_delta_S1eq1(data=dat_in)
  pm_result                 = f_pm(dat=dat_in)
  dat.jags                  = f_datjags(dat=dat_in,gamma_in,pm_result)
  inits                     = f_inits(dat=dat_in,gamma_in) 
  postparam                 = f_postparam_jags(dat.jags,inits,parSave,text,n.chains,n.iter,n.burnin,thin)       
# postparam                 = f_postparam_jags.model(file,dat.jags,inits,n.chains,n.adapt,parSave,n.iter,thin)  
  ace                       = f_ace_1sim(postparam = postparam,dat=dat_in)   
  result_df                 = rbind.data.frame(result_df, data.frame( Sim         = i, 
                                                                      gamma_true  = gamma,
                                                                      gamma_in    = gamma_in,
                                                                      trued_S1eq0 = as.numeric(trued_S1eq0),
                                                                      trued_S1eq1 = as.numeric(trued_S1eq1),
                                                                      ace))     
}
result_df 


# Bayesian Estimated d  
mean(result_df$ITT_S1eq0)
mean(result_df$ITT_S1eq1)

# true d in simulated data   
mean(result_df$trued_S1eq0)
mean(result_df$trued_S1eq1)

# Theoretical delta  
# d0 = 0.5  ( Y1-Y0 | S1==0 )
# d1 = 0.8  ( Y1-Y0 | S1==1 )
 






 