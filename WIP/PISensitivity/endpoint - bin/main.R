################################################################################################
###    Simulate data from Sensitivity Analysis Paper 
###          Type of Endpoint: Binary
################################################################################################
rm(list=ls())
library(survival)
library(mvtnorm)
library(rjags)
library(dplyr)
library(R2jags)

################################## 
# Functions
##################################
{
source('WIP/PISensitivity/endpoint - bin/f_sim.r')                    # Function to simulate data
source('WIP/PISensitivity/endpoint - bin/f_OR_XXX.r')                 # Function to calculate the sample causal effect from simulated data
source('WIP/PISensitivity/endpoint - bin/f_mod.r')                    # Function to define model and write to file
source('WIP/PISensitivity/endpoint - bin/f_datjags.r')                # Function to define dat.jags
source('WIP/PISensitivity/endpoint - bin/f_pm.r')                     # Function to define prior means
source('WIP/PISensitivity/endpoint - bin/f_inits.r')                  # Function to define all initial values
source('WIP/PISensitivity/endpoint - bin/f_postparam_jags.r')         # Function to compute postparam
source('WIP/PISensitivity/endpoint - bin/f_postparam_jagsmodel.r')    # Function to compute postparam (not used)
source('WIP/PISensitivity/endpoint - bin/f_ace_1sim.r')               # Function to compute ACE -> Odds Ratio within each Stratum of Interest

}

##################################
# set parameters
##################################
{ 
OR_S1eq0                = 0.5
OR_S1eq1                = 0.8
gamma                   = 0
gamma_in                = gamma
delta                   = round(log(OR_S1eq0),2)                                ;delta
eta                     = round(log(OR_S1eq1),2) + gamma - delta                ;eta 
n                       = 200
mean                    = 0
sd                      = 1
alpha0                  = -1.78
alpha1                  = 2
alpha2                  = 0 
beta0                   = 0            
beta1                   = 0.5  
beta2                   = -0.5 
nSim                    = 30          
n.chains                = 3           
n.adapt                 = 1000
n.burnin                = 50
n.iter                  = 200
  thin                  = 2
parSave                 = c("Y0","Y1","S1", "beta", "alpha")
file                    = "mod.txt"
}

##################################
# ACE (fixed gamma)
##################################
 
 result_df   = NULL
 
for (i in 1:nSim) { 
  
  dat_in                    = f_sim(n,mean,sd,alpha0, alpha1, alpha2, beta0, beta1, beta2, eta, delta, gamma)  
  OR_S1eq1                  = f_OR_S1eq1(data = dat_in)
  OR_S1eq0                  = f_OR_S1eq0(data = dat_in)
  pm_result                 = f_pm(dat = dat_in)
  dat.jags                  = f_datjags(dat = dat_in,gamma_in,pm_result)
  inits                     = f_inits(dat=dat_in) 
  postparam                 = f_postparam_jags(dat.jags,inits,parSave,text,n.chains,n.iter,n.burnin,thin)
# postparam                 = f_postparam_jags.model(file,dat.jags,inits,n.chains,n.adapt,parSave,n.iter,thin)
  ace                       = f_ace_1sim( postparam=postparam,dat=dat_in)     
  result_df                 =  rbind.data.frame(result_df, data.frame(Sim        = i,
                                                                      Gamma_true = gamma, 
                                                                      Gamma_in   = gamma_in, 
                                                                      OR_S1eq1   = as.numeric(OR_S1eq1), 
                                                                      OR_S1eq0   = as.numeric(OR_S1eq0),
                                                                      ace))      

}
result_df                             

# Bayesian Estimated OR
mean(result_df$or.S1eq0)  
mean(result_df$or.S1eq1)  

# True OR in Simulated Data                     
mean(result_df$OR_S1eq0)  
mean(result_df$OR_S1eq1)  

# Theoretical OR (Defined in Parameter Settings)
# OR_S1eq0 = 0.5
# OR_S1eq1 = 0.8
  