################################################################################################
###         Simulate data from Sensitivity Analysis Paper 
###           Type of Endpoint: Time to Event  
################################################################################################
rm(list=ls())
setwd("C:/Users/liubing8/OneDrive - Merck Sharp & Dohme LLC/Documents/Github Ripos/PSS---share/WIP/PISensitivity/endpoint - tte")
library(survival)
library(rjags)
library(dplyr)
library(R2jags)

################################## 
# Functions
##################################
{
source('f_sim.r')                                     # Function to simulate data
source('f_trueHR.r')                                  # Function to calculate the true causal effect(HR) for simulated datasets  
source('f_mod.r')                                     # Function to define model and write to file
source('f_datjags.r')                                 # Function to define dat.jags
source('f_pm.r')                                      # Function to define the prior means in the model:
source('f_inits.r')                                   # Function to define all initial values    
source('f_postparam_jags.r')                          # Function to compute postparam
source('f_postparam_jagsmodel.r')                     # Function to compute postparam
source('f_ace_1sim.r')                                # Function to compute ACE -> Hazard Ratio for each Stratum of Interest
source('f_cutoff.r')                                  # Function to determine cutoff  
}
##################################
# set parameters
##################################
{
HR_S1eq0                = 0.5
HR_S1eq1                = 0.8
gamma                   = 0
gamma_in                = gamma
delta                   = round(log(HR_S1eq0),2)                        ;delta
eta                     = round(log(HR_S1eq1),2) + gamma - delta        ;eta 
n                       = 300
mean                    = 0
sd                      = 1
alpha0                  = -1.78
alpha1                  = 2
alpha2                  = 0 
beta0                   = -3 
beta1                   = 0.5  
beta2                   = -0.5 
nSim                    = 10          
n.chains                = 3
n.burnin                = 30
n.adapt                 = 1000
n.iter                  = 100
  thin                  = 2
parSave                 = c("Y0","Y1","S1", "beta", "alpha")
file                    = "mod.txt"
cutoff                  = 100
#nsim_pm                = 50

# cutoff determination:
# dat_in = f_sim(n,mean,sd,alpha0, alpha1, alpha2, beta0, beta1, beta2, eta, delta, cutoff,gamma)
# f_cutoff(data = dat_in)
}


##################################
# ACE (fixed gamma)
##################################

result_df =  NULL
for (i in 1:nSim) {  
                     dat_in       =  f_sim(n,mean,sd,alpha0, alpha1, alpha2, beta0, beta1, beta2, eta, delta, cutoff, gamma)    
                     HR_S1eq1     =  f_trueHR(dat=dat_in)$trueHR_S1eq1
                     HR_S1eq0     =  f_trueHR(dat=dat_in)$trueHR_S1eq0
                     pm_result    =  f_pm(dat=dat_in)
                     dat.jags     =  f_datjags(dat=dat_in, gamma_in,pm_result )  
                     inits        =  f_inits(dat=dat_in,gamma_in,pm_result) 
                     postparam    =  f_postparam_jags(dat.jags,inits,parSave,text,n.chains,n.iter,n.burnin,thin)
                     #postparam   =  f_postparam_jags.model(file,dat.jags,inits,n.chains,n.adapt,parSave,n.iter,thin)
                     ace          =  f_ace_1sim( postparam = postparam,dat=dat_in,cutoff = cutoff)    
                     result_df    =  rbind.data.frame(result_df, data.frame(Sim = i,
                                                                            Gamma_true = gamma, 
                                                                            Gamma_in   = gamma_in,
                                                                            HR_S1eq0   = as.numeric(HR_S1eq0),
                                                                            HR_S1eq1   = as.numeric(HR_S1eq1),
                                                                            ace))       
                 
}
result_df                             

# Bayesian Estimated HR
mean(result_df$hr.S1eq0)
mean(result_df$hr.S1eq1)  

# True HR in Simulated Data                     
mean(result_df$HR_S1eq0)  
mean(result_df$HR_S1eq1) 

# Theoretical HR (Defined in Parameter Settings)
# HR_S1eq0 = 0.5
# HR_S1eq1 = 0.8

 




