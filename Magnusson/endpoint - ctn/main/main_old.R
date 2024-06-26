rm(list=ls())
library(R2jags)  ;
library(dplyr)

#------------------#
# Functions
#------------------#
{
source("Data Simulator/adace simulator/f_sim.r")            #source('Magnusson/endpoint - ctn/main/f_sim.r')                                   # Function to simulate data  
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

#------------------#
# set parameters
#------------------#
{
n              = 500                                # argument of f_sim: sample size in each 1 simulated trial
seed           = seq(1,n,1)                         # argument of f_sim: set seed
alpha1         = c(1.3,  0.3, -0.3)                 # argument of f_sim: coefficient of lm(Z_1 ~ X_1 X_2) at time 1, Z_1 is set to be the BASELINE variable
alpha2         = c(  0,    0,    0)                 # argument of f_sim: coefficient of lm(Z_2 ~ X_1 X_2) at time 2, set to 0 s.t. there's no other covariates besides X & BASE
alpha3         = c(  0,    0,    0)                 # argument of f_sim: coefficient of lm(Z_3 ~ X_1 X_2) at time 3, set to 0 s.t. there's no other covariates besides X & BASE
beta           = c(0.3,  0.1, -0.3,  0.2,0,0 )      # argument of f_sim: coefficient of Y ~ X_1 X_2 Z_1 Z_2 Z_3 at time 3, beta[5,6] set to 0  s.t. Y is determined only by X & BASE
gamma1         = c(2.5, -0.1, -0.2, -0.3)           # argument of f_sim: coefficient of lm(A ~ X_1 X_2 Z_1) at time 1
gamma2         = c(2.4, -0.1, -0.2, -0.5)           # argument of f_sim: coefficient of lm(A ~ X_1 X_2 Z_2) at time 2    
gamma3         = c(2.3, -0.1, -0.2, -0.5)           # argument of f_sim: coefficient of lm(A ~ X_1 X_2 Z_1) at time 3, note that Z_3 has been replaced with Z_1, s.t. A is determined only by X & BASE
TrtEff_adhpbo  = 0.5                                # argument of f_sim: true treatment/causal effect in stratum [H][1]
TrtEff_adhnei  = 0                                  # argument of f_sim: true treatment/causal effect in stratum [D][2]
TrtEff_adhboth = 2                                  # argument of f_sim: true treatment/causal effect in stratum [I][3]
TrtEff_adhact  = 1.5                                # argument of f_sim: true treatment/causal effect in stratum [B][4]
nSim           = 3                                  # number of simulated trials
parSave        = c("delta","S0","S1","Y0","Y1","w") # argument of jags() 
n.chains       = 2                                  # argument of jags()
n.burnin       = 20                                # argument of jags()
n.iter         = 100                               # argument of jags()
thin           = 2                                  # argument of jags()
file           = "mod.txt"                          # argument of jags()
n.adapt        = 1000                               # argument of jags.model()
}

#------------------#
# ACE 
#------------------#

result_df  =  NULL

for (i in 1:nSim) {  
  
  # simulate 1 dataset and prepare for variables
  
  sim           = f_sim(seed[i],n,alpha1,alpha2,alpha3,beta,gamma1,gamma2,gamma3,TrtEff_adhnei,TrtEff_adhboth,TrtEff_adhact,TrtEff_adhpbo)
  dat_          = sim$full_long %>% filter(AVISITN==3) 
  dat_in        = dat_ %>%  mutate(Y0                = ifelse(TRT==0, Y, NA),
                                   Y1                = ifelse(TRT==1, Y, NA),
                                   Z                 = TRT,
                                   S                 = ICE,
                                   S0                = ifelse(TRT==0, S, NA),
                                   S1                = ifelse(TRT==1, S, NA),
                                   X_1_standardized  = X_1, #X_1-mean(X_1),
                                   X_2_standardized  = X_2, #X_2-mean(X_2),
                                   base_standardized = BASE #-mean(BASE)   
                                   )
  for(r in 1:nrow(dat_in)){dat_in$Utrue[r] = strsplit(dat_in$U[r],"/")[[1]][3] }
  
  # true causal effect (the true 'd' of stratum D/I/H/B)
  trued_H       = sim$true_d_Tm$true_d_Tm_adhpbo
  trued_D       = sim$true_d_Tm$true_d_Tm_adhnei
  trued_I       = sim$true_d_Tm$true_d_Tm_adhboth
  trued_B       = sim$true_d_Tm$true_d_Tm_adhact 
  
  # ace calculation for 1 simulated dataset
  
  I             = f_I(dat=dat_in)
  pm_results    = f_pm(sim)
  dat.jags      = f_datjags(dat = dat_in, I=I,pm_results=pm_results) 
  inits         = f_inits(dat=dat_in) 
  postparam     = f_postparam_jags(dat.jags,inits,parSave,text,n.chains,n.iter,n.burnin,thin)        
# postparam     = f_postparam_jags.model(file,dat.jags,inits,n.chains,n.adapt,parSave,n.iter,thin)  
  ace           = f_ace_1sim(postparam = postparam,dat=dat_in,I=I)   
  
  # stack results of all simulations
  
  result_df     =  rbind.data.frame(result_df, 
                                    data.frame(Sim = i,                                                 
                                               trued_H_01 = trued_H,
                                               trued_D_11 = trued_D,
                                               trued_I_00 = trued_I, 
                                               trued_B_10 = trued_B,
                                               ace) 
                                    )
  
}
result_df 
  

# ACE (delta version & ITT version)
mean(result_df$delta_H); mean(result_df$ITT_H)
mean(result_df$delta_D); mean(result_df$ITT_D)
mean(result_df$delta_I); mean(result_df$ITT_I)
mean(result_df$delta_B); mean(result_df$ITT_B)
mean(result_df$delta_IB); mean(result_df$ITT_IB)


# causal/trt effect in simulated data (at visit 3)
mean(result_df$trued_H_01)
mean(result_df$trued_D_11)
mean(result_df$trued_I_00)
mean(result_df$trued_B_10)

# true/theoretical value of causal/trt effect (defined in f_setting)
# TrtEff_adhpbo  = 0.5                             # true treatment/causal effect in stratum [H][1]
# TrtEff_adhnei  = 0                               # true treatment/causal effect in stratum [D][2]
# TrtEff_adhboth = 2                               # true treatment/causal effect in stratum [I][3]
# TrtEff_adhact  = 1.5                             # true treatment/causal effect in stratum [B][4]

  



    
