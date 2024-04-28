rm(list=ls())
library(R2jags)  ;
library(dplyr)

 
#------------------#
# Functions
#------------------#
{
source("Data Simulator/adace simulator/f_sim.r")                                  # adace simulator: or source('Magnusson/endpoint - ctn/main/f_sim.r')                                   # Function to simulate data  
source("Data Simulator/cities simulator/f_sim.r")                                 # cities simulator
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

source("Data Simulator/adace simulator/setting_adace.r")                           # adace simulator
source("Data Simulator/cities simulator/scenarios/trt_large_scen_D.r")             # cities simulator, scenario D

{
parSave        = c("delta","S0","S1","Y0","Y1","w")                                # argument of jags() 
n.chains       = 2                                                                 # argument of jags()
n.burnin       = 20                                                                # argument of jags()
n.iter         = 100                                                               # argument of jags()
thin           = 2                                                                 # argument of jags()
file           = "mod.txt"                                                         # argument of jags()
n.adapt        = 1000                                                              # argument of jags.model()
}

#------------------#
# ACE 
#------------------#

result_df  =  NULL

for (i in 1:nSim) {  
  
#  adace simulator : simulate 1 dataset and prepare for variables
#   sim           = f_sim(seed[i],n,alpha1,alpha2,alpha3,beta,gamma1,gamma2,gamma3,TrtEff_adhnei,TrtEff_adhboth,TrtEff_adhact,TrtEff_adhpbo)
#   dat_          = sim$full_long %>% filter(AVISITN==3)  ; for(r in 1:nrow(dat_in)){dat_in$Utrue[r] = strsplit(dat_in$U[r],"/")[[1]][3] }
  
  # cities simulator : simulate 1 dataset and prepare for variables
   sim     = f_sim(seed_val <- seed_vec[i], n_patient_vector, p_loe_max, z_l_loe,  z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list,  sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae,  prob_ae,  reference_id, plot_po, up_good,  threshold, delta_adjustment_in, covariate_df) 
   dat_    = sim$observed_out %>% filter(AVISITN==4) 
  
   # dat_in and true ace
   dat_in  = dat_ %>%  mutate(      Y0                = ifelse(TRT==0, Y, NA),
                                    Y1                = ifelse(TRT==1, Y, NA),
                                    Z                 = TRT,
                                    S                 = ICE,
                                    S0                = ifelse(TRT==0, S, NA),
                                    S1                = ifelse(TRT==1, S, NA),
                                    X_1_standardized  = X_1-mean(X_1),
                                    X_2_standardized  = X_2-mean(X_2),
                                    base_standardized = BASE-mean(BASE)   )
    
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

{
# true/theoretical value of causal/trt effect (defined in f_setting)
# TrtEff_adhpbo  = 0.5                             # true treatment/causal effect in stratum [H][1]
# TrtEff_adhnei  = 0                               # true treatment/causal effect in stratum [D][2]
# TrtEff_adhboth = 2                               # true treatment/causal effect in stratum [I][3]
# TrtEff_adhact  = 1.5                             # true treatment/causal effect in stratum [B][4]
}



    
