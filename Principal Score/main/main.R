rm(list=ls())

library(nnet)
library(dplyr)

#----------------------
#  Functions
#----------------------
{
#source("Data Simulator/adace simulator/f_sim.r")                                  # adace simulator: or source('Magnusson/endpoint - ctn/main/f_sim.r')                                   # Function to simulate data  
source("Data Simulator/cities simulator/f_sim.r")                                 # cities simulatorsource("Principal Score/main/f_augdata.r")
source("Principal Score/main/f_EM_betas.r")
source("Principal Score/main/f_pi.r")
source("Principal Score/main/f_w.r")
source("Principal Score/main/f_prob.r")
source("Principal Score/main/f_coeff.r")
source("Principal Score/main/f_PS.r")
source("Principal Score/main/f_PS_MBoot_1sim.r")
source("Principal Score/main/f_D.r")
source("Principal Score/main/f_augdata.r")
}

#----------------------
#  Parameter Settings
#----------------------

# source("Data Simulator/adace simulator/setting_adace.r")                           # adace simulator
# n = 5000                                                                             # argument of f_sim: sample size in each 1 simulated trial

{
# source("Data Simulator/cities simulator/scenarios/trt_large_scen_A.r")             # cities simulator: large  trt effect,  scenario A
# source("Data Simulator/cities simulator/scenarios/trt_large_scen_B.r")             # cities simulator: large  trt effect,  scenario B
 source("Data Simulator/cities simulator/scenarios/trt_large_scen_C.r")             # cities simulator: large  trt effect,  scenario C
# source("Data Simulator/cities simulator/scenarios/trt_large_scen_D.r")             # cities simulator: large  trt effect,  scenario D
# source("Data Simulator/cities simulator/scenarios/trt_modest_scen_A.r")            # cities simulator: modest trt effect,  scenario A
# source("Data Simulator/cities simulator/scenarios/trt_modest_scen_B.r")            # cities simulator: modest trt effect,  scenario B
# source("Data Simulator/cities simulator/scenarios/trt_modest_scen_C.r")            # cities simulator: modest trt effect,  scenario C
# source("Data Simulator/cities simulator/scenarios/trt_modest_scen_D.r")            # cities simulator: modest trt effect,  scenario D
# source("Data Simulator/cities simulator/scenarios/trt_null_scen_A.r")              # cities simulator: null   trt effect,  scenario A
# source("Data Simulator/cities simulator/scenarios/trt_null_scen_B.r")              # cities simulator: null   trt effect,  scenario B
# source("Data Simulator/cities simulator/scenarios/trt_null_scen_C.r")              # cities simulator: null   trt effect,  scenario C
# source("Data Simulator/cities simulator/scenarios/trt_null_scen_D.r")              # cities simulator: null   trt effect,  scenario D
# source("Data Simulator/cities simulator/scenarios/diff_1_scen_1.R")                 # cities simulator:
# source("Data Simulator/cities simulator/scenarios/diff_1_scen_2.R")                 # cities simulator:
# source("Data Simulator/cities simulator/scenarios/diff_1_scen_3.R")                 # cities simulator:
# source("Data Simulator/cities simulator/scenarios/diff_1_scen_4.R")                 # cities simulator:
# source("Data Simulator/cities simulator/scenarios/diff_1_scen_5.R")                 # cities simulator:
# source("Data Simulator/cities simulator/scenarios/diff_1_scen_6.R")                 # cities simulator:
}
 

{
seed_0     = 100
seed_v     = seq(seed_0, seed_0+nSim-1,1)

seed_M_0   = 2020
M          = 5
seed_M_v   = seq(seed_M_0, seed_M_0+M-1,1)
ep1        = 1;
ep0        = 1;  
Trace      =  T;
iter.max   = 200;
error0     = 10^-6
UtoRemove  = 'NeverAdhere/11/D'
}

 
#----------------------
#  ACE
#----------------------
result_df           = NULL #cbind.data.frame(sim=1:nsim,AACE=rep(NA,nsim))

for(i in 1:nSim){
  
  # DATA  
#  sim          = f_sim(seed_v[i],n,alpha1,alpha2,alpha3,beta,gamma1,gamma2,gamma3,TrtEff_adhnei,TrtEff_adhboth,TrtEff_adhact,TrtEff_adhpbo)
#  full         = sim$full_long %>% filter(AVISITN==visit)  
#  full         = f_D(full)
#  data_in      = full %>% filter(U!=UtoRemove) %>%   rename( Z=TRT) %>%  mutate(indexZD=paste0(Z,D))
  
# cities simulator : simulate 1 dataset and prepare for variables
  sim          = f_sim(seed_val <- seed_vec[i], n_patient_vector, p_loe_max, z_l_loe,  z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list,  sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae,  prob_ae,  reference_id, plot_po, up_good,  threshold, delta_adjustment_in, covariate_df) 
  full         = sim$observed_out %>% filter(AVISITN==4) 
  full         = f_D(full)
  data_in      = full %>% filter(U!=UtoRemove) %>%   rename( Z=TRT) %>%  mutate(indexZD=paste0(Z,D))
  
  # TRUE 
  trued_A      = sim$true_d_Tm$true_d_Tm_adhact
  trued_C      = sim$true_d_Tm$true_d_Tm_adhboth
  trued_N      = sim$true_d_Tm$true_d_Tm_adhpbo
  trued_A_nsl  = sim$true_d_Tm_nsl$true_d_Tm_adhact_nsl
  trued_C_nsl  = sim$true_d_Tm_nsl$true_d_Tm_adhboth_nsl
  trued_N_nsl  = sim$true_d_Tm_nsl$true_d_Tm_adhpbo_nsl
  
  # ACE 
  ace          = f_PS_MBoot_1sim(data_in, ep0,ep1,M,iter.max,error0,seed_M_v)
  
  # STACK
  result_df    =  rbind.data.frame(result_df, 
                                    cbind.data.frame(Sim = i, 
                                                     trued_N_01      = trued_N,
                                                     trued_N_01_nsl  = trued_N_nsl,
                                                     trued_C_00      = trued_C,
                                                     trued_C_00_nsl  = trued_C_nsl,
                                                     trued_A_10      = trued_A,
                                                     trued_A_10_nsl  = trued_A_nsl,
                                                     ace)  
  )
  
}


result_df 

# ACE  
mean(result_df$AACE); mean(result_df$AACE.adj)
mean(result_df$NACE); mean(result_df$NACE.adj)
mean(result_df$CACE); mean(result_df$CACE.adj)
 

# causal/trt effect in simulated data (at visit 3)
mean(result_df$trued_A_10); mean(result_df$trued_A_10_nsl)
mean(result_df$trued_N_01); mean(result_df$trued_N_01_nsl)
mean(result_df$trued_C_00); mean(result_df$trued_C_00_nsl) 


# true/theoretical value of causal/trt effect (defined in f_setting)
# TrtEff_adhact   = 1.5                               # A
# TrtEff_adhpbo   = 0.5                               # N
# TrtEff_adhboth  = 2                                 # C

# PCP - Prob of correct prediction
mean(result_df$APCP)
mean(result_df$NPCP)
mean(result_df$CPCP)

#write.csv(result_df,"Principal Score/resultprint/result_df.csv")
