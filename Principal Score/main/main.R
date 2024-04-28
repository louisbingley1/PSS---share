rm(list=ls())

library(nnet)
library(dplyr)

#----------------------
#  Functions
#----------------------
{
source("Data Simulator/adace simulator/f_sim.r")         #source("Principal Score/main/f_sim.r")
source("Principal Score/main/f_augdata.r")
source("Principal Score/main/f_EM_betas.r")
source("Principal Score/main/f_pi.r")
source("Principal Score/main/f_w.r")
source("Principal Score/main/f_prob.r")
source("Principal Score/main/f_coeff.r")
source("Principal Score/main/f_PS.r")
source("Principal Score/main/f_PS_MBoot_1sim.r")
source("Principal Score/main/f_D.r")
}

#----------------------
#  Parameter Settings
#----------------------
{
n              = 5000                               # argument of f_sim: sample size in each 1 simulated trial
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

seed_0     = 100
nsim       = 2
seed_v     = seq(seed_0, seed_0+nsim-1,1)
seed_M_0   = 2020
M          = 5
seed_M_v   = seq(seed_M_0, seed_M_0+M-1,1)


visit    = 3
ep1      = 1;
ep0      = 1;  
Trace    = T;
iter.max = 200;
error0   = 10^-6
UtoRemove = 'NeverAdhere/11/D'
}

 
#----------------------
#  ACE
#----------------------
result_df           = NULL #cbind.data.frame(sim=1:nsim,AACE=rep(NA,nsim))

for(i in 1:nsim){
  
  # DATA  
  sim          = f_sim(seed_v[i],n,alpha1,alpha2,alpha3,beta,gamma1,gamma2,gamma3,TrtEff_adhnei,TrtEff_adhboth,TrtEff_adhact,TrtEff_adhpbo)
  full         = sim$full_long %>% filter(AVISITN==visit)  
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

write.csv(result_df,"Principal Score/resultprint/result_df.csv")
