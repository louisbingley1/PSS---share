
#======================
#  PS_dataframe
#======================

library(nnet)
library(dplyr)

#----------------------
#  Functions
#----------------------
{
  source("Principal Score/main/f_sim.r")
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
#  Table Print
#----------------------
result_df           = NULL #cbind.data.frame(sim=1:nsim,AACE=rep(NA,nsim))
i                   = 1

# DATA  
sim          = f_sim(seed_v[i],n,alpha1,alpha2,alpha3,beta,gamma1,gamma2,gamma3,TrtEff_adhnei,TrtEff_adhboth,TrtEff_adhact,TrtEff_adhpbo)
full         = sim$full_long %>% filter(AVISITN==visit)  
full         = f_D(full)
data_in      = full %>% filter(U!=UtoRemove) %>%   rename( Z=TRT) %>%  mutate(indexZD=paste0(Z,D))

# TRUE 
trued_A      = sim$true_d_T3$true_d_T3_adhact
trued_C      = sim$true_d_T3$true_d_T3_adhboth
trued_N      = sim$true_d_T3$true_d_T3_adhpbo
trued_A_nsl  = sim$true_d_T3_nsl$true_d_T3_adhact_nsl
trued_C_nsl  = sim$true_d_T3_nsl$true_d_T3_adhboth_nsl
trued_N_nsl  = sim$true_d_T3_nsl$true_d_T3_adhpbo_nsl

# f_PS_MBoot_1sim(data_in, ep0,ep1,M,iter.max,error0,seed_M_v)
N            = nrow(data_in)                     # number of subjects in data
n            = ceiling(0.2*N)                     # sample full number(=0.5N) of subjects in each bootstrap data
CACE_bs      = rep(NA,M) 
CACE.adj_bs  = rep(NA,M)
CPCP_bs      = rep(NA,M)
NACE_bs      = rep(NA,M) 
NACE.adj_bs  = rep(NA,M)
NPCP_bs      = rep(NA,M)
AACE_bs      = rep(NA,M) 
AACE.adj_bs  = rep(NA,M)
APCP_bs      = rep(NA,M)
CAACE_bs     = rep(NA,M) 
CAACE.adj_bs = rep(NA,M)


m             = 1
set.seed(seed_M_v[m])
boot          = data_in[sample(1:N,n,replace=F),] ;head(boot) # re-sample data with replacement
result        = f_PS( data <- boot, ep1,ep0,beta.a,beta.n, iter.max,error0)
outtable      = result$AugData

write.csv(outtable,"Principal Score/resultprint/PS_df.csv")
