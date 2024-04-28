rm(list = ls())
 
library(nnet)
library(dplyr)
library(R2jags)  ;
library(dplyr)
library(adace)
#----------------------
#  Functions
#----------------------
source("Data Simulator/adace simulator/f_sim.r")
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

#----------------------
#  Parameter Settings
#----------------------
 
{ # simulation
n              = 1000
seed_0         = 2020                               # for f_sim
nsim           = 100                                # for f_sim
seed_v         = seq(seed_0, seed_0+nsim-1,1)       # for f_sim (for argument of f_sim -- seed := seed_v[i] for the ith simulation)
#n_bs           = 1000
#seed           = seq(1,n,1)                        # argument of f_sim: set seed
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
visit          = 3


}
{ # bayesian
  parSave        = c("delta","S0","S1","Y0","Y1","w")     # argument of jags() 
  n.chains       = 1                                      # argument of jags()
  n.burnin       = 200                                     # argument of jags()
  n.iter         = 500                                     # argument of jags()
  thin           = 1                                      # argument of jags()
  file           = "mod.txt"                              # argument of jags()
  n.adapt        = 1000                                   # argument of jags.model()
}
{ # principal score
  n_ps           = 5000                             # Instead of using n as other methods do, ps method needs much larger sample to bootstrap
  seed_M_0       = 10                               # for bootstrap sample()
  M              = 5                                # for bootstrap sample()
  seed_M_v       = seq(seed_M_0, seed_M_0+M-1,1)    # for bootstrap sample()
  UtoRemove = 'NeverAdhere/11/D'
  ep1            = 1;
  ep0            = 1; 
  iter.max       = 200;
  error0         = 10^-6
  Trace          = T;
}

#----------------------
#  nsim
#----------------------
result_df_BS           = NULL
result_df_AD           = NULL
result_df_PS           = NULL  

for(i in 1:nsim){
 
  # DATA SIMULATION
  
   {
    sim          = f_sim(seed_v[i],   n = n,    alpha1,alpha2,alpha3,beta,gamma1,gamma2,gamma3,TrtEff_adhnei,TrtEff_adhboth,TrtEff_adhact,TrtEff_adhpbo)
    full         = sim$full_long %>% filter(AVISITN==visit)  
    full         = f_D(full) 
    dat_in        = full  %>%  mutate( Y0                = ifelse(TRT==0, Y, NA),
                                       Y1                = ifelse(TRT==1, Y, NA),
                                       Z                 = TRT,
                                       S                 = ICE,
                                       S0                = ifelse(TRT==0, S, NA),
                                       S1                = ifelse(TRT==1, S, NA),
                                       X_1_standardized  = X_1,  # -mean(X_1),
                                       X_2_standardized  = X_2,  # -mean(X_2),
                                       base_standardized = BASE) # -mean(BASE)    )
  }
  
  # BAYESIAN  
  
   { 
   # DATA SIMULATION : done.
   
   # TRUE 
    trued_H       = sim$true_d_Tm$true_d_Tm_adhpbo
    trued_D       = sim$true_d_Tm$true_d_Tm_adhnei
    trued_I       = sim$true_d_Tm$true_d_Tm_adhboth
    trued_B       = sim$true_d_Tm$true_d_Tm_adhact 
    
    # ACE
    I             = f_I(dat=dat_in)
    pm_results    = f_pm(sim)
    dat.jags      = f_datjags(dat = dat_in, I=I,pm_results=pm_results) 
    inits         = f_inits(dat=dat_in) 
    postparam     = f_postparam_jags(dat.jags,inits,parSave,text,n.chains,n.iter,n.burnin,thin)        
    ace_BS        = f_ace_1sim(postparam = postparam,dat=dat_in,I=I)   
    
    # STACK
    result_df_BS   =  rbind.data.frame(result_df_BS, 
                                      data.frame(Sim = i, 
                                                 trued_H_01 = trued_H,
                                                 trued_D_11 = trued_D,
                                                 trued_I_00 = trued_I, 
                                                 trued_B_10 = trued_B,
                                                 ace_BS) 
    )
 }
  
  # AdACE

   {
    # DATA SIMULATION : done.
     
    # TRUE
     
     trued_adhpbo       = sim$true_d_Tm$true_d_Tm_adhpbo
     trued_adhnei       = sim$true_d_Tm$true_d_Tm_adhnei
     trued_adhboth      = sim$true_d_Tm$true_d_Tm_adhboth
     trued_adhact       = sim$true_d_Tm$true_d_Tm_adhact 
    
    # ACE 
     
    X             = dat_in %>% select(X_1, X_2) %>% as.matrix()
    A             = sim$A_forAdACE
    Y             = dat_in %>% pull(Y)
    Z             = sim$Z_forAdACE
    TRT           = dat_in %>% pull(TRT)
    fit_starplusA = est_S_Star_Plus_MethodA(X<-X,A<-A,Z<-Z,Y<-Y,TRT<-TRT)        # estimate ACE in *+ stratum (adh to ACT at least) - Method A
    fit_starplusB = est_S_Star_Plus_MethodB(X<-X,A<-A,Z<-Z,Y<-Y,TRT<-TRT )       # estimate ACE in *+ stratum (adh to ACT at least) - Method B
    fit_plusplusA = est_S_Plus_Plus_MethodA(X<-X,A<-A,Z<-Z,Y<-Y,TRT<-TRT)        # estimate ACE in ++ stratum (adh to both arms) - Method A
    fit_plusplusB = est_S_Plus_Plus_MethodB(X<-X,A<-A,Z<-Z,Y<-Y,TRT<-TRT)        # estimate ACE in ++ stratum (adh to both arms) - Method B
    ace_AD        = cbind.data.frame(  
                                        adace_mean_starplusA           = fit_starplusA[[1]],       
                                        adace_se_starplusA             = fit_starplusA[[2]],
                                        adace_res1_starplusA           = fit_starplusA[[3]],
                                        adace_res0_starplusA           = fit_starplusA[[4]],
                                        adace_se_res1_starplusA        = fit_starplusA[[5]],
                                        adace_se_res0_starplusA        = fit_starplusA[[6]],
                                        
                                        adace_mean_starplusB            = fit_starplusB[[1]],      
                                        adace_se_starplusB              = fit_starplusB[[2]],
                                        adace_res1_starplusB            = fit_starplusB[[3]],
                                        adace_res0_starplusB            = fit_starplusB[[4]],
                                        adace_se_res1_starplusB         = fit_starplusB[[5]],
                                        adace_se_res0_starplusB         = fit_starplusB[[6]],
                                        
                                        adace_mean_plusplusA            = fit_plusplusA[[1]],      
                                        adace_se_plusplusA              = fit_plusplusA[[2]],
                                        adace_res1_plusplusA            = fit_plusplusA[[3]],
                                        adace_res0_plusplusA            = fit_plusplusA[[4]],
                                        adace_se_res1_plusplusA         = fit_plusplusA[[5]],
                                        adace_se_res0_plusplusA         = fit_plusplusA[[6]],
                                        
                                        adace_mean_plusplusB            = fit_plusplusB[[1]],     
                                        adace_se_plusplusB              = fit_plusplusB[[2]],
                                        adace_res1_plusplusB            = fit_plusplusB[[3]],
                                        adace_res0_plusplusB            = fit_plusplusB[[4]],
                                        adace_se_res1_plusplusB         = fit_plusplusB[[5]],
                                        adace_se_res0_plusplusB         = fit_plusplusB[[6]] 
       
                                     )
  
    
   
    # STACK 
    result_df_AD   =  rbind.data.frame(result_df_AD, 
                                       data.frame(Sim = i, 
                                                  trued_01 = trued_adhpbo,
                                                  trued_11 = trued_adhnei,
                                                  trued_00 = trued_adhboth, 
                                                  trued_10 = trued_adhact,
                                                  ace_AD)
                                       ) 
  
}
  
  # PS
  
   { # DATA SIMULATION -- Re-Create data : Simulate a larger data pool (sample size: n_ps) and bootstrap. 
    sim          = f_sim(seed_v[i],  n=n_ps  ,alpha1,alpha2,alpha3,beta,gamma1,gamma2,gamma3,TrtEff_adhnei,TrtEff_adhboth,TrtEff_adhact,TrtEff_adhpbo)
    full         = sim$full_long %>% filter(AVISITN==visit)  
    full         = f_D(full)
    ps_in        = full %>% filter(U!=UtoRemove) %>%   rename( Z=TRT) %>% mutate(indexZD=paste0(Z,D))
    
    # TRUE 
    trued_A      = sim$true_d_Tm$true_d_Tm_adhact
    trued_C      = sim$true_d_Tm$true_d_Tm_adhboth
    trued_N      = sim$true_d_Tm$true_d_Tm_adhpbo
    trued_A_nsl  = sim$true_d_Tm_nsl$true_d_Tm_adhact_nsl
    trued_C_nsl  = sim$true_d_Tm_nsl$true_d_Tm_adhboth_nsl
    trued_N_nsl  = sim$true_d_Tm_nsl$true_d_Tm_adhpbo_nsl
    
    # ACE 
    ace_PS          = f_PS_MBoot_1sim(data_boot <- ps_in, ep0,ep1,M,iter.max,error0,seed_M_v)
    
    # STACK
    result_df_PS    =  rbind.data.frame( result_df_PS, 
                                         cbind.data.frame(Sim             = i, 
                                                          trued_N_01      = trued_N,
                                                          trued_N_01_nsl  = trued_N_nsl,
                                                          trued_C_00      = trued_C,
                                                          trued_C_00_nsl  = trued_C_nsl,
                                                          trued_A_10      = trued_A,
                                                          trued_A_10_nsl  = trued_A_nsl,
                                                          ace_PS)  
    )
  }
  
}

# ALL RESULTS
result_df_PS
result_df_BS
result_df_AD

colMeans(result_df_PS)
colMeans(result_df_BS)
colMeans(result_df_AD)

 
write.csv(result_df_PS,"3methods comparison/resultprint/PS.csv")
write.csv(result_df_BS,"3methods comparison/resultprint/BS.csv")
write.csv(result_df_AD,"3methods comparison/resultprint/AD.csv")
 
