rm(list = ls())
 
library(nnet)
library(dplyr)
library(R2jags)  ;
library(dplyr)
library(adace)
#----------------------
#  Functions
#----------------------
source("Data Simulator/adace simulator/f_sim.r")                                  # adace simulator: or source('Magnusson/endpoint - ctn/main/f_sim.r')                                   # Function to simulate data  
#source("Data Simulator/cities simulator/f_sim.r")                                 # cities simulator
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

# adace simulator
  nSim  = 10                                                                         # number of simulated trials
  n     = 1000                                                                       # sample size per trial
  source("Data Simulator/adace simulator/setting_adace.r")                           # adace simulator

# cities simulator
{
#   nSim  = 10                                                                         # number of simulated trials
#   n_patient_ctrl = 200
#   n_patient_expt = 200
  
  # source("Data Simulator/cities simulator/scenarios/trt_large_scen_A.r")             # cities simulator: large  trt effect,  scenario A
  # source("Data Simulator/cities simulator/scenarios/trt_large_scen_B.r")             # cities simulator: large  trt effect,  scenario B
  # source("Data Simulator/cities simulator/scenarios/trt_large_scen_C.r")             # cities simulator: large  trt effect,  scenario C
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

# Bayeisan/Magnusson
{ 
  parSave        = c("delta","S0","S1","Y0","Y1","w")                                # argument of jags() 
  n.chains       = 2                                                                 # argument of jags()
  n.burnin       = 20                                                                # argument of jags()
  n.iter         = 100                                                               # argument of jags()
  thin           = 2                                                                 # argument of jags()
  file           = "mod.txt"                                                         # argument of jags()
  #n.adapt        = 1000                                                              # argument of jags.model()
}  

# Principal Score
{
  n_ps                 = 5000                                                           # update n with a larger n -- for bootstrap [adace simulator]
  #n_patient_vector_ps  = 3*n_patient_vector                                             # update n with a larger n -- for bootstrap [cities simulator]     
  seed_M_0             = 2020
  M                    = 5
  seed_M_v             = seq(seed_M_0, seed_M_0+M-1,1)
  ep1                  = 1;
  ep0                  = 1;  
  Trace                =  T;
  iter.max             = 200;
  error0               = 10^-6
  UtoRemove            = 'NeverAdhere/11/D'
}
#----------------------
#  nsim
#----------------------
result_df_BS           = NULL
result_df_AD           = NULL
result_df_PS           = NULL  

for(i in 1:nSim){
 
  # DATA SIMULATION
  
  #  adace simulator :  
  { 
#    sim     = f_sim(seed_v[i],n,alpha1,alpha2,alpha3,beta,gamma1,gamma2,gamma3,TrtEff_adhnei,TrtEff_adhboth,TrtEff_adhact,TrtEff_adhpbo)
#    dat_    = sim$full_long %>% filter(AVISITN==visit)  ; for(r in 1:nrow(dat_)){dat_$Utrue[r] = strsplit(dat_$U[r],"/")[[1]][3] }
#    dat_in  = dat_ %>%  mutate(      Y0                = ifelse(TRT==0, Y, NA),
#                                     Y1                = ifelse(TRT==1, Y, NA),
#                                     Z                 = TRT,
#                                     S                 = ICE,
#                                     S0                = ifelse(TRT==0, S, NA),
#                                     S1                = ifelse(TRT==1, S, NA),
#                                     X_1_standardized  = X_1-mean(X_1),
#                                     X_2_standardized  = X_2-mean(X_2),
#                                     base_standardized = BASE-mean(BASE)   )
  }
  
  # cities simulator : simulate 1 dataset and prepare for variables
  {
      sim     = f_sim(seed_val <- seed_vec[i], n_patient_vector, p_loe_max, z_l_loe,  z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list,  sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae,  prob_ae,  reference_id, plot_po, up_good,  threshold, delta_adjustment_in, covariate_df) 
      dat_    = sim$observed_out %>% filter(AVISITN==maxtime) 
      dat_in  = dat_ %>%  mutate(    Y0                = ifelse(TRT==0, Y, NA),
                                     Y1                = ifelse(TRT==1, Y, NA),
                                     Z                 = TRT,
                                     S                 = ICE,
                                     S0                = ifelse(TRT==0, S, NA),
                                     S1                = ifelse(TRT==1, S, NA),
                                     X_1_standardized  = X_1-mean(X_1),
                                     X_2_standardized  = X_2-mean(X_2),
                                     base_standardized = BASE-mean(BASE)   )
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
     
    X             = dat_in %>% ungroup() %>% select(X_1, X_2) %>% as.matrix()
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
  
  # PRINCIPAL SCORE
  
   { 
    # DATA SIMULATION -- Re-Create data : Simulate a larger data pool (sample size: n_ps or n_patient_vector_ps) and bootstrap. 
     
    # adace simulator:
     {
      # sim          = f_sim(seed_v[i],n<-n_ps,alpha1,alpha2,alpha3,beta,gamma1,gamma2,gamma3,TrtEff_adhnei,TrtEff_adhboth,TrtEff_adhact,TrtEff_adhpbo)
      # full         = sim$full_long %>% filter(AVISITN==visit)  
      # full         = f_D(full)
      # ps_in        = full %>% filter(U!=UtoRemove) %>%   rename( Z=TRT) %>%  mutate(indexZD=paste0(Z,D))
     } 
     
    # cities simulator : simulate 1 dataset and prepare for variables
     {  
         sim          = f_sim(seed_val <- seed_vec[i], n_patient_vector<-n_patient_vector_ps, p_loe_max, z_l_loe,  z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list,  sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae,  prob_ae,  reference_id, plot_po, up_good,  threshold, delta_adjustment_in, covariate_df) 
         full         = sim$observed_out %>% filter(AVISITN==maxtime) 
         full         = f_D(full)
         ps_in        = full %>% filter(U!=UtoRemove) %>%   rename( Z=TRT) %>%  mutate(indexZD=paste0(Z,D))
     }  
     
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

 
 write.csv(result_df_PS,"Comparison/resultprint/adace simulator/PS_adacesimulator.csv")
 write.csv(result_df_BS,"Comparison/resultprint/adace simulator/BS_adacesimulator.csv")
 write.csv(result_df_AD,"Comparison/resultprint/adace simulator/AD_adacesimulator.csv")
# write.csv(result_df_PS,"WIP - Bing/resultprint/cities simulator/PS_citiesimulator.csv")
# write.csv(result_df_BS,"WIP - Bing/resultprint/cities simulator/BS_citiesimulator.csv")
# write.csv(result_df_AD,"WIP - Bing/resultprint/cities simulator/AD_citiesimulator.csv")
