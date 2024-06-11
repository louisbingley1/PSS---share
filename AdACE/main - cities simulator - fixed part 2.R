# rm(list = ls())
 
# library(nnet)
# library(dplyr)
# library(R2jags)  ;
# library(dplyr)
# library(adace)

#======================
#  Functions
#======================
# ...

#=======================
#  Parameter Settings
#=======================

# [1.a] Parameters in adace simulator
# ...

# [1.b] Parameters in cities simulator
# ...

# [2] Parameters in Bayeisan/Magnusson
{ 
  parSave        = c("delta","S0","S1","Y0","Y1","w")                                # argument of jags() 
  n.chains       = 2                                                                 # argument of jags()
  n.burnin       = 20                                                                # argument of jags()
  n.iter         = 100                                                               # argument of jags()
  thin           = 2                                                                 # argument of jags()
  file           = "mod.txt"                                                         # argument of jags()
  #n.adapt        = 1000                                                              # argument of jags.model()
}  

# [3] Parameters in Principal Score
{
  #n_ps                 = 5000                                                           # update n with a larger n -- for bootstrap [adace simulator]
  n_patient_vector_ps  = 3*c(n_patient_ctrl, n_patient_expt)                            # update n with a larger n -- for bootstrap [cities simulator]     
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

#===============================
#  ACE from nSim Simulations
#===============================

result_df_BS           = NULL
result_df_AD           = NULL
result_df_PS           = NULL  

for(i in 1:nSim){
 
  #--------------------
  # DATA SIMULATION
  #--------------------
  
  #  adace simulator :  
  { 
#   sim     = f_sim(seed_v[i],n,alpha1,alpha2,alpha3,beta,gamma1,gamma2,gamma3,TrtEff_adhnei,TrtEff_adhboth,TrtEff_adhact,TrtEff_adhpbo)
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
  
  # cities simulator :  
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
  
  #--------------------
  # BAYESIAN  
  #--------------------
   { 
   # DATA SIMULATION : done.
   
   # TRUE 
    trued_H       = sim$true_d_Tm$true_d_Tm_adhpbo
    trued_D       = sim$true_d_Tm$true_d_Tm_adhnei
    trued_I       = sim$true_d_Tm$true_d_Tm_adhboth
    trued_B       = sim$true_d_Tm$true_d_Tm_adhact 
    trued_sp      = sim$true_d_Tm$true_d_Tm_sp
    
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
                                                 trued_IB_sp = trued_sp,
                                                 ace_BS) 
    )
  }
  
  #--------------------
  # AdACE
  #-------------------- 
   {
    # DATA SIMULATION : done.
     
    # TRUE
     
     trued_adhpbo       = sim$true_d_Tm$true_d_Tm_adhpbo
     trued_adhnei       = sim$true_d_Tm$true_d_Tm_adhnei
     trued_adhboth      = sim$true_d_Tm$true_d_Tm_adhboth
     trued_adhact       = sim$true_d_Tm$true_d_Tm_adhact 
     trued_sp           = sim$true_d_Tm$true_d_Tm_sp
     
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
                                                  trued_sp = trued_sp,
                                                  ace_AD)
                                       ) 
  
}
  
  #--------------------
  # PRINCIPAL SCORE
  #-------------------- 
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
    trued_AC     = sim$true_d_Tm$true_d_Tm_sp
    trued_A_nsl  = sim$true_d_Tm_nsl$true_d_Tm_adhact_nsl
    trued_C_nsl  = sim$true_d_Tm_nsl$true_d_Tm_adhboth_nsl
    trued_N_nsl  = sim$true_d_Tm_nsl$true_d_Tm_adhpbo_nsl
    trued_AC_nsl = sim$true_d_Tm_nsl$true_d_Tm_sp_nsl
    
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
                                                          trued_AC_sp     = trued_AC,
                                                          trued_AC_sp_nsl = trued_AC_nsl,
                                                          ace_PS)  
    )
  }
  
}

#  ALL RESULTS
# result_df_PS
# result_df_BS
# result_df_AD

# colMeans(result_df_PS)
# colMeans(result_df_BS)
# colMeans(result_df_AD)


#==========================
# deliver
#==========================

#----------------------------------
# from adace simulator
#----------------------------------

# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/adace simulator/PS_adacesimulator.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/adace simulator/BS_adacesimulator.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/adace simulator/AD_adacesimulator.csv")

#----------------------------------
# from cities simulator
#----------------------------------

# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_large_scen_B.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_large_scen_B.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_large_scen_B.csv")
 
# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_large_scen_C.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_large_scen_C.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_large_scen_C.csv")

# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_large_scen_D.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_large_scen_D.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_large_scen_D.csv")
 
# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_modest_scen_B.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_modest_scen_B.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_modest_scen_B.csv")
 
# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_modest_scen_C.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_modest_scen_C.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_modest_scen_C.csv")
 
# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_modest_scen_D.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_modest_scen_D.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_modest_scen_D.csv")
 
# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_null_scen_B.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_null_scen_B.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_null_scen_B.csv")
 
# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_null_scen_C.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_null_scen_C.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_null_scen_C.csv")
 
# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_null_scen_D.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_null_scen_D.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_null_scen_D.csv")
 
# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_diff_1_scen_1.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_diff_1_scen_1.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_diff_1_scen_1.csv")

# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_diff_1_scen_2.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_diff_1_scen_2.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_diff_1_scen_2.csv")

# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_diff_1_scen_3.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_diff_1_scen_3.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_diff_1_scen_3.csv")

# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_diff_1_scen_4.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_diff_1_scen_4.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_diff_1_scen_4.csv")

# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_diff_1_scen_5.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_diff_1_scen_5.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_diff_1_scen_5.csv")

# write.csv(result_df_PS,"Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_diff_1_scen_6.csv")
# write.csv(result_df_BS,"Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_diff_1_scen_6.csv")
# write.csv(result_df_AD,"Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_diff_1_scen_6.csv")

 