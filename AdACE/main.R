rm(list = ls())
 
library(nnet)
library(dplyr)
library(R2jags)  ;
library(dplyr)
library(adace)

#======================
#  Functions
#======================

source("Data Simulator/cities simulator/f_sim.r")                                     # cities simulator
  

#=======================
#  Parameter Settings
#=======================
 
# [1.b] Parameters in cities simulator
 
   total_data        <- 20
   nsim  = nSim      <- total_data
   n_core            <- 10
   nsim_1core        <- nsim/n_core
   starting_seed_val <- seq(1, nsim-nsim_1core+1, nsim_1core)  # 0:(n_core-1)*nsim_1core + 1
   
   source("Data Simulator/cities simulator/scenarios/scen_vali.r")             # cities simulator: large  trt effect,  scenario B
  
  

#===============================
#  ACE from nSim Simulations
#===============================

result_df_AD           = NULL
 
for(i in 1:nSim){
 
  #--------------------
  # DATA SIMULATION
  #--------------------
  
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
 
  
}

# ALL RESULTS
 

result_df_AD
colMeans(result_df_AD)
colMeans(result_df_AD %>% select(trued_00, adace_mean_plusplusA, adace_se_plusplusA, adace_res1_plusplusA, adace_res0_plusplusA, adace_se_res1_plusplusA, adace_se_res0_plusplusA ))
colMeans(result_df_AD %>% select(trued_sp, adace_mean_starplusA, adace_se_starplusA, adace_res1_starplusA, adace_res0_starplusA, adace_se_res1_starplusA, adace_se_res0_starplusA ))
