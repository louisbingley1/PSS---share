hpc_f_2m = function(i,sim_list){

  
  result_df_BS           = NULL
#  result_df_AD           = NULL
  result_df_PS           = NULL  
  
  #--------------------
  # DATA SIMULATION
  #-------------------- 
  # cities simulator :  
  {
    sim     = sim_list[[i]]
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
  
  #--------------------
  # PRINCIPAL SCORE
  #-------------------- 
  { 
    # DATA SIMULATION -- Re-Create data : Simulate a larger data pool (sample size: n_ps or n_patient_vector_ps) and bootstrap. 

    # cities simulator : simulate 1 dataset and prepare for variables
    {  
      sim          = sim
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

  
  list = list( result_df_PS= result_df_PS,  result_df_BS =  result_df_BS  )
  return(list)

}




# df_1sim <- hpc_f_2m(i,sim_list,seed_vec, n_patient_vector, p_loe_max, z_l_loe,  z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list,  sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae,  prob_ae,  reference_id, plot_po, up_good,  threshold, delta_adjustment_in, covariate_df)
# list(df_1sim)



 
 