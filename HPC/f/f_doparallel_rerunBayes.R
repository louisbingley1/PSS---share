
f_1sim = function(i,seed_vec, n_patient_vector, p_loe_max, z_l_loe,  z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list,  sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae,  prob_ae,  reference_id, plot_po, up_good,  threshold, delta_adjustment_in, covariate_df){

  
  result_df_BS           = NULL
  result_df_AD           = NULL
  result_df_PS           = NULL  
  
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
 
  
  list = list( result_df_BS =  result_df_BS)
  return(list)

}




# df_1sim <- f_1sim(seed_vec, n_patient_vector, p_loe_max, z_l_loe,  z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list,  sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae,  prob_ae,  reference_id, plot_po, up_good,  threshold, delta_adjustment_in, covariate_df)
# list(df_1sim)



 
 