#---------------------------------
# Asymptotic Estimate of Estimand  --> True ace
#---------------------------------

f_nsim_true = function(total_data){
  data_out = data_generator_loop1(n_patient_vector,
                                  p_loe_max, 
                                  z_l_loe,
                                  z_u_loe,
                                  p_ee_max,
                                  z_l_ee,
                                  z_u_ee,
                                  timepoints,
                                  pacf_list,
                                  sigma_ar_vec,
                                  mean_list,
                                  beta_list,
                                  p_admin,
                                  rate_dc_ae,
                                  prob_ae,
                                  starting_seed_val,
                                  reference_id, 
                                  plot_po,
                                  up_good,
                                  threshold,
                                  total_data,
                                  delta_adjustment_in,
                                  covariate_df)
  
  estimates_out = plot_estimates(data_out = data_out,  
                                 total_data = total_data,
                                 timepoints = timepoints,
                                 reference_id = reference_id,
                                 IR_display = IR_display,
                                 normal_output = TRUE,
                                 static_output = TRUE)
  
  # True ACE 
  true_ace_table = estimates_out %>%
    mutate(mean_se = paste0(mean, " (", round(se, 2) , ")")) %>%
    dplyr::select(-se, -Arm, -mean) %>%
    pivot_wider(names_from = Estimand, values_from = mean_se)
  
  
  
  # True DC
  dc_out = plot_dc(data_out = data_out, 
                   total_data = total_data, 
                   timepoints = timepoints,
                   static_output = TRUE)
  
  dc_out %>% 
    ungroup() %>%
    filter(Timepoints == max(timepoints)) %>%
    select(Arm, Reason, Value) %>%
    pivot_wider(names_from = Arm,
                values_from = Value) %>%
    arrange(factor(Reason, levels = c("AE", "LOE", "EE", "ADMIN", "OVERALL")))
  
  
  # deliver
  result_list = list(true_ace_table = true_ace_table, dc_out=dc_out )
  return(result_list)
  
}