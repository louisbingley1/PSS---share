
data_generator_loop1 = function (n_patient_vector, p_loe_max, z_l_loe, z_u_loe, p_ee_max, 
                                 z_l_ee, z_u_ee, timepoints, pacf_list, sigma_ar_vec, mean_list, 
                                 beta_list, p_admin, rate_dc_ae, prob_ae, seed_val, reference_id, 
                                 plot_po = FALSE, up_good, threshold, total_data, delta_adjustment_in, 
                                 covariate_df){
  
  `%notin%` = Negate(`%in%`)
  
  if(total_data==1){
    #----------------------------
    # Simulate 1 Dataset
    #----------------------------
    
    data_out = data_generator1(n_patient_vector, p_loe_max, z_l_loe, 
                               z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list, 
                               sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae, 
                               prob_ae, seed_val, reference_id, plot_po = FALSE, up_good, 
                               threshold, delta_adjustment_in, covariate_df)
  }else{
    #----------------------------
    # Simulate Multiple Datasets
    #----------------------------
    
    pb       = txtProgressBar(min = (seed_val), max = (seed_val +  total_data - 1), style = 3, width = 50, char = "=")
    m        = ifelse(length(delta_adjustment_in) == 0 | is.na(sum(delta_adjustment_in)),  3, 4)
    
    for (seed_val in (seed_val):(seed_val + total_data -   1)) {               #  (seed_val + 1):(seed_val + total_data -   1
      
      data_temp = data_generator1(n_patient_vector, p_loe_max, 
                                  z_l_loe, z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, 
                                  pacf_list, sigma_ar_vec, mean_list, beta_list, 
                                  p_admin, rate_dc_ae, prob_ae, seed_val, reference_id, 
                                  plot_po = FALSE, up_good, threshold, delta_adjustment_in, 
                                  covariate_df)
      
      for (i in 1:(length(data_out) - m)) {
        for (j in 1:length(data_out[[i]])) {
          for (k in 1:length(data_out[[i]][[j]])) {
            data_out[[i]][[j]][[k]] = rbind(data_out[[i]][[j]][[k]],    data_temp[[i]][[j]][[k]])
          }
        }
      }
      data_out$observed_df = rbind(data_out$observed_df,  data_temp$observed_df)
      data_out$po_df = rbind(data_out$po_df, data_temp$po_df)
      setTxtProgressBar(pb, seed_val)
      
    }
  }
  return(data_out)
}
