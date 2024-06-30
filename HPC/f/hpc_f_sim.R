nSim           = 2                                                                          # number of simulated trials
n_patient_ctrl = 200
n_patient_expt = 200

source("Data Simulator/cities simulator/f_sim.R")                                     # cities simulator
source("Data Simulator/cities simulator/scenarios/trt_large_scen_B.R")             # cities simulator: large  trt effect,  scenario B

library(cities)
library(tidyverse)

sim_list = c()
for(i in 1: nSim){
  
  sim          =  f_sim(seed_val <- seed_vec[i], n_patient_vector, p_loe_max, z_l_loe,  z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list,  sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae,  prob_ae,  reference_id, plot_po, up_good,  threshold, delta_adjustment_in, covariate_df) 
  sim_list[[i]] =  sim  
           
}

save(sim_list, file="HPC/sim_list/sim_list.Rdata")
# load(file = "Data Simulator/cities simulator/sim_list.Rdata") # loads result
 
