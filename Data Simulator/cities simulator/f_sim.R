library(cities)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(ggthemes)
source("Data Simulator/cities simulator/utility functions/splpl_utilities_BL_1.R")
source("Data Simulator/cities simulator/utility functions/f_nsim_true.R")
source("Data Simulator/cities simulator/utility functions/f_U.R")


f_sim = function(seed_val ,
                 n_patient_vector, p_loe_max, z_l_loe,  z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list,  sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae,  prob_ae,  reference_id, plot_po, up_good,  threshold, delta_adjustment_in, covariate_df){
    
  
# original  

data_out      = data_generator1(n_patient_vector, p_loe_max, z_l_loe,  z_u_loe, p_ee_max, z_l_ee, z_u_ee, timepoints, pacf_list,  sigma_ar_vec, mean_list, beta_list, p_admin, rate_dc_ae,  prob_ae, seed_val, reference_id, plot_po, up_good,  threshold, delta_adjustment_in, covariate_df)
 

po_mar        = data_out$po_df %>% 
                        dplyr::select(seed, subject, arm, timepoints, aval) %>%
                        rename(aval_mar = aval)                # in po_df: aval is mar?
observed_mar  = data_out$observed_df  %>%      
                        left_join(po_mar, by=c("seed", "subject", "arm", "timepoints")) %>%
                        rename(aval_mnar = aval)               # in observed_df: aval is mNar ?
observed_out  = observed_mar %>%
                    ungroup() %>%
                    tidyr::complete(seed, subject, timepoints) %>%           # COMPLETE EMPTY LINES - SUBJECT 4, TIME 4
                    ungroup() %>%
                    group_by(seed, subject, arm) %>%
                    dplyr::select(-aval_mnar, -aval_mar) %>%   
                    zoo::na.locf() %>%                                       # COMPLETE EMPTY LINES - SUBJECT 4, TIME 4
                    left_join(observed_mar  %>% dplyr::select(-dc_loe, -dc_admin, -dc_ee, -dc_ae, -continuous, -binary, -base),              by=c("seed", "subject", "arm", "timepoints"))  %>%
                    left_join(data_out$po_df%>% dplyr::select(-dc_loe, -dc_admin, -dc_ee, -dc_ae, -continuous, -binary, -base, -observed),   by=c("seed", "subject", "arm", "timepoints")) 

# update observed_out: Add DC indicator

observed_out_1  = observed_out   %>% mutate(DC = ifelse( dc_loe+dc_ee+dc_admin+dc_ae > 0 ,1 ,0))

# update po_df : two arms on the same row

po_df         = data_out$po_df %>% mutate(DC = ifelse( dc_loe+dc_ee+dc_admin+dc_ae > 0 ,1 ,0))
po1           = po_df %>% filter(arm == 1) %>% rename(arm0=arm,Y0=aval,dc0_loe=dc_loe,dc0_ee=dc_ee,dc0_admin=dc_admin,dc0_ae=dc_ae,DC0=DC) 
po2           = po_df %>% filter(arm == 2) %>% rename(arm1=arm,Y1=aval,dc1_loe=dc_loe,dc1_ee=dc_ee,dc1_admin=dc_admin,dc1_ae=dc_ae,DC1=DC)
po_df         = po1 %>% left_join( po2 %>% dplyr::select(-base, -continuous,-binary,-observed ), by=c("seed", "subject",  "timepoints")) 
for(r in 1:nrow(po_df)){ po_df$U[r] = f_U( A0 = 1-po_df$DC0[r] , A1=1-po_df$DC1[r] )}


# merge  observed_out_1 & po_df: 
observed_out  = observed_out_1 %>%  
                    left_join(po_df %>% dplyr::select(-base,-continuous,-binary),by=c("seed","subject","timepoints")) %>%
                    left_join(cbind.data.frame(timepoints=c(12,24,48,55),AVISITN=c(1,2,3,4)),by="timepoints") %>%
                    select(-arm0,-arm1,-timepoints,-observed)%>%
                    mutate(arm = arm-1, d=Y1-Y0) %>%
                    rename(USUBJID = subject, BASE=base, X_1=continuous, X_2=binary, TRT=arm,ICE0=DC0,ICE1=DC1,ICE=DC,Y=aval_mar)
                    
for(r in 1:nrow(observed_out)){observed_out$Utrue[r] = strsplit(observed_out$U[r],"/")[[1]][3] }
 
# create adhereance matrix A for ADACE    
# observed_out_=observed_out_1[1:20,]
A_1 = 1- observed_out %>% filter(AVISITN==1) %>% pull(ICE)
A_2 = 1- observed_out %>% filter(AVISITN==2) %>% pull(ICE)
A_3 = 1- observed_out %>% filter(AVISITN==3) %>% pull(ICE)
A_4 = 1- observed_out %>% filter(AVISITN==4) %>% pull(ICE)
A   = cbind(A_1, A_2, A_3,A_4)

# create list Z = list(BASE, Y1,Y2,Y3) for ADACE
#Z_1 = rep(NA,nrow(observed_out  %>% filter(AVISITN==1) ))
Z_1 = observed_out %>% filter(AVISITN==1) %>% pull(BASE)
Z_2 = observed_out %>% filter(AVISITN==1) %>% pull(Y)
Z_3 = observed_out %>% filter(AVISITN==2) %>% pull(Y)
Z_4 = observed_out %>% filter(AVISITN==3) %>% pull(Y)
Z   = list(Z_1,Z_2,Z_3,Z_4)

# create X for ADACE
X   = observed_out %>% filter(AVISITN==1)  %>% ungroup %>% select( X_1,X_2) %>% as.matrix()
#X   = observed_out %>% filter(AVISITN==1)  %>% ungroup %>% select( BASE,X_1,X_2) %>% as.matrix()

#---------------
# true ACE at Tm
#---------------
{
  maxtime   = max(observed_out$AVISITN)
  true_d_Tm_adhact  = mean( observed_out %>% filter(AVISITN==maxtime & U=='AdhereToACT/10/A') %>% pull(d) )
  true_d_Tm_adhpbo  = mean( observed_out %>% filter(AVISITN==maxtime & U=='AdhereToPBO/01/N') %>% pull(d) )
  true_d_Tm_adhboth = mean( observed_out %>% filter(AVISITN==maxtime & U=='AlwaysAdhere/00/C') %>% pull(d) )
  true_d_Tm_adhnei  = mean( observed_out %>% filter(AVISITN==maxtime & U=='NeverAdhere/11/D') %>% pull(d) )
  true_d_Tm_sp      = mean( observed_out %>% filter( AVISITN==maxtime & ((U=='AdhereToACT/10/A')|(U=='AlwaysAdhere/00/C')) ) %>% pull(d) )
  true_d_Tm         = list( true_d_Tm_adhact  = true_d_Tm_adhact ,
                            true_d_Tm_adhpbo  = true_d_Tm_adhpbo ,
                            true_d_Tm_adhboth = true_d_Tm_adhboth,
                            true_d_Tm_adhnei  = true_d_Tm_adhnei,
                            true_d_Tm_sp      = true_d_Tm_sp )
}   

{
  true_d_Tm_adhact_nsl  = mean( observed_out %>% filter(AVISITN==maxtime & U=='AdhereToACT/10/A' & TRT==1) %>% pull(Y) )- mean( observed_out %>% filter(AVISITN==maxtime & U=='AdhereToACT/10/A' & TRT==0) %>% pull(Y) )
  true_d_Tm_adhpbo_nsl  = mean( observed_out %>% filter(AVISITN==maxtime & U=='AdhereToPBO/01/N' & TRT==1) %>% pull(Y) )- mean( observed_out %>% filter(AVISITN==maxtime & U=='AdhereToPBO/01/N' & TRT==0) %>% pull(Y) )
  true_d_Tm_adhboth_nsl = mean( observed_out %>% filter(AVISITN==maxtime & U=='AlwaysAdhere/00/C' & TRT==1) %>% pull(Y) )- mean( observed_out %>% filter(AVISITN==maxtime & U=='AlwaysAdhere/00/C' & TRT==0) %>% pull(Y))
  true_d_Tm_adhnei_nsl  = mean( observed_out %>% filter(AVISITN==maxtime & U=='NeverAdhere/11/D' & TRT==1) %>% pull(Y) )-  mean( observed_out %>% filter(AVISITN==maxtime & U=='NeverAdhere/11/D' & TRT==0) %>% pull(Y) )
  true_d_Tm_sp_nsl      = mean( observed_out %>% filter( AVISITN==maxtime & ((U=='AdhereToACT/10/A')|(U=='AlwaysAdhere/00/C')) & TRT==1) %>% pull(Y) )-  mean( observed_out %>% filter(AVISITN==maxtime & ((U=='AdhereToACT/10/A')|(U=='AlwaysAdhere/00/C')) & TRT==0) %>% pull(Y) )
  true_d_Tm_nsl         = list( true_d_Tm_adhact_nsl  = true_d_Tm_adhact_nsl ,
                                true_d_Tm_adhpbo_nsl  = true_d_Tm_adhpbo_nsl ,
                                true_d_Tm_adhboth_nsl = true_d_Tm_adhboth_nsl,
                                true_d_Tm_adhnei_nsl  = true_d_Tm_adhnei_nsl ,
                                true_d_Tm_sp_nsl      = true_d_Tm_sp_nsl)
  
} 



return(list(observed_out  = observed_out, 
            true_d_Tm     = true_d_Tm,
            true_d_Tm_nsl = true_d_Tm_nsl,
            Z_forAdACE    = Z,
            A_forAdACE    = A,
            X_forAdACE    = X))


}


  

#-------------------
# nsim true ace & dc
#-------------------

# nsim_true_list      = f_nsim_true(total_data )
# nsim_true_ace_table = nsim_true_list$true_ace_table
# nsim_dc_out         = nsim_true_list$dc_out
