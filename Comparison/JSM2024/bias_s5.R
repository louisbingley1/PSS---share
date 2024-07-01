f_bias_v= function(true_v, est_v){ bias_v = est_v-true_v;  return(bias_v) }

  #--------------
  # BS
  #--------------

  # S01/H
  BS_S01_bias_v = f_bias_v(true_v = s5_BS$trued_H_01, est_v = s5_BS$delta_H )
  
  # 10/B
  BS_S10_bias_v = f_bias_v(true_v = s5_BS$trued_B_10, est_v = s5_BS$delta_B )
  
  # 00/I
  BS_S00_bias_v = f_bias_v(true_v = s5_BS$trued_I_00, est_v = s5_BS$delta_I )
  
  # 11/D
  BS_S11_bias_v = f_bias_v(true_v = s5_BS$trued_D_11, est_v = s5_BS$delta_D )
  
  #  *0/IB
  BS_Ss0_bias_v = f_bias_v(true_v = s5_BS$trued_IB_sp, est_v =s5_BS$delta_IB)
  
 
  #--------------
  # AD
  #--------------
  
   
  # 00/++
  AD_S00_bias_v = f_bias_v(true_v =  s5_AD$trued_00, est_v = s5_AD$adace_mean_plusplusA)
   
  # *0/*+
  AD_Ss0_bias_v = f_bias_v(true_v =  s5_AD$trued_sp, est_v = s5_AD$adace_mean_starplusA)
   
  #--------------
  # PS
  #--------------
   
  # 01/N
  PS1_S01_bias_v = f_bias_v(true_v = s5_PS$trued_N_01, est_v = s5_PS$NACE )
  PS2_S01_bias_v = f_bias_v(true_v = s5_PS$trued_N_01, est_v = s5_PS$NACE.adj )
  
  # 10/A
  PS1_S10_bias_v = f_bias_v(true_v = s5_PS$trued_A_10, est_v = s5_PS$AACE)
  PS2_S10_bias_v = f_bias_v(true_v = s5_PS$trued_A_10, est_v = s5_PS$AACE.adj )
  
  # 00/C
  PS1_S00_bias_v = f_bias_v(true_v = s5_PS$trued_C_00, est_v = s5_PS$CACE )
  PS2_S00_bias_v = f_bias_v(true_v = s5_PS$trued_C_00, est_v = s5_PS$CACE.adj )
  
  # *0/AC
  PS1_Ss0_bias_v = f_bias_v(true_v = s5_PS$trued_AC_sp, est_v = s5_PS$CAACE)
  PS2_Ss0_bias_v = f_bias_v(true_v = s5_PS$trued_AC_sp, est_v = s5_PS$CAACE.adj)
  

  
  df_5 = rbind.data.frame(  cbind.data.frame(bias = BS_S01_bias_v, strata ="Strata 01", method = "Bayesian"   ,scenario = "s5"),
                            cbind.data.frame(bias = BS_S10_bias_v, strata ="Strata 10", method = "Bayesian"   ,scenario = "s5"),
                            cbind.data.frame(bias = BS_S00_bias_v, strata ="Strata 00", method = "Bayesian"   ,scenario = "s5"),
                            cbind.data.frame(bias = BS_S11_bias_v, strata ="Strata 11", method = "Bayesian"   ,scenario = "s5"),
                            cbind.data.frame(bias = BS_Ss0_bias_v, strata ="Strata *0", method = "Bayesian"   ,scenario = "s5"),
                            cbind.data.frame(bias = AD_S00_bias_v, strata ="Strata 00", method = "AdACE"      ,scenario = "s5"),
                            cbind.data.frame(bias = AD_Ss0_bias_v, strata = "Strata *0",method = "AdACE"      ,scenario = "s5"),
                            cbind.data.frame(bias = PS1_S00_bias_v,strata = "Strata 00",method = "PS"         ,scenario = "s5"),
                            cbind.data.frame(bias = PS2_S00_bias_v,strata = "Strata 00",method = "PS(Cov-adj)",scenario = "s5"),
                            cbind.data.frame(bias = PS1_S01_bias_v,strata = "Strata 01",method = "PS"         ,scenario = "s5"),
                            cbind.data.frame(bias = PS2_S01_bias_v,strata = "Strata 01",method = "PS(Cov-adj)",scenario = "s5"),
                            cbind.data.frame(bias = PS1_S10_bias_v,strata = "Strata 10",method = "PS"         ,scenario = "s5"),
                            cbind.data.frame(bias = PS2_S10_bias_v,strata = "Strata 10",method = "PS(Cov-adj)",scenario = "s5"),
                            cbind.data.frame(bias = PS1_Ss0_bias_v,strata = "Strata *0",method = "PS"         ,scenario = "s5"),
                            cbind.data.frame(bias = PS2_Ss0_bias_v,strata = "Strata *0",method = "PS(Cov-adj)",scenario = "s5"))
  
  library(ggplot2)
  
  ggplot(df_5,aes(strata,bias))+
    geom_boxplot(aes(fill=method))+
    ylab("bias (Scenario 5)")
