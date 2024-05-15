 

# result_df_BS = read.csv("Simulation Study/WIP - Bing/resultprint/cities simulator/BS_citiesimulator_trt_large_scen_B.csv",header = T)
# result_df_PS = read.csv("Simulation Study/WIP - Bing/resultprint/cities simulator/PS_citiesimulator_trt_large_scen_B.csv",header = T)
# result_df_AD = read.csv("Simulation Study/WIP - Bing/resultprint/cities simulator/AD_citiesimulator_trt_large_scen_B.csv",header = T)
# BS = result_df_BS 
# PS = result_df_PS
# AD = result_df_AD 
f_cat3=function(mean,se){show=paste0( round(mean,3), '(', round(se,3), ")") ; return(show)}
f_comparison_table = function(AD,BS,PS){
  
    ##########
    # ADACE
    ##########
    
    #---------
    # s*+  
    #---------
    
    # TRUE
    H4 <- round( mean(AD$trued_sp),3); # mean(BS$trued_IB_sp);mean(PS$trued_AC_sp);mean(PS$trued_AC_sp_nsl)  #H4
    
    # ACE
    H9  <- f_cat3( mean(AD$adace_mean_starplusA), mean(AD$adace_se_starplusA))   #H9
    H10 <- f_cat3( mean(AD$adace_mean_starplusB), mean(AD$adace_se_starplusB))   #H10
     
    #---------
    # s++  
    #---------
    
    # TRUE
    F4 <- round( mean(AD$trued_00),3);#mean(BS$trued_I_00);mean(PS$trued_C_00)   #F4
    
    # ACE
    F9  <- f_cat3( mean(AD$adace_mean_plusplusA), mean(AD$adace_se_plusplusA))   #F9
    F10 <- f_cat3( mean(AD$adace_mean_plusplusB), mean(AD$adace_se_plusplusB))   #F10
    
    
    #############
    # BAYESIAN
    #############
    
    #---------
    # 01 (H)
    #---------
    
    # TRUE
    D4 <- round( mean(BS$trued_H_01),3);# mean(AD$trued_01);mean(PS$trued_N_01)   # D4
    
    # ACE
    D7 <- f_cat3( mean(BS$ITT_H), mean(BS$ITT_se_H) )       # D7
    D8 <- f_cat3( mean(BS$delta_H), mean(BS$delta_se_H))     # D8
    
    #---------
    # 11 (D)
    #---------
    
    # TRUE
    E4 <- round( mean(BS$trued_D_11),3)# mean(AD$trued_11);  # E4
    
    # ACE
    E7 <- f_cat3( mean(BS$ITT_D),mean(BS$ITT_se_D))        # E7
    E8 <- f_cat3( mean(BS$delta_D),mean(BS$delta_se_D))      # E8
    
    #---------
    # 10 (B)
    #---------
    
    # TRUE
    G4 <- round( mean(BS$trued_B_10),3) # mean(AD$trued_10);  # G4
    
    # ACE
    G7 <- f_cat3( mean(BS$ITT_B),mean(BS$ITT_se_B))         # G7
    G8 <- f_cat3( mean(BS$delta_B),mean(BS$delta_se_B))      # G8
    
    #---------
    # 00 (I)
    #---------
    
    # TRUE
    F4 <- round( mean(BS$trued_I_00),3) # mean(AD$trued_00);  # F4
    
    # ACE
    F7 <- f_cat3( mean(BS$ITT_I),mean(BS$ITT_se_I))         # F7
    F8 <- f_cat3( mean(BS$delta_I),mean(BS$delta_se_I))      # F8
    
    #---------
    # *0 (IB)
    #---------
    
    # TRUE
    H4 <- round( mean(BS$trued_IB_sp),3) # mean(AD$trued_sp);  # H4
    
    # ACE
    H7 <- f_cat3( mean(BS$ITT_IB),mean(BS$ITT_se_IB))         # H7
    H8 <- f_cat3( mean(BS$delta_IB),mean(BS$delta_se_IB))      # H8
    
    
    ##########
    # PS
    ##########
    
    #---------
    # 01 (N)
    #---------
    
    # TRUE
    D4 <- round( mean(PS$trued_N_01),3) ;# mean(PS$trued_N_01_nsl);# mean(AD$trued_01);mean(BS$trued_H_01)   # D4
    
    # ACE
    D11 <- f_cat3(  mean(PS$NACE),mean(PS$NACE_se))             # D11
    D12 <- f_cat3( mean(PS$NACE.adj),mean(PS$NACE.adj_se))      # D12
    
    #---------
    # 11 (excluded)
    #---------
    
    
    
    #---------
    # 10 (A)
    #---------
    
    # TRUE
    G4 <- round( mean(PS$trued_A_10),3) ; #mean(PS$trued_A_10_nsl); # mean(AD$trued_10);mean(BS$trued_B_10);  # G4
    
    # ACE  
    G11 <- f_cat3( mean(PS$AACE),mean(PS$AACE_se))             # G11
    G12 <- f_cat3( mean(PS$AACE.adj),mean(PS$AACE.adj_se))      # G12
    
    #---------
    # 00 (C)
    #---------
    
    # TRUE
    F4 <- round( mean(PS$trued_C_00),3) ; #mean(PS$trued_C_00_nsl);# mean(AD$trued_00);  # F4
    
    # ACE
    F11 <- f_cat3( mean(PS$CACE),mean(PS$CACE_se))             # F11
    F12 <- f_cat3( mean(PS$CACE.adj),mean(PS$CACE.adj_se))      # F12
    
    #---------
    # *0 (AC)
    #---------
    
    # TRUE
    H4 <- round( mean(PS$trued_AC_sp),3) ; #mean(PS$trued_AC_sp_nsl) #mean(BS$trued_IB_sp);# mean(AD$trued_sp);  # H4
    
    # ACE
    H11 <- f_cat3( mean(PS$CAACE),mean(PS$CAACE_se))             # H11
    H12 <- f_cat3( mean(PS$CAACE.adj),mean(PS$CAACE.adj_se))      # H12
    
    
    D9=D10=E9=E10=G9=G10=E11=E12=NA
    D5=D6=E5=E6=F5=F6=G5=G6=H5=H6=NA
    
    #####################
    # comparison tb
    #####################
    
    tb = as.data.frame(
      matrix(c(D4,D5,D6,D7,D8,D9,D10,D11,D12,
               E4,E5,E6,E7,E8,E9,E10,E11,E12,
               F4,F5,F6,F7,F8,F9,F10,F11,F12,
               G4,G5,G6,G7,G8,G9,G10,G11,G12,
               H4,H5,H6,H7,H8,H9,H10,H11,H12),nrow=9,ncol=5,byrow = F)
    )%>% rename('D'='V1','E'='V2','F'='V3','G'='V4','H'='V5') 
    
    
    return(tb)

}
