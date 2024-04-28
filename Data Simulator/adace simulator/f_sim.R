# f_sim:  Function to simulate a longitudinal data (similar to the AdACE-simulator code)  
# Outcome Y0,Y1 are only available at VISIT 3, simulated using parameter TrtEff_XXX as the true causal effect of each stratum.
# TrtEff_adhnei   :=  E(Y1 - Y0 | A0==0 & A1==0)
# TrtEff_adhboth  :=  E(Y1 - Y1 | A0==1 & A1==1)
# TrtEff_adhact   :=  E(Y1 - Y0 | A0==0 & A1==1)
# TrtEff_adhpbo   :=  E(Y1 - Y0 | A0==1 & A1==0)

library(dplyr)

f_U = function(A0,A1){
  if(A0==0 & A1==0){U='NeverAdhere/11/D'}else
    if(A0==1 & A1==1){U='AlwaysAdhere/00/C'}else
      if(A0==0 & A1==1){U='AdhereToACT/10/A'}else
        if(A0==1 & A1==0){U='AdhereToPBO/01/N'}
  return(U)
}

f_sim = function(seed,n,alpha1,alpha2,alpha3,beta,gamma1,gamma2,gamma3,TrtEff_adhnei,TrtEff_adhboth,TrtEff_adhact,TrtEff_adhpbo){  #  n=300; alpha1 = c(2.3,2.3, -0.3);alpha2 = c(2.3,2.3, -0.3);  alpha3 = c(2.3,2.3, -0.3);beta   = c(-3, -3,-0.3, 0.2,0.4,0.7);TrtEff_true = 2;gamma1 = c(2, -3, -1, -3);gamma2 = c(2, -3, -1, -2); gamma3 = c(2, -3, -1, -2) 
  
  set.seed(seed)
  
  #---------------
  # data prep
  #---------------
  {
    X_1       = rnorm(n)
    X_2       = rbinom(n,1,.5)
    TRT       = rbinom(n,1,.5)
    USUBJID   = seq(1,n,1)
    
    # Z ~ X  [through alpha]
    # Note:  Z_1 later will be defined as 'BASE'.
    
    Z0_1    = alpha1[1] +  X_1 *alpha1[2] +  X_2 *alpha1[3]  + rnorm(n)
    Z0_2    = alpha2[1] +  X_1 *alpha2[2] +  X_2 *alpha2[3]  + rnorm(n)
    Z0_3    = alpha3[1] +  X_1 *alpha3[2] +  X_2 *alpha3[3]  + rnorm(n)
    Z1_1    = alpha1[1] + 1.5*(  X_1 *alpha1[2] +  X_2 *alpha1[3] ) + rnorm(n)
    Z1_2    = alpha2[1] + 1.5*(  X_1 *alpha2[2] +  X_2 *alpha2[3] ) + rnorm(n)
    Z1_3    = alpha3[1] + 1.5*(  X_1 *alpha3[2] +  X_2 *alpha3[3] ) + rnorm(n)
    Z_1     =  Z1_1  *  TRT  +  Z0_1  * (1- TRT )
    Z_2     =  Z1_2  *  TRT  +  Z0_2  * (1- TRT )
    Z_3     =  Z1_3  *  TRT  +  Z0_3  * (1- TRT )
    Z       = list(Z_1 , Z_2 , Z_3 )
    
    
    # A ~ X, Z [through gamma]
    # Note: only the 3rd visit's adhererance will be used -- so only p_XX_3 matters.
    #       for p_A0_3, Z0_3 is replaced with Z0_1;
    #       for p_A1_3, Z1_3 is replaced with Z1_1.
    #       This way, the Adherance(or ICE) at visit 3 are only determined by X_1, X_2, BASE (Z_1), which matches JAGS modeling.
    
    p_A0_1  = 1 / ( 1 + exp(-(gamma1[1] + (  X_1 *gamma1[2] +  X_2 *gamma1[3]  ) +  Z0_1  * gamma1[1])) ) 
    p_A0_2  = 1 / ( 1 + exp(-(gamma2[1] + (  X_1 *gamma2[2] +  X_2 *gamma2[3]  ) +  Z0_2  * gamma2[1])) )
    p_A0_3  = 1 / ( 1 + exp(-(gamma3[1] + (  X_1 *gamma3[2] +  X_2 *gamma3[3]  ) +  Z0_1  * gamma3[1])) )
    p_A1_1  = 1 / ( 1 + exp(-(gamma1[1] + (  X_1 *gamma1[2] +  X_2 *gamma1[3]  ) +  Z1_1  * gamma1[1])) )
    p_A1_2  = 1 / ( 1 + exp(-(gamma2[1] + (  X_1 *gamma2[2] +  X_2 *gamma2[3]  ) +  Z1_2  * gamma2[1])) ) 
    p_A1_3  = 1 / ( 1 + exp(-(gamma3[1] + (  X_1 *gamma3[2] +  X_2 *gamma3[3]  ) +  Z1_1  * gamma3[1])) )
    A0_1    = rbinom(n, size = 1, prob =  p_A0_1   )          
    A0_2    = rbinom(n, size = 1, prob =  p_A0_2   ) *  A0_1     
    A0_3    = rbinom(n, size = 1, prob =  p_A0_3   ) *  A0_2     
    A1_1    = rbinom(n, size = 1, prob =  p_A1_1   )
    A1_2    = rbinom(n, size = 1, prob =  p_A1_2   ) *  A1_1 
    A1_3    = rbinom(n, size = 1, prob =  p_A1_3   ) *  A1_2 
    A_1     =  A1_1  *  TRT  +  A0_1  * (1 -  TRT )
    A_2     =  A1_2  *  TRT  +  A0_2  * (1 -  TRT )
    A_3     =  A1_3  *  TRT  +  A0_3  * (1 -  TRT )
    A       = cbind(A_1, A_2, A_3)
    
    # Y   ~ X, Z  [through beta]
    
    U_1 = rep(NA,n) ; 
    U_2 = rep(NA,n) ;
    U_3 = rep(NA,n) ;
    Y0  = rep(NA,n) ;  # only calculated for visit 3, based on U_3
    Y1  = rep(NA,n) ;  # only calculated for visit 3, based on U_3
    Y   = rep(NA,n) ;  # only calculated for visit 3, based on U_3
    d   = rep(NA,n) ;  # only calculated for visit 3
    
    for(i in 1:n){
      
      U_1[i] = f_U( A0_1[i] , A1_1[i] )
      U_2[i] = f_U( A0_2[i] , A1_2[i] )
      U_3[i] = f_U( A0_3[i] , A1_3[i] )
      
      if( U_3[i] == 'NeverAdhere/11/D'){   
        
        Y0[i]  = beta[1] + (  X_1[i] *beta[2] +   X_2[i] *beta[3] ) +  Z0_1[i]  * beta[4] +   Z0_2[i]  * beta[5] +  Z0_3[i] * beta[6]  +  rnorm(1, mean = 0, sd = 0.3)
        Y1[i]  = beta[1] + (  X_1[i] *beta[2] +   X_2[i] *beta[3] ) +  Z1_1[i]  * beta[4] +   Z1_2[i]  * beta[5] +  Z1_3[i] * beta[6]  +  rnorm(1, mean = 0, sd = 0.3)  + TrtEff_adhnei                                         
        Y[i]   =  Y1[i] *  TRT[i] +  Y0[i] * (1 -  TRT[i])
        d[i]   =  Y1[i] -  Y0[i]
        
      }else if( U_3[i] == 'AlwaysAdhere/00/C'){
        
        Y0[i]  = beta[1] + (   X_1[i] *beta[2] +   X_2[i] *beta[3] ) +  Z0_1[i]  * beta[4] +   Z0_2[i]  * beta[5] +  Z0_3[i]  * beta[6]  +  rnorm(1, mean = 0, sd = 0.3)
        Y1[i]  = beta[1] + (   X_1[i] *beta[2] +   X_2[i] *beta[3] ) +  Z1_1[i]  * beta[4] +   Z1_2[i]  * beta[5] +  Z1_3[i]  * beta[6]  +  rnorm(1, mean = 0, sd = 0.3)  + TrtEff_adhboth                                         
        Y[i]   =  Y1[i] *  TRT[i] +  Y0[i] * (1 -  TRT[i])
        d[i]   =  Y1[i] -  Y0[i]
        
      }else if ( U_3[i] == 'AdhereToACT/10/A'){
        
        Y0[i]  = beta[1] + (   X_1[i] *beta[2] +   X_2[i] *beta[3] ) +  Z0_1[i]  * beta[4] +  Z0_2[i]  * beta[5] +  Z0_3[i]  * beta[6]  +  rnorm(1, mean = 0, sd = 0.3)
        Y1[i]  = beta[1] + (   X_1[i] *beta[2] +   X_2[i] *beta[3] ) +  Z1_1[i]  * beta[4] +  Z1_2[i]  * beta[5] +  Z1_3[i]  * beta[6]  +  rnorm(1, mean = 0, sd = 0.3)  + TrtEff_adhact                                          
        Y[i]   =  Y1[i] *  TRT[i] +  Y0[i] * (1 -  TRT[i])
        d[i]   =  Y1[i] -  Y0[i]
        
      }else if( U_3[i] == 'AdhereToPBO/01/N' ){
        
        Y0[i]  = beta[1] + (   X_1[i] *beta[2] +   X_2[i] *beta[3] ) +  Z0_1[i]  * beta[4] +  Z0_2[i]  * beta[5] +  Z0_3[i]  * beta[6]  +  rnorm(1, mean = 0, sd = 0.3)
        Y1[i]  = beta[1] + (   X_1[i] *beta[2] +   X_2[i] *beta[3] ) +  Z1_1[i]  * beta[4] +  Z1_2[i]  * beta[5] +  Z1_3[i]  * beta[6]  +  rnorm(1, mean = 0, sd = 0.3)  + TrtEff_adhpbo                                          
        Y[i]   =  Y1[i] *  TRT[i] +  Y0[i] * (1 -  TRT[i])
        d[i]   =  Y1[i] -  Y0[i]
        
      }
      
    }
    
  }
  
  #---------------
  # full_wide
  #---------------
  { 
    full_wide   = cbind.data.frame( USUBJID,X_1,X_2,TRT, 
                                    Y0,Y1,Y,d,
                                    Z0_1,Z1_1,Z_1,
                                    Z0_2,Z1_2,Z_2,
                                    Z0_3,Z1_3,Z_3,
                                    A0_1,A1_1,A_1,
                                    A0_2,A1_2,A_2,
                                    A0_3,A1_3,A_3) %>% 
      mutate( U_1 = U_1,U_2 = U_2 ,U_3 = U_3 )
  }
  
  #---------------
  # full_long
  #---------------
  {
    X_cov           = full_wide %>% select(USUBJID,X_1,X_2,TRT,Z_1) %>% rename(BASE = Z_1)
    full_long       = cbind.data.frame( USUBJID  = rep(1:n, 3), 
                                        AVISITN  = rep(1:3, each=n), 
                                        Y0       =   c( rep(NA,2*nrow(full_wide)), full_wide %>% pull(Y0)), 
                                        Y1       =   c( rep(NA,2*nrow(full_wide)), full_wide %>% pull(Y1)), 
                                        Y        =   c( rep(NA,2*nrow(full_wide)), full_wide %>% pull(Y)),
                                        d        =   c( rep(NA,2*nrow(full_wide)), full_wide %>% pull(d)),  
                                        ICE      = 1-c( full_wide %>% pull(A_1), full_wide %>% pull(A_2), full_wide %>% pull(A_3)),
                                        U        =   c( full_wide %>% pull(U_1), full_wide %>% pull(U_2), full_wide %>% pull(U_3))
    ) %>% 
      left_join(X_cov, by=c("USUBJID")) %>% 
      arrange(USUBJID) 
    
  }
  
  #---------------
  # obs_long
  #---------------
  
  obs_long       = full_long %>% mutate(Y = ifelse(ICE == 1, NA, Y)) %>%  select(-c(Y0,Y1,U,d))
  
  #---------------
  # true ACE at Tm
  #---------------
  {
    true_d_Tm_adhact  = mean( full_long %>% filter(AVISITN==3 & U=='AdhereToACT/10/A') %>% pull(d) )
    true_d_Tm_adhpbo  = mean( full_long %>% filter(AVISITN==3 & U=='AdhereToPBO/01/N') %>% pull(d) )
    true_d_Tm_adhboth = mean( full_long %>% filter(AVISITN==3 & U=='AlwaysAdhere/00/C') %>% pull(d) )
    true_d_Tm_adhnei  = mean( full_long %>% filter(AVISITN==3 & U=='NeverAdhere/11/D') %>% pull(d) )
    true_d_Tm         = list( true_d_Tm_adhact  = true_d_Tm_adhact ,
                              true_d_Tm_adhpbo  = true_d_Tm_adhpbo ,
                              true_d_Tm_adhboth = true_d_Tm_adhboth,
                              true_d_Tm_adhnei  = true_d_Tm_adhnei )
  }   
  
  {
    true_d_Tm_adhact_nsl  = mean( full_long %>% filter(AVISITN==3 & U=='AdhereToACT/10/A' & TRT==1) %>% pull(Y) )-
                            mean( full_long %>% filter(AVISITN==3 & U=='AdhereToACT/10/A' & TRT==0) %>% pull(Y) )
    true_d_Tm_adhpbo_nsl  = mean( full_long %>% filter(AVISITN==3 & U=='AdhereToPBO/01/N' & TRT==1) %>% pull(Y) )-
                            mean( full_long %>% filter(AVISITN==3 & U=='AdhereToPBO/01/N' & TRT==0) %>% pull(Y) )
    true_d_Tm_adhboth_nsl = mean( full_long %>% filter(AVISITN==3 & U=='AlwaysAdhere/00/C' & TRT==1) %>% pull(Y) )-
                            mean( full_long %>% filter(AVISITN==3 & U=='AlwaysAdhere/00/C' & TRT==0) %>% pull(Y))
    true_d_Tm_adhnei_nsl  = mean( full_long %>% filter(AVISITN==3 & U=='NeverAdhere/11/D' & TRT==1) %>% pull(Y) )-
                            mean( full_long %>% filter(AVISITN==3 & U=='NeverAdhere/11/D' & TRT==0) %>% pull(Y) )
    true_d_Tm_nsl         = list( true_d_Tm_adhact_nsl  = true_d_Tm_adhact_nsl ,
                                  true_d_Tm_adhpbo_nsl  = true_d_Tm_adhpbo_nsl ,
                                  true_d_Tm_adhboth_nsl = true_d_Tm_adhboth_nsl,
                                  true_d_Tm_adhnei_nsl  = true_d_Tm_adhnei_nsl )
  } 
  
  
  return(list(full_wide   = full_wide,
              full_long   = full_long,
              obs_long    = obs_long,
              true_d_Tm   = true_d_Tm,
              true_d_Tm_nsl=true_d_Tm_nsl,
              Z_forAdACE   = Z,
              A_forAdACE   = A)
  )
  
  
}  


 