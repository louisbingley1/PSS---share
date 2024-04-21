f_ace_1sim      =  function(postparam,dat,I) {   
 
  niter          = nrow(postparam)
  
  #---------------------------------------------------
  # Extract results from postparam
  #---------------------------------------------------
 { 
  # "S0"
  
  S0_pot_        = postparam[,grep("S0",    names(postparam), value=TRUE)]     # nrows=niter,ncols=n
  col_order      = paste0("S0[",1:nrow(dat),"]")
  S0_pot         = S0_pot_[,col_order]
  S0_pot         = t(S0_pot)
  
  # "S1"
  
  S1_pot_        = postparam[,grep("S1",    names(postparam), value=TRUE)]     # nrows=niter,ncols=n
  col_order      = paste0("S1[",1:nrow(dat),"]")
  S1_pot         = S1_pot_[,col_order]
  S1_pot         = t(S1_pot)
  
  # "Y0"
  
  Y0_pot_        = postparam[,grep("Y0",   names(postparam), value=TRUE)]
  col_order      = paste0("Y0[",1:nrow(dat),"]")
  Y0_pot         = Y0_pot_[,col_order]
  Y0_pot         = t(Y0_pot)
  
  #  "Y1"
  
  Y1_pot_        = postparam[,grep("Y1",   names(postparam), value=TRUE)]
  col_order      = paste0("Y1[",1:nrow(dat),"]")
  Y1_pot         = Y1_pot_[,col_order]
  Y1_pot         = t(Y1_pot)
  
  # "delta"
  
  delta_pot      = postparam[,grep("delta", names(postparam), value=TRUE)]     # nrows=niter,ncols=4  
  
  # "w"
  w             = postparam[,grep("w", names(postparam), value=TRUE)] 
  index_i       = rep(1:nrow(dat_in),each=4)
  index_j       = rep(1:4, nrow(dat_in))
  col_order     = paste0("w[",index_i,",",index_j,"]")
  w             = w[,col_order]
  
  }
  
  #---------------------------------------------------
  # Pull ACE,PCP from all iterations
  #---------------------------------------------------
  
  ITTmean_H  = ITTmean_D  = ITTmean_I  = ITTmean_B  = rep(NA, niter)
  ITTse_H    = ITTse_D    = ITTse_I    = ITTse_B    = rep(NA, niter)
  delta_se_H = delta_se_D = delta_se_I = delta_se_B = rep(NA, niter)
  PCP_H      = PCP_D      = PCP_I      = PCP_B      = rep(NA, niter)
  nH         = nD         = nI         = nB         = rep(NA, niter)
  for (iter in 1:niter){
    
    W = w[iter,] 
    W = matrix(W,nrow=nrow(dat_in),ncol=4,byrow = T) 
    W = as.data.frame(W) 
    colnames(W) <-c("H","D","I","B")
    
    # Create ITT table for 1 iteration
    {
    ITT_tb   =  cbind.data.frame(USUBJID = seq(1,nrow(dat),1), 
                                 S0_pot  = S0_pot[,iter],
                                 S1_pot  = S1_pot[,iter],
                                 Y0_pot  = Y0_pot[,iter],
                                 Y1_pot  = Y1_pot[,iter],
                                 U       = dat$U,
                                 Z       = dat$TRT,
                                 S       = dat$S,
                                 S0      = dat$S0,
                                 S1      = dat$S1,
                                 Y       = dat$Y,
                                 Y0      = dat$Y0,
                                 Y1      = dat$Y1,
                                 I       = I,
                                 W       = W 
                                ) %>% 
                          mutate(S0_imp  = ifelse(is.na(S0), S0_pot, S0),
                                 S1_imp  = ifelse(is.na(S1), S1_pot, S1),
                                 Y0_imp  = ifelse(is.na(Y0), Y0_pot, Y0),
                                 Y1_imp  = ifelse(is.na(Y1), Y1_pot, Y1),
                                 ITT_imp = Z*(Y1_imp-Y0_imp) + (1-Z)*(Y1_imp-Y0_imp)) %>%
                          select(USUBJID,S,Z,Y,I.1,I.2,I.3,I.4,W.H,W.D,W.I,W.B,S0,S0_imp,S1,S1_imp,Y0,Y0_imp,Y1,Y1_imp,ITT_imp,U)%>% 
                          rename(stratum_H=I.1,stratum_D=I.2,stratum_I=I.3,stratum_B=I.4)%>%
                          `rownames<-`(NULL)
    ITT_tb = f_Uhat(data=ITT_tb) ;head(ITT_tb)
    ITT_tb = f_Utrue(data=ITT_tb) ;head(ITT_tb)
    ITT_tb = ITT_tb %>% mutate(IU=as.numeric(Uhat==Utrue))
    
    for(i in 1:nrow(ITT_tb)){
      if(ITT_tb$Utrue[i]=="H"){ITT_tb$PCP[i]= ITT_tb$IU[i]*as.numeric(ITT_tb$W.H[i])  }else
        if(ITT_tb$Utrue[i]=="D"){ITT_tb$PCP[i]= ITT_tb$IU[i]*as.numeric(ITT_tb$W.D[i])  }else
          if(ITT_tb$Utrue[i]=="I"){ITT_tb$PCP[i]= ITT_tb$IU[i]*as.numeric(ITT_tb$W.I[i])  }else
            if(ITT_tb$Utrue[i]=="B"){ITT_tb$PCP[i]= ITT_tb$IU[i]*as.numeric(ITT_tb$W.B[i])  } 
              
    }
    }
   
    #-------------------------
    #  1. ACE
    #-------------------------
    # predicted n
    
    nH_    = nrow(ITT_tb %>% filter(S0_imp==0 & S1_imp==1) )
    nD_    = nrow(ITT_tb %>% filter(S0_imp==1 & S1_imp==1) )
    nI_    = nrow(ITT_tb %>% filter(S0_imp==0 & S1_imp==0) )
    nB_    = nrow(ITT_tb %>% filter(S0_imp==1 & S1_imp==0) )
    
    nH__    = nrow(ITT_tb %>% filter(Uhat=="H") )
    nD__    = nrow(ITT_tb %>% filter(Uhat=="D") )
    nI__    = nrow(ITT_tb %>% filter(Uhat=="I") )
    nB__    = nrow(ITT_tb %>% filter(Uhat=="B") )
    
    # (1.1) ITT version ACE
    
    ITTvec_H  = (   ITT_tb %>% filter(S0_imp==0 & S1_imp==1) %>% pull(ITT_imp) )   # S(0)=0, S(1)=1 -->  stratum H
    ITTvec_D  = (   ITT_tb %>% filter(S0_imp==1 & S1_imp==1) %>% pull(ITT_imp) )   # S(0)=1, S(1)=1 -->  stratum D
    ITTvec_I  = (   ITT_tb %>% filter(S0_imp==0 & S1_imp==0) %>% pull(ITT_imp) )   # S(0)=0, S(1)=0 -->  stratum I
    ITTvec_B  = (   ITT_tb %>% filter(S0_imp==1 & S1_imp==0) %>% pull(ITT_imp) )   # S(0)=1, S(1)=0 -->  stratum B
    
    # estimate -- ITTmean_U
    
    ITTmean_H[iter]  = mean(   ITTvec_H )    
    ITTmean_D[iter]  = mean(   ITTvec_D )    
    ITTmean_I[iter]  = mean(   ITTvec_I )    
    ITTmean_B[iter]  = mean(   ITTvec_B ) 
    
    # se(estimate)
    
    ITTse_H[iter] = sd(ITTvec_H)/sqrt(nH_)
    ITTse_D[iter] = sd(ITTvec_D)/sqrt(nD_)
    ITTse_I[iter] = sd(ITTvec_I)/sqrt(nI_)
    ITTse_B[iter] = sd(ITTvec_B)/sqrt(nB_)
    
    # (1.2) Delta version ACE
    
    # estimate  -- delta_U: posterior estimate of delta
    
    delta_H = delta_pot[,1]
    delta_D = delta_pot[,2]
    delta_I = delta_pot[,3]
    delta_B = delta_pot[,4]
    
    # se(estimate)
    
    # For normal conjugate prior, the precision of posterior delta = prior precision + likelihood precision
    prior_precision       = 1               # defined in f_datjags.R
    delta_se_H[iter]      = 1/(prior_precision + 1/ITTse_H[iter])
    delta_se_D[iter]      = 1/(prior_precision + 1/ITTse_D[iter])
    delta_se_I[iter]      = 1/(prior_precision + 1/ITTse_I[iter])
    delta_se_B[iter]      = 1/(prior_precision + 1/ITTse_B[iter])
    
    #-------------------------
    #  2. PCP
    #-------------------------
    
    # true n
    
    nH[iter]    = nrow(ITT_tb %>% filter(Utrue == "H") )
    nD[iter]    = nrow(ITT_tb %>% filter(Utrue == "D") )
    nI[iter]    = nrow(ITT_tb %>% filter(Utrue == "I") )
    nB[iter]    = nrow(ITT_tb %>% filter(Utrue == "B") )
    
    # PCP
    
    PCP_H[iter]  = mean(   ITT_tb %>% filter(Utrue == "H") %>% pull(PCP) )
    PCP_D[iter]  = mean(   ITT_tb %>% filter(Utrue == "D") %>% pull(PCP) )
    PCP_I[iter]  = mean(   ITT_tb %>% filter(Utrue == "I") %>% pull(PCP) )
    PCP_B[iter]  = mean(   ITT_tb %>% filter(Utrue == "B") %>% pull(PCP) )
    

  }
  
  
  #---------------------------------------------------
  # Analyze ACE with Rubin's Rule
  #---------------------------------------------------
  {
  ITT_H_results   = f_rubins_rule(estimate = ITTmean_H, se = ITTse_H)
  ITT_D_results   = f_rubins_rule(estimate = ITTmean_D, se = ITTse_D)
  ITT_I_results   = f_rubins_rule(estimate = ITTmean_I, se = ITTse_I)
  ITT_B_results   = f_rubins_rule(estimate = ITTmean_B, se = ITTse_B)
  delta_H_results = f_rubins_rule(estimate = delta_H, se = delta_se_H)
  delta_D_results = f_rubins_rule(estimate = delta_D, se = delta_se_D)
  delta_I_results = f_rubins_rule(estimate = delta_I, se = delta_se_I)
  delta_B_results = f_rubins_rule(estimate = delta_B, se = delta_se_B)
  }
  
  #-------------------- 
  # Deliver
  #--------------------
  {
  est  = data.frame(ITT_H   =  ITT_H_results$est_mean,
                    ITT_D   =  ITT_D_results$est_mean,
                    ITT_I   =  ITT_I_results$est_mean, 
                    ITT_B   =  ITT_B_results$est_mean, 
                     
                    ITT_se_H   =  ITT_H_results$est_se,
                    ITT_se_D   =  ITT_D_results$est_se,
                    ITT_se_I   =  ITT_I_results$est_se, 
                    ITT_se_B   =  ITT_B_results$est_se, 
                    
                    ITT_lb_H   =  ITT_H_results$lb,
                    ITT_lb_D   =  ITT_D_results$lb,
                    ITT_lb_I   =  ITT_I_results$lb, 
                    ITT_lb_B   =  ITT_B_results$lb,  
                    
                    ITT_ub_H   =  ITT_H_results$ub,
                    ITT_ub_D   =  ITT_D_results$ub,
                    ITT_ub_I   =  ITT_I_results$ub, 
                    ITT_ub_B   =  ITT_B_results$ub,  
                    
                    ITT_pvalue_H   =  ITT_H_results$pvalue,
                    ITT_pvalue_D   =  ITT_D_results$pvalue,
                    ITT_pvalue_I   =  ITT_I_results$pvalue, 
                    ITT_pvalue_B   =  ITT_B_results$pvalue, 
                    
                    delta_H =  delta_H_results$est_mean,
                    delta_D =  delta_D_results$est_mean,
                    delta_I =  delta_I_results$est_mean,
                    delta_B =  delta_B_results$est_mean,
                    
                    delta_se_H =  delta_H_results$est_se,
                    delta_se_D =  delta_D_results$est_se,
                    delta_se_I =  delta_I_results$est_se,
                    delta_se_B =  delta_B_results$est_se,
                    
                    delta_lb_H =  delta_H_results$lb,
                    delta_lb_D =  delta_D_results$lb,
                    delta_lb_I =  delta_I_results$lb,
                    delta_lb_B =  delta_B_results$lb,
                    
                    delta_ub_H =  delta_H_results$ub,
                    delta_ub_D =  delta_D_results$ub,
                    delta_ub_I =  delta_I_results$ub,
                    delta_ub_B =  delta_B_results$ub,
                    
                    delta_pvalue_H =  delta_H_results$pvalue,
                    delta_pvalue_D =  delta_D_results$pvalue,
                    delta_pvalue_I =  delta_I_results$pvalue,
                    delta_pvalue_B =  delta_B_results$pvalue,
                    
                    PCP_H   =  mean(PCP_H),
                    PCP_D   =  mean(PCP_D),
                    PCP_I   =  mean(PCP_I),
                    PCP_B   =  mean(PCP_B),
                    
                    nH      =  mean(nH),
                    nD      =  mean(nD),
                    nI      =  mean(nI),
                    nB      =  mean(nB))
  }
  
  return(est)   
  
}
