f_ace_1sim      =  function(postparam,dat,I) {   
 
  niter          = nrow(postparam)
   
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
  
  
  ITT_H          = rep(NA, niter)
  ITT_D          = rep(NA, niter)
  ITT_I          = rep(NA, niter)
  ITT_B          = rep(NA, niter)
  
  for (iter in 1:niter){
    ITT_tb   =  cbind.data.frame(USUBJID = seq(1,nrow(dat),1), 
                                 S0_pot  = S0_pot[,iter],
                                 S1_pot  = S1_pot[,iter],
                                 Y0_pot  = Y0_pot[,iter],
                                 Y1_pot  = Y1_pot[,iter],
                                 Z       = dat$TRT,
                                 S       = dat$S,
                                 S0      = dat$S0,
                                 S1      = dat$S1,
                                 Y       = dat$Y,
                                 Y0      = dat$Y0,
                                 Y1      = dat$Y1,
                                 I       = I
                                  ) %>% 
                          mutate(S0_imp  = ifelse(is.na(S0), S0_pot, S0),
                                 S1_imp  = ifelse(is.na(S1), S1_pot, S1),
                                 Y0_imp  = ifelse(is.na(Y0), Y0_pot, Y0),
                                 Y1_imp  = ifelse(is.na(Y1), Y1_pot, Y1),
                                 ITT_imp = Z*(Y1_imp-Y0_imp) + (1-Z)*(Y1_imp-Y0_imp)) %>%
                          select(USUBJID,S,Z,Y,I.1,I.2,I.3,I.4,S0,S0_imp,S1,S1_imp,Y0,Y0_imp,Y1,Y1_imp,ITT_imp)%>% 
                          rename(stratum_H=I.1,stratum_D=I.2,stratum_I=I.3,stratum_B=I.4)%>%
                          `rownames<-`(NULL)
    head(ITT_tb)
    
    ITT_H[iter]  = mean(   ITT_tb %>% filter(S0_imp==0 & S1_imp==1) %>% pull(ITT_imp) )   # S(0)=0, S(1)=1 -->  stratum H
    ITT_D[iter]  = mean(   ITT_tb %>% filter(S0_imp==1 & S1_imp==1) %>% pull(ITT_imp) )   # S(0)=1, S(1)=1 -->  stratum D
    ITT_I[iter]  = mean(   ITT_tb %>% filter(S0_imp==0 & S1_imp==0) %>% pull(ITT_imp) )   # S(0)=0, S(1)=0 -->  stratum I
    ITT_B[iter]  = mean(   ITT_tb %>% filter(S0_imp==1 & S1_imp==0) %>% pull(ITT_imp) )   # S(0)=1, S(1)=0 -->  stratum B
    
  }
  
  est  = data.frame(ITT_H   =  mean(ITT_H,na.rm = T), 
                    ITT_D   =  mean(ITT_D,na.rm = T), 
                    ITT_I   =  mean(ITT_I,na.rm = T), 
                    ITT_B   =  mean(ITT_B,na.rm = T), 
                    delta_H =  mean(delta_pot[,1]),
                    delta_D =  mean(delta_pot[,2]),
                    delta_I =  mean(delta_pot[,3]),
                    delta_B =  mean(delta_pot[,4]))
  
  return(est)   
  
}
