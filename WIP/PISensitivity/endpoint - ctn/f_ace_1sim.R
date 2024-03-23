# Function to compute ACE by strata -> Y1-Y0 or the 'delta‘ | S1 == 1 or 0
f_ace_1sim      =  function(postparam,dat) {   
  #dim(postparam)
  niter             = nrow(postparam)
  
  # "S1"
  S1_pot_           = postparam[,grep("S1",    names(postparam), value=TRUE)]     # nrows=niter,ncols=n
  col_order         = paste0("S1[",1:nrow(dat),"]")
  S1_pot            = S1_pot_[,col_order]
  S1_pot            = t(S1_pot)
  
  # "Y1"
  Y1_pot_           = postparam[,grep("Y1",   names(postparam), value=TRUE)]     # nrows=niter,ncols=n
  col_order         = paste0("Y1[",1:nrow(dat),"]")
  Y1_pot            = Y1_pot_[,col_order]
  Y1_pot            = t(Y1_pot)
  
  # "Y0"
  Y0_pot_           = postparam[,grep("Y0",   names(postparam), value=TRUE)]     # nrows=niter,ncols=n
  col_order         = paste0("Y0[",1:nrow(dat),"]")
  Y0_pot            = Y0_pot_[,col_order]
  Y0_pot            = t(Y0_pot)
   
  ITT_S1eq0         = rep(NA, niter)
  ITT_S1eq1         = rep(NA, niter)
  
  #  delta_S1eq0       = delta_post[,1]  # stratum '1' -- Harmed
  #  delta_S1eq1       = delta_post[,1]  # stratum '1' -- Harmed
  
  for (iter in 1:niter){
    
    ITT_tb   =  dat %>% select(USUBJID, TRT, Y1, Y0, S1) %>%
                        mutate(  S1_pot  = S1_pot[,iter],
                                 Y0_pot  = Y0_pot[,iter],
                                 Y1_pot  = Y1_pot[,iter],
                                 S1_imp  = ifelse(is.na(S1), S1_pot, S1), 
                                 Y0_imp  = ifelse(is.na(Y0), Y0_pot, Y0),
                                 Y1_imp  = ifelse(is.na(Y1), Y1_pot, Y1),
                                 ITT     = TRT*(Y1_imp-Y0_imp) + (1-TRT)*(Y1_imp-Y0_imp))
    # head(ITT_tb)
    
    ITT_S1eq0[iter]  = mean(   ITT_tb %>% filter(S1_imp==0) %>% pull(ITT) )   # extract those belonging to stratum H
    ITT_S1eq1[iter]  = mean(   ITT_tb %>% filter(S1_imp==1) %>% pull(ITT) )   # extract those belonging to stratum H
    
    
  }
  
  est  = data.frame(ITT_S1eq0 = mean(ITT_S1eq0), 
                    ITT_S1eq1 = mean(ITT_S1eq1))
  
  return(est)   
  
}  
