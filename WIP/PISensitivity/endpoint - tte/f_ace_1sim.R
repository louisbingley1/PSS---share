# Function to compute ACE -> Hazard Ratio for each Stratum of Interest
f_ace_1sim    =  function(postparam,dat, cutoff) {  # dat = dat_in
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
  
  
  loghr_S1eq1       = rep(NA, niter)
  loghr_S1eq0       = rep(NA, niter)
  loghr_S1eq1_vars  = rep(NA, niter)
  loghr_S1eq0_vars  = rep(NA, niter) 
  hr_S1eq1_mst      = rep(NA, niter)
  hr_S1eq0_mst      = rep(NA, niter)
  
  for (iter in 1:niter){
    
    dat_imp  =  dat %>% select(USUBJID, TRT, Y1, Y0, S1, X1,X2) %>%
      mutate(  S1_pot  = S1_pot[,iter],
               Y0_pot  = Y0_pot[,iter],
               Y1_pot  = Y1_pot[,iter],
               S1_imp  = ifelse(is.na(S1), S1_pot, S1), 
               Y0_imp  = ifelse(is.na(Y0), Y0_pot, Y0),
               Y1_imp  = ifelse(is.na(Y1), Y1_pot, Y1),
               Y_imp   = ifelse(TRT==1, Y1_imp, Y0_imp),
               c_imp   = ifelse(Y_imp<=cutoff, 1, 0))
    
    # S1 = 1
    
    fit_S1eq1                    = coxph(Surv(Y_imp, c_imp) ~ TRT+ X1 + X2 , data = dat_imp %>% filter(S1_imp == 1) ) 
    loghr_S1eq1[iter]            = coef(fit_S1eq1)[1]       # estimated logOR_S1eq1
    loghr_S1eq1_vars[iter]       = vcov(fit_S1eq1)[1, 1]
    m1_S1eq1_imp                 = median(dat_imp %>% filter(S1_imp == 1) %>% pull(Y1_imp))
    m0_S1eq1_imp                 = median(dat_imp %>% filter(S1_imp == 1) %>% pull(Y0_imp))
    hr_S1eq1_mst[iter]           = m0_S1eq1_imp/m1_S1eq1_imp
    
    # S1 = 0
    
    fit_S1eq0                    = coxph(Surv(Y_imp, c_imp) ~ TRT+ X1 + X2 , data = dat_imp %>% filter(S1_imp == 0) ) 
    loghr_S1eq0[iter]            = coef(fit_S1eq0)[1]       # estimated logOR_S1eq0
    loghr_S1eq0_vars[iter]       = vcov(fit_S1eq0)[1, 1]
    m1_S1eq0_imp                 = median(dat_imp %>% filter(S1_imp == 0) %>% pull(Y1_imp))
    m0_S1eq0_imp                 = median(dat_imp %>% filter(S1_imp == 0) %>% pull(Y0_imp))
    hr_S1eq0_mst[iter]           = m0_S1eq0_imp/m1_S1eq0_imp
       
  }
  
  ## Rubin's rule to combine HR
  out <- data.frame(loghr_S1eq1_mean  = c(mean(loghr_S1eq1)), 
                    loghr_S1eq1_vars  = c(mean(loghr_S1eq1_vars) + var(loghr_S1eq1)), 
                    loghr_S1eq0_mean  = c(mean(loghr_S1eq0)), 
                    loghr_S1eq0_vars  = c(mean(loghr_S1eq0_vars) + var(loghr_S1eq0)),
                    hr_S1eq1_mst_mean = mean(hr_S1eq1_mst),
                    hr_S1eq1_mst_vars = var(hr_S1eq1_mst)/niter,
                    hr_S1eq0_mst_mean = mean(hr_S1eq0_mst),
                    hr_S1eq0_mst_vars = var(hr_S1eq0_mst)/niter
  )
  est <- data.frame(hr.S1eq0       = exp(out$loghr_S1eq0_mean),
                    hr.S1eq0.h     = exp(out$loghr_S1eq0_mean + 1.96*sqrt(out$loghr_S1eq0_vars)),
                    hr.S1eq0.l     = exp(out$loghr_S1eq0_mean - 1.96*sqrt(out$loghr_S1eq1_vars)),
                    hr.S1eq1       = exp(out$loghr_S1eq1_mean),
                    hr.S1eq1.h     = exp(out$loghr_S1eq1_mean + 1.96*sqrt(out$loghr_S1eq1_vars)),
                    hr.S1eq1.l     = exp(out$loghr_S1eq1_mean - 1.96*sqrt(out$loghr_S1eq1_vars)),
                    hr_S1eq1_mst   = out$hr_S1eq1_mst_mean,
                    hr_S1eq1_mst.h = out$hr_S1eq1_mst_mean + 1.96*sqrt(out$hr_S1eq1_mst_vars),
                    hr_S1eq1_mst.l = out$hr_S1eq1_mst_mean - 1.96*sqrt(out$hr_S1eq1_mst_vars),
                    hr_S1eq0_mst   = out$hr_S1eq0_mst_mean,
                    hr_S1eq0_mst.h = out$hr_S1eq0_mst_mean + 1.96*sqrt(out$hr_S1eq0_mst_vars),
                    hr_S1eq0_mst.l = out$hr_S1eq0_mst_mean - 1.96*sqrt(out$hr_S1eq0_mst_vars)
  )
  
  #return(list(out = out, est = est   ))  # S1 = sims[, S1_ind]
  return(est)
}
