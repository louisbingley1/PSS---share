# Function to compute ACE -> Odds Ratio within each Stratum of Interest
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
  
  logor_S1eq1       = rep(NA, niter)
  logor_S1eq0       = rep(NA, niter)
  logor_S1eq1_vars  = rep(NA, niter)
  logor_S1eq0_vars  = rep(NA, niter) 
  
  for (iter in 1:niter){
    
    dat_imp  =  dat %>% select(USUBJID, TRT, Y1, Y0, S1, X1,X2) %>%
                      mutate(  S1_pot  = S1_pot[,iter],
                               Y0_pot  = Y0_pot[,iter],
                               Y1_pot  = Y1_pot[,iter],
                               S1_imp  = ifelse(is.na(S1), S1_pot, S1), 
                               Y0_imp  = ifelse(is.na(Y0), Y0_pot, Y0),
                               Y1_imp  = ifelse(is.na(Y1), Y1_pot, Y1),
                               Y_imp   = ifelse(TRT==1, Y1_imp, Y0_imp))
    
    fit_S1eq1                    = glm(Y_imp ~ TRT+ X1 + X2 , data = dat_imp %>% filter(S1_imp == 1),family = "binomial")
    fit_S1eq0                    = glm(Y_imp ~ TRT+ X1 + X2 , data = dat_imp %>% filter(S1_imp == 0),family = "binomial")
    logor_S1eq1[iter]            = coef(fit_S1eq1)[2]       # estimated logOR_S1eq1
    logor_S1eq0[iter]            = coef(fit_S1eq0)[2]       # estimated logOR_S1eq0
    logor_S1eq1_vars[iter]       = vcov(fit_S1eq1)[2, 2]
    logor_S1eq0_vars[iter]       = vcov(fit_S1eq0)[2, 2]
  }
  
  ## Rubin's rule to combine OR
  
  out <- data.frame(logor_S1eq1_mean  = c(mean(logor_S1eq1)), 
                    logor_S1eq1_vars  = c(mean(logor_S1eq1_vars) + var(logor_S1eq1)), 
                    logor_S1eq0_mean  = c(mean(logor_S1eq0)), 
                    logor_S1eq0_vars  = c(mean(logor_S1eq0_vars) + var(logor_S1eq0)))
  
  est <- data.frame(or.S1eq0   = exp(out$logor_S1eq0_mean),
                    or.S1eq0.h = exp(out$logor_S1eq0_mean + 1.96*sqrt(out$logor_S1eq0_vars)),
                    or.S1eq0.l = exp(out$logor_S1eq0_mean - 1.96*sqrt(out$logor_S1eq1_vars)),
                    or.S1eq1   = exp(out$logor_S1eq1_mean),
                    or.S1eq1.h = exp(out$logor_S1eq1_mean + 1.96*sqrt(out$logor_S1eq1_vars)),
                    or.S1eq1.l = exp(out$logor_S1eq1_mean - 1.96*sqrt(out$logor_S1eq1_vars)))
  
  return(est)
}
