# Function to define dat.jags
f_datjags =function(dat, gamma_in,pm_result){
  dat.jags <-           list( Y0       = dat$Y0,
                              Y1       = dat$Y1,
                              S1       = dat$S1, 
                              N        = nrow(dat), 
                              X_s      = model.matrix(~ X1 + U , data = dat), 
                              X_y      = model.matrix(~ X1 + X2, data = dat)[,-1],
                              gamma_in = gamma_in,
                              alpha_pm = pm_result$alpha_pm,
                              beta_pm  = pm_result$beta_pm,
                              eta_pm   = pm_result$eta_pm,
                              delta_pm = pm_result$delta_pm,
                              beta0_pm = pm_result$beta0_pm
  )
  
  return(dat.jags)
}
