f_inits  =  function(dat,seed,gamma_in,pm_result){    
  inits <- function(){ list(alpha     = pm_result$alpha_pm,   # true alpha0 = -1.78, alpha1=2, alpha2=0
                            beta      = pm_result$beta_pm,    # true beta0 = -3, beta1 = 0.5, beta2 = -0.5
                            eta       = pm_result$eta_pm,     # true eta = 0.47,
                            delta     = pm_result$delta_pm,   # true delta = -0.69 
                            .RNG.name  = "base::Wichmann-Hill", 
                            .RNG.seed  = seed)  }
  return(inits)
}
 