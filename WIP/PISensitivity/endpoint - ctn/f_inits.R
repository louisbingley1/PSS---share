f_inits   =  function(dat,seed,gamma_in){    
  init.alpha            = glm( S1  ~ X1 + U,        data = dat, family = "binomial"  )$coefficient                
  init.beta0            = glm( Y0_ ~ X1 + X2 + S1,  data = dat  )$coefficient[1]    # assuming Y0_ is known
  init.beta             = glm( Y0_ ~ X1 + X2 + S1,  data = dat  )$coefficient[1:3]   # assuming Y0_ is known, and take out coeff of S1                 
  init.eta              = glm( Y1  ~ X1 + X2 + S1,  data = dat  )$coefficient[4]
  init.beta0_plus_delta = glm( Y1  ~ X1 + X2 + S1,  data = dat  )$coefficient[1]
  init.delta            = init.beta0_plus_delta - init.beta0
  inits <- function(){ list(alpha     = init.alpha ,   # true alpha0 = -1.78, alpha1=2, alpha2=0
                            beta      = init.beta,     # true beta0=0, beta1 = 0.5, beta2 = -0.5
                            eta       = init.eta,      # true eta = 0.3,
                            delta     = init.delta,    # true delta = 0.5 
                            .RNG.name  = "base::Wichmann-Hill", 
                            .RNG.seed  = seed)  }
  return(inits)
}