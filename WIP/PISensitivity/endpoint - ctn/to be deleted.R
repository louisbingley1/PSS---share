
# Function to set initial values for missing Y0 and Y1 ( :( doesn't work! --> have to use Y0_, Y1_ in dat.jags for Y0,Y1 )
f_Y0_init =function(dat,gamma_in){
  init.beta  = glm( Y0_ ~ S1 + X1 + X2, data = dat  )$coefficient[-2]   # take out coeff of S1              
  X_y         = model.matrix(~ X1 + X2, data = dat)
  S1          = dat$S1
  init.mu0    = init.beta %*% t(X_y) + gamma_in*S1 
  Y02        <- dat$Y0
  ind        <- is.na(dat$Y0)
  Y02[!ind]  <- NA      
  Y02[ind]   <- rnorm(init.mu0[ind],1)
  return(Y02)
}  
f_Y1_init =function(dat,gamma_in){
  
  init.beta   = glm( Y1 ~ S1 + X1 + X2, data = dat )$coefficient    # coeff of S1 is eta now, intercept is beta0+delta. No need to take out anything.                           
  X_y_        = model.matrix(~ S1 + X1 + X2, data = dat)
  S1          = dat$S1
  init.mu1    = init.beta %*% t(X_y_) 
  Y12        <- dat$Y1
  ind        <- is.na(dat$Y1)
  Y12[!ind]  <- NA      
  Y12[ind]   <- rnorm(init.mu1[ind],1)
  return(Y12)
}
