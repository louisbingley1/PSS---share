f_inits=function(dat){
  
  X     = model.matrix(lm(Z ~ X_1_standardized + X_2_standardized + base_standardized, data = dat))       
  inits = function(){list(beta    = cbind( rnorm(ncol(X),0,1),
                                           rnorm(ncol(X),0,1),
                                           rnorm(ncol(X),0,1),
                                           rnorm(ncol(X),0,1)), 
                          eta     = cbind( rnorm(ncol(X),0,1),
                                           rnorm(ncol(X),0,1),
                                           rnorm(ncol(X),0,1)), 
                          delta   = rnorm( 4,1,.1)  
  )}
  return(inits)
}
