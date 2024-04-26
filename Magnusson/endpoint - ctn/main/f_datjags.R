f_datjags = function(dat,I,pm_results){
  X   = model.matrix(lm(Z ~ X_1_standardized + X_2_standardized + base_standardized, data = dat))
  dat.jags=list(  N                           = nrow(dat),         
                  k                           = 4,                
                  P                           = ncol(X),         
                  Y0                          = dat$Y0,
                  Y1                          = dat$Y1,
                  S0                          = dat$S0,
                  S1                          = dat$S1,
                  X                           = X,                
                  I                           = I,              
                  prior_delta1_mean           = pm_results$prior_delta1_mean,     # H: true TrtEff=0.5 
                  prior_delta2_mean           = pm_results$prior_delta2_mean,     # D: true TrtEff=0
                  prior_delta3_mean           = pm_results$prior_delta3_mean,     # I: true TrtEff=2
                  prior_delta4_mean           = pm_results$prior_delta4_mean,     # B: true TrtEff=1.5
                  prior_beta_mean             = 0,
                  prior_eta0_H_mean           = 0,   
                  prior_eta123_H_mean         = 0,
                  prior_eta0123_DIB_mean      = 0,
                  prior_delta_precision       = 1, 
                  prior_beta_precision        = 0.01,
                  prior_eta0_H_precision      = 0.01,
                  prior_eta123_H_precision    = 0.01,
                  prior_eta0123_DIB_precision = 0.01
  ) 
  
  return(dat.jags)
  
}
