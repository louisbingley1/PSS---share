# Function to define the prior means in the model:

f_pm = function(dat){

    alpha0                = glm( S1  ~ X1 + U,        data = dat, family = "binomial"  )$coefficient[1]
    alpha1                = glm( S1  ~ X1 + U,        data = dat, family = "binomial"  )$coefficient[2]
    alpha2                = glm( S1  ~ X1 + U,        data = dat, family = "binomial"  )$coefficient[3]
    beta0                 = glm( Y0_ ~ X1 + X2 + S1,  data = dat, family = "binomial"  )$coefficient[1]
    beta1                 = glm( Y0_ ~ X1 + X2 + S1,  data = dat, family = "binomial"  )$coefficient[2]
    beta2                 = glm( Y0_ ~ X1 + X2 + S1,  data = dat, family = "binomial"  )$coefficient[3]
    beta0_plus_delta      = glm( Y1  ~ X1 + X2 + S1,  data = dat, family = "binomial"  )$coefficient[1]
    eta                   = glm( Y1  ~ X1 + X2 + S1,  data = dat, family = "binomial"  )$coefficient[4]
    
  return(list(alpha_pm =  c(alpha0, alpha1,alpha2),
              beta_pm  =  c(beta0,beta1,beta2),
              eta_pm   =  eta ,
              delta_pm =  beta0_plus_delta - beta0
              )
         )
}
