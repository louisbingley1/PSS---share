
# Function to define the prior means in the model:
f_pm = function(dat){
  
    alpha0                = glm( S1  ~ X1 + U, data = dat, family = "binomial" )$coefficient[1]
    alpha1                = glm( S1  ~ X1 + U, data = dat, family = "binomial" )$coefficient[2]
    alpha2                = glm( S1  ~ X1 + U, data = dat, family = "binomial" )$coefficient[3]
    beta1                 = coxph(Surv(Y0_, c1) ~ X1 + X2 + S1,   data = dat   )$coefficient[1]
    beta2                 = coxph(Surv(Y0_, c1) ~ X1 + X2 + S1,   data = dat   )$coefficient[2]
    eta                   = coxph(Surv(Y1_, c1) ~ X1 + X2 + S1,   data = dat   )$coefficient[3]
    beta0                 = lm(formula = log(1/Y0_) ~ X1 + X2 + S1, data= dat  )$coefficient[1]
    beta0_plus_delta      = lm(formula = log(1/Y1 ) ~ X1 + X2 + S1, data= dat  )$coefficient[1]
    delta                 = beta0_plus_delta - beta0

  return(list(alpha_pm = c(alpha0,alpha1,alpha2),
              beta_pm  = c(beta1,beta2),
              eta_pm   = eta,
              delta_pm = delta,
              beta0_pm = beta0
              )
         )
}
