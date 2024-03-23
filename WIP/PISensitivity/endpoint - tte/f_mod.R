# Function to define model and write to file
{
  text <- "model{
            
            ## model                                                            
            
            for(i in 1:N){
            
              ## binary model for S(1) (trt disc due to AE on trt)
                  logit(p[i])       = inprod(alpha[], X_s[i,] )                 
                  S1[i]             ~ dbern(p[i])
                  
              ## Survival model for Y(0)
                  mu0[i]             = inprod(beta[],X_y[i,] )               
                  lambda0[i]         = exp(beta0 + mu0[i] + gamma_in*S1[i]) 
                  Y0[i]              ~ dexp(lambda0[i])  
                  
              ## Survival model for Y(1)
                  mu1[i]             = inprod(beta[],X_y[i,] )               
                  lambda1[i]         = exp(beta0 + mu1[i] + eta*S1[i]+ delta) 
                  Y1[i]              ~ dexp(lambda1[i])  

            }
            
            ## priors 
            
              for(b in 1:3){      alpha[b] ~ dnorm(alpha_pm[b],1) }   # true alpha = -1.78,2,0
              
              beta0  ~ dnorm(beta0_pm,1)                             # true beta0 = -3
              for(b in 1:2){      beta[b]  ~ dnorm(beta_pm[b], 1) }  # true beta1=0.5, beta2=-0.5
              eta    ~ dnorm(eta_pm,1)                               # true eta = 0.47
              delta  ~ dnorm(delta_pm,1)                             # true delta = -0.69
             
              # beta0  ~ dnorm(-3,1)                                   # true beta0 = -3
              # for(b in 1:2){      beta[b]  ~ dnorm(0, 1)          }  # true beta1=0.5 , beta2=-0.5
              # eta    ~ dnorm(.5,1)                                   # true eta = 0.47
              # delta  ~ dnorm(-.5,1)                                  # true delta = -0.69

}
"
cat(text, file="mod.txt")

}
  