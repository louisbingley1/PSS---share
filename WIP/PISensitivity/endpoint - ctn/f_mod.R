# Function to define model

  text <- "model{
            
            ## model                                                            
            
            for(i in 1:N){
            
              ## binary model for S(1) (trt disc due to AE on trt)
                  logit(p[i])       = inprod(alpha[], X_s[i,] )                 
                  S1[i]             ~ dbern(p[i])
                  
              ## Continuous model for Y(0),Y(1)
                   mu0_post[i]       = inprod(beta[], X_y[i,]) + gamma_in*S1[i]
                   mu1_post[i]       = inprod(beta[], X_y[i,]) + eta*S1[i] + delta  # X_y1==X_y
                   Y0[i]             ~ dnorm(mu0_post[i],1)
                   Y1[i]             ~ dnorm(mu1_post[i],1)

            }
            
            ## priors                                                           
            for(b in 1:3){  alpha[b] ~ dnorm(alpha_pm[b], 1)
                             beta[b]  ~ dnorm(beta_pm[b], 1) }
            eta    ~ dnorm(eta_pm,1)    
            delta  ~ dnorm(delta_pm,1)   
}

"