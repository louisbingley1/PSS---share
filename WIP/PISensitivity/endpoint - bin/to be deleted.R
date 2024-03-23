# Function to set initial values for missing Y0
f_Y02     =  function(dat){
  Y02       <- dat$Y0
  ind       <- is.na(dat$Y0)
  Y02[!ind] <- NA      
  Y02[ind]  <- rbinom(sum(ind),1, mean(dat$Y0, na.rm = T))
  return(Y02)
}  

# Function to define all initial values   
f_inits   =  function(dat,seed){    
  
  init.alpha  <- glm( S1 ~ X1 + U,       data = dat %>% filter(TRT == 1),  family = "binomial")$coefficient
  init.beta_  <- glm( Y1 ~ X1 + X2 + S1, data = dat %>% filter(TRT == 1),  family = "binomial")$coefficient
  init.beta   <- init.beta_[1:3]      
  init.Y0     <- f_Y02(dat= dat)
  
  inits <- function(){ list(Y0       = init.Y0,  
                            beta      = init.beta, 
                            alpha     = init.alpha,
                            .RNG.name  = "base::Wichmann-Hill", 
                            .RNG.seed  = 2020) 
  }
  
  
  return(inits)
  
}