# Function to set initial values for missing Y0 (not used)
f_Y0_init = function(dat){
  Y02       <- dat$Y0
  ind       <- is.na(dat$Y0)
  m0        <- median(Y02[!ind])
  Y02[!ind] <- NA      
  Y02[ind]  <- rexp(sum(ind),log(2)/m0)
  return(Y02)
}  

