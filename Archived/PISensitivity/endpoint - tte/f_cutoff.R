# Function to determine cutoff (pick any simulation set -> dat_in)
f_cutoff = function(data){
  lambda0 = data$lambda0
  lambda1 = data$lambda1
  m0      = log(2)/lambda0  # median survival time of the control
  m1      = log(2)/lambda1  # median survival time of the treatment
  par(mfrow=c(1,2))
  plot(density(m0))
  plot(density(m1))         # visualize the mode(survival time) and pick an appropriate cutoff
}