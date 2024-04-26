# Function to calculate the delta|S1=0 for each simulated dataset

f_delta_S1eq0 <- function(data) {
  data_S1eq0        <- data %>% filter(S1_ == 0)
  Y1_minus_Y0_S1eq0 <- mean( data_S1eq0$Y1_ -  data_S1eq0$Y0_ )
  return(Y1_minus_Y0_S1eq0)
}

# Function to calculate the delta|S1=1 for each simulated dataset

f_delta_S1eq1 <- function(data) {
  data_S1eq1        <- data %>% filter(S1_ == 1)
  Y1_minus_Y0_S1eq1 <- mean( data_S1eq1$Y1_ -  data_S1eq1$Y0_ )
  return(Y1_minus_Y0_S1eq1)
}