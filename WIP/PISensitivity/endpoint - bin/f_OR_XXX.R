# Function to calculate the true causal effect (OR) |S1=1 for each simulated dataset
f_OR_S1eq0 <- function(data) {
  data_S1eq0        <- data %>% filter(S1_ == 0)
  treated_cases     <- sum(    data_S1eq0$Y[ data_S1eq0$TRT == 1])
  treated_non_cases <- sum(1 - data_S1eq0$Y[ data_S1eq0$TRT == 1])
  control_cases     <- sum(    data_S1eq0$Y[ data_S1eq0$TRT == 0  ])
  control_non_cases <- sum(1 - data_S1eq0$Y[ data_S1eq0$TRT == 0  ])
  OR_S1eq0            <- (treated_cases / treated_non_cases) / (control_cases / control_non_cases)
  return(OR_S1eq0)
}
f_OR_S1eq1 <- function(data) {
  data_S1eq1        <- data %>% filter(S1_ == 1)
  treated_cases     <- sum(    data_S1eq1$Y[ data_S1eq1$TRT == 1])
  treated_non_cases <- sum(1 - data_S1eq1$Y[ data_S1eq1$TRT == 1])
  control_cases     <- sum(    data_S1eq1$Y[ data_S1eq1$TRT == 0])
  control_non_cases <- sum(1 - data_S1eq1$Y[ data_S1eq1$TRT == 0])
  OR_S1eq1            <- (treated_cases / treated_non_cases) / (control_cases / control_non_cases)
  return(OR_S1eq1)
}
