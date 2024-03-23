# Function to calculate the true causal effect(HR) for simulated datasets  
f_trueHR = function(dat){
  trueHR_S1eq0 = exp( coxph(Surv(Y, c) ~ TRT+ X1 + X2 , data = dat %>% filter(S1_ == 0) )$coefficients[1] ) 
  trueHR_S1eq1 = exp( coxph(Surv(Y, c) ~ TRT+ X1 + X2 , data = dat %>% filter(S1_ == 1) )$coefficients[1] ) 
  return(list( trueHR_S1eq0 = trueHR_S1eq0,
               trueHR_S1eq1 = trueHR_S1eq1 )
         )
}
