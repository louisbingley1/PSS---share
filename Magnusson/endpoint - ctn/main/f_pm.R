f_pm = function(sim){
  list = list(
    prior_delta1_mean = sim$true_d_T3$true_d_T3_adhpbo,     # H: true TrtEff=0.5 
    prior_delta2_mean = sim$true_d_T3$true_d_T3_adhnei,     # D: true TrtEff=0
    prior_delta3_mean = sim$true_d_T3$true_d_T3_adhboth,    # I: true TrtEff=2
    prior_delta4_mean = sim$true_d_T3$true_d_T3_adhact     # B: true TrtEff=1.5  
  )
  return(list)
}
