f_rubins_rule = function(estimate, se, alpha=0.05, theta_null = 0){   
  
  #-----------------------------------------------------------------------------------------------
  # m       : number of iterations/imputation sets
  # estimate: a vector of length m. Each element is the mean(ITT) of all subjects within a strata.
  # se      : a vector of length m. Each element is the se(ITT) of all subjects within a strata.
  #-----------------------------------------------------------------------------------------------
  
  # Pooling Effect estimates
  m         = length(estimate)           # number of iterations (each iteration is an imputation set for 1 simulated data)
  theta_bar = mean(estimate)
  
  # Pooling Standard errors
  V_w       = mean(se^2)
  V_b       = (sum(estimate - theta_bar)^2)/(m-1)
  V_t       = V_w + V_b + V_b/m
  se_total  = sqrt(V_t)
  
  # Confidence Interval
  r = (V_b+V_b/m)/V_w
  v = (m-1)*(1+1/r)^2
  lb = theta_bar - qt(p = 1 - alpha/2, df = v)*se_total   
  ub = theta_bar + qt(p = 1 - alpha/2, df = v)*se_total
  
  # pvalue
  test_stat = (theta_bar-theta_null)^2/V_t
  pvalue = 1- pf(q = test_stat, df1 = 1, df2 = v)

  
  result = list(est_mean=theta_bar, est_se = se_total, lb=lb, ub=ub, pvalue=pvalue)        
  
  return(result)
  
}
