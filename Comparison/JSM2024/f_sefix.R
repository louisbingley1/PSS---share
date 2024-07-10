f_sefix=function(data){
  
 se_B = sd(data$delta_B)
 se_D = sd(data$delta_D)
 se_I = sd(data$delta_I)
 se_H = sd(data$delta_H)
 se_IB = sd(data$delta_IB)
 
 data$delta_lb_H = data$delta_H - 1.96*se_H 
 data$delta_lb_B = data$delta_B - 1.96*se_B 
 data$delta_lb_D = data$delta_D - 1.96*se_D 
 data$delta_lb_I = data$delta_I - 1.96*se_I 
 data$delta_lb_IB = data$delta_IB - 1.96*se_IB
 
 data$delta_ub_H = data$delta_H + 1.96*se_H 
 data$delta_ub_B = data$delta_B + 1.96*se_B 
 data$delta_ub_D = data$delta_D + 1.96*se_D 
 data$delta_ub_I = data$delta_I + 1.96*se_I 
 data$delta_ub_IB = data$delta_IB + 1.96*se_IB
 
 return(data)
}

data=head(s1_BS)
f_sefix(data)
