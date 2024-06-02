f_PS_MBoot_1sim = function(data_in, ep0,ep1,M,iter.max,error0,seed_M_v){ 

  #  Bootstrap -> to compute se(ACE) -- the sd of all bootstrap ACEs, within 1 simulation;
 
  N            = nrow(data_in)                     # number of subjects in data
  n            = N                     # sample full number(=0.5N) of subjects in each bootstrap data
  CACE_bs      = rep(NA,M) 
  CACE.adj_bs  = rep(NA,M)
  CPCP_bs      = rep(NA,M)
  NACE_bs      = rep(NA,M) 
  NACE.adj_bs  = rep(NA,M)
  NPCP_bs      = rep(NA,M)
  AACE_bs      = rep(NA,M) 
  AACE.adj_bs  = rep(NA,M)
  APCP_bs      = rep(NA,M)
  CAACE_bs      = rep(NA,M) 
  CAACE.adj_bs  = rep(NA,M)
  
  for(m in 1:M){ # m=1
    set.seed(seed_M_v[m])

    boot            = data_in[sample(1:N,n,replace=T),] ;head(boot) # re-sample data with replacement
    boot            = as.data.frame(boot)
    result          = f_PS( data <- boot, ep1,ep0,beta.a,beta.n, iter.max, error0)
    
    AACE_bs[m]      =  result$AACE
    AACE.adj_bs[m]  =  result$AACE.adj
    APCP_bs[m]      =  result$APCP
    
    CACE_bs[m]      =  result$CACE
    CACE.adj_bs[m]  =  result$CACE.adj
    CPCP_bs[m]      =  result$CPCP
    
    NACE_bs[m]      =  result$NACE
    NACE.adj_bs[m]  =  result$NACE.adj
    NPCP_bs[m]      =  result$NPCP
    
    CAACE_bs[m]      =  result$CAACE
    CAACE.adj_bs[m]  =  result$CAACE.adj
     
  }
  
  # Extract and store values

  ace = cbind.data.frame( AACE              = mean(AACE_bs,na.rm=T) ,                         
                          AACE_se           = sd(AACE_bs,na.rm=T)  ,                          
                          AACE_CI_l         = quantile(AACE_bs, c(0.025,0.975),na.rm = T)[1] ,   
                          AACE_CI_u         = quantile(AACE_bs, c(0.025,0.975),na.rm = T)[2],    
                          AACE.adj          = mean(AACE.adj_bs,na.rm=T),
                          AACE.adj_se       = sd(AACE.adj_bs,na.rm=T),                         
                          AACE.adj_CI_l     = quantile(AACE.adj_bs, c(0.025,0.975),na.rm = T)[1], 
                          AACE.adj_CI_u     = quantile(AACE.adj_bs, c(0.025,0.975),na.rm = T)[2], 
                          
                          CACE              = mean(CACE_bs,na.rm=T) ,                         
                          CACE_se           = sd(CACE_bs,na.rm=T),                            
                          CACE_CI_l         = quantile(CACE_bs, c(0.025,0.975),na.rm = T)[1],    
                          CACE_CI_u         = quantile(CACE_bs, c(0.025,0.975),na.rm = T)[2] ,   
                          CACE.adj          = mean(CACE.adj_bs,na.rm=T) ,                       
                          CACE.adj_se       = sd(CACE.adj_bs,na.rm=T)  ,                       
                          CACE.adj_CI_l     = quantile(CACE.adj_bs, c(0.025,0.975),na.rm = T)[1] ,
                          CACE.adj_CI_u     = quantile(CACE.adj_bs, c(0.025,0.975),na.rm = T)[2] ,
                          
                          NACE              = mean(NACE_bs,na.rm=T)   ,                       
                          NACE_se           = sd(NACE_bs,na.rm=T)  ,                          
                          NACE_bs_CI_l      = quantile(NACE_bs, c(0.025,0.975),na.rm = T)[1]    ,
                          NACE_bs_CI_u      = quantile(NACE_bs, c(0.025,0.975),na.rm = T)[2]   , 
                          NACE.adj          = mean(NACE.adj_bs,na.rm=T)       ,                 
                          NACE.adj_se       = sd(NACE.adj_bs,na.rm=T),                         
                          NACE.adj_CI_l     = quantile(NACE.adj_bs, c(0.025,0.975),na.rm = T)[1]  ,
                          NACE.adj_CI_u     = quantile(NACE.adj_bs, c(0.025,0.975),na.rm = T)[2] ,
                          
                          CAACE              = mean(CAACE_bs,na.rm=T) ,                         
                          CAACE_se           = sd(CAACE_bs,na.rm=T)  ,                          
                          CAACE_CI_l         = quantile(CAACE_bs, c(0.025,0.975),na.rm = T)[1] ,   
                          CAACE_CI_u         = quantile(CAACE_bs, c(0.025,0.975),na.rm = T)[2],    
                          CAACE.adj          = mean(CAACE.adj_bs,na.rm=T),
                          CAACE.adj_se       = sd(CAACE.adj_bs,na.rm=T),                         
                          CAACE.adj_CI_l     = quantile(CAACE.adj_bs, c(0.025,0.975),na.rm = T)[1], 
                          CAACE.adj_CI_u     = quantile(CAACE.adj_bs, c(0.025,0.975),na.rm = T)[2], 
                          
                          APCP              = mean(APCP_bs),
                          CPCP              = mean(CPCP_bs),
                          NPCP              = mean(NPCP_bs)
                          
                                                    
  )
  rownames(ace) <- NULL
  

 return(ace)
  
}
