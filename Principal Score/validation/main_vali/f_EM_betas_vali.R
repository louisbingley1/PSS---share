# EM Algorithm to compute betas - the coeff in weighted multinomial logreg model: strata ~ X+BASE
f_EM_betas_vali=function(data,Z,D,X,N,V,USUBJID,beta.a = beta.a,beta.n=beta.n,iter.max,error0 ,Trace ){
  
  # initial betas (will give the initial w = .5 for splitted subjects.)
  if(is.null(beta.a)){beta.a = rep(0, V)}
  if(is.null(beta.n)){beta.n = rep(0, V)}
  
  iter = 1   
  
  # EM  
  repeat{ if(Trace == T) {print(paste("The ", iter, "-th EM iteration!", sep=""))}
    
    # EM starts from initial betas --> 0 
    beta.a_old = beta.a 
    beta.n_old = beta.n
    
    # betas --> w_iter
    AugData    = f_augdata_vali(data,Z,D,X,beta.a_old,beta.n_old,USUBJID) ; 
     
    # w_iter --> new betas
    augdata    = as.matrix(AugData[,1:9])
    fit        = multinom(augdata[, 1] ~ augdata[, (3:(V+1))], weights = augdata[, (V+2)], trace = FALSE)   # Update betas using the coeff from Multinomial logistic regression model /using "nnet" package
    betas      = coef(fit);betas
    beta.a     = betas[1, ]
    beta.n     = betas[2, ]
    
    # break check
    error      = sum((beta.a - beta.a_old)^2)  + sum((beta.n - beta.n_old)^2);error
    iter       = iter + 1
    if(iter>iter.max||error<error0)   break        
    
  }
  
  return(list(beta.a=beta.a,beta.n=beta.n,AugData=AugData ))
  
}

