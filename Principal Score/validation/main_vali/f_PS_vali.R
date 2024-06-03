
f_PS_vali = function(data, ep1, ep0,beta.a= NULL,beta.n=NULL, iter.max,error0){  
  
  N         =  nrow(data)
  X_in      =  as.matrix(data[, 1:5])     # data %>%  dplyr::select("X_1","X_2") %>% cbind(data %>% dplyr::select(BASE)) %>% as.matrix       
  X         =  cbind(rep(1, N), X_in)
  V         =  ncol(X)
  Z         =  data %>%  pull(Z) %>% as.numeric()   
  Y         =  data %>%  pull(Y)  
  D         =  data %>%  pull(D) 
  USUBJID   =  data %>%  pull(USUBJID)
  indexZD   =  data %>%  pull(indexZD)
  
  # pi      : computed from observed data
  pilist    = f_pi(Z,D)
  pi.n      = pilist$pi.n
  pi.a      = pilist$pi.a
  pi.c      = pilist$pi.c
  
  # betas:     beta.a, beta.n 
  #            coefficients of the weighted multinomial logistic regression model.
  #            compuyou3youted from EM algorithm.
  betalist  = f_EM_betas_vali( data,Z,D,X,N,V,USUBJID,beta.a = NULL,beta.n=NULL,iter.max,error0,Trace = T )
  beta.a    = betalist$beta.a
  beta.n    = betalist$beta.n
  AugData   = betalist$AugData # head(AugData);AugData %>% arrange(USUBJID);dim(AugData)

  # merge
  boot_     = data %>% distinct(Z,D,Y,U,USUBJID,indexZD)   # head(AugData);head(boot_)
  AugData   = left_join(AugData, boot_, by="USUBJID") 
  for(i in 1: nrow(AugData)){  AugData$Utrue[i] <- AugData$U[i] }# for(i in 1: nrow(AugData)){  AugData$Utrue[i] <- strsplit(AugData$U[i],"/")[[1]][3] }
   
 
  # ps score: computed from betas
  ps.score  = f_PROB_vali(AugData ,beta.a,beta.n)
  colnames(ps.score) = c("ps.score.1","ps.score.2","ps.score.3")
  AugData   = cbind.data.frame(AugData,ps.score);head(AugData)
  
  # W
  AugData   = f_w(AugData,pi.a,pi.c,pi.n,ep1,ep0);head(AugData)
 
   
  
  # PCP - prob of correct prediction
 # AugData   <- AugData %>% mutate( IU   = as.numeric(Uhat == Utrue),  PCP  = w_Uhat*IU) 
  head(AugData)
  
  # fit
  coefflist   = f_coeff_vali(AugData);  
  coeff_tb   = coefflist$coeff
  coeff.1c   = coefflist$coeff.1c
  coeff.0c   = coefflist$coeff.0c
  coeff.1a   = coefflist$coeff.1a
  coeff.0a   = coefflist$coeff.0a
  coeff.1n   = coefflist$coeff.1n
  coeff.0n   = coefflist$coeff.0n   
  coeff.1ca   = coefflist$coeff.1ca
  coeff.0ca   = coefflist$coeff.0ca
  
  AugData = left_join(AugData, coeff_tb,by="indexZU" );head(AugData)
  
  # ACEs
  
  if(pi.n>0 & pi.a>0 & pi.c>0){
    
    # CACE, NACE and AACE
    {
    AugData$Yw    = AugData$Y * AugData$w
    CACE          = mean(AugData %>% filter(PS==1 & Z==1) %>% pull(Yw)  , na.rm=T ) - mean(AugData %>% filter(PS==1 & Z==0) %>% pull(Yw) , na.rm=T )                                   
    AACE          = mean(AugData %>% filter(PS==2 & Z==1) %>% pull(Yw)  , na.rm=T ) - mean(AugData %>% filter(PS==2 & Z==0) %>% pull(Yw) , na.rm=T )                           
    NACE          = mean(AugData %>% filter(PS==3 & Z==1) %>% pull(Yw)  , na.rm=T ) - mean(AugData %>% filter(PS==3 & Z==0) %>% pull(Yw) , na.rm=T ) 
    CAACE         = mean(AugData %>% filter( (PS==1|PS==2) & (Z==1) ) %>% pull(Yw)  , na.rm=T ) - mean(AugData %>% filter((PS==1|PS==2) & (Z==0)) %>% pull(Yw) , na.rm=T )                           
    
    }
    
    # CACE.adj, NACE.adj and AACE.adj 
    {AugData       = AugData %>% mutate(
                           one_w = one * w,
                           X_1_w = X_1 * w,
                           X_2_w = X_2 * w,
                           X_3_w = X_3 * w,
                           X_4_w = X_4 * w,
                           X_5_w = X_5 * w
                           
    )
    AugData$Yhat  = AugData$coeff.intercept + 
                    AugData$X_1 * AugData$coeff.X_1 + 
                    AugData$X_2 * AugData$coeff.X_2 + 
                    AugData$X_3 * AugData$coeff.X_3 + 
                    AugData$X_4 * AugData$coeff.X_4 + 
                    AugData$X_5 * AugData$coeff.X_5  
    AugData$Rw    = AugData$w * (AugData$Y - AugData$Yhat )
    C.adj.data    = as.matrix( rbind.data.frame(AugData %>% filter(indexZU=='1c') %>% select(one_w,X_1_w,X_2_w,X_3_w,X_4_w,X_5_w),
                                                AugData %>% filter(indexZU=='0c') %>% select(one_w,X_1_w,X_2_w,X_3_w,X_4_w,X_5_w))
                              )
    A.adj.data    = as.matrix( rbind.data.frame(AugData %>% filter(indexZU=='1a') %>% select(one_w,X_1_w,X_2_w,X_3_w,X_4_w,X_5_w),
                                                AugData %>% filter(indexZU=='0a') %>% select(one_w,X_1_w,X_2_w,X_3_w,X_4_w,X_5_w))
    )
    N.adj.data    = as.matrix( rbind.data.frame(AugData %>% filter(indexZU=='1n') %>% select(one_w,X_1_w,X_2_w,X_3_w,X_4_w,X_5_w),
                                                AugData %>% filter(indexZU=='0n') %>% select(one_w,X_1_w,X_2_w,X_3_w,X_4_w,X_5_w))
                              )
    CA.adj.data    = as.matrix( rbind.data.frame(AugData %>% filter(indexZU=='1c' | indexZU=='1a' ) %>% select(one_w,X_1_w,X_2_w,X_3_w,X_4_w,X_5_w),
                                                 AugData %>% filter(indexZU=='0c' | indexZU=='0a' ) %>% select(one_w,X_1_w,X_2_w,X_3_w,X_4_w,X_5_w))
    )
    CACE.adj      = mean(AugData %>% filter(indexZU=='1c') %>% pull(Rw)) - mean(AugData %>% filter(indexZU=='0c') %>% pull(Rw)) + mean(C.adj.data %*% as.numeric(coeff.1c-coeff.0c) )
    AACE.adj      = mean(AugData %>% filter(indexZU=='1a') %>% pull(Rw)) - mean(AugData %>% filter(indexZU=='0a') %>% pull(Rw)) + mean(A.adj.data %*% as.numeric(coeff.1a-coeff.0a) )
    NACE.adj      = mean(AugData %>% filter(indexZU=='1n') %>% pull(Rw)) - mean(AugData %>% filter(indexZU=='0n') %>% pull(Rw)) + mean(N.adj.data %*% as.numeric(coeff.1n-coeff.0n) )
    CAACE.adj      = mean(AugData %>% filter(indexZU=='1c' | indexZU=='1a' ) %>% pull(Rw)) - mean(AugData %>% filter(indexZU=='0c' | indexZU=='0a') %>% pull(Rw)) + mean(CA.adj.data %*% as.numeric(coeff.1ca-coeff.0ca) )
    }
    
    # CPCP, NPCP,APCP
#   {
#     nC   = sum(AugData %>% filter(Utrue == "C") %>%pull(w_Uhat))
#      nN   = sum(AugData %>% filter(Utrue == "N") %>%pull(w_Uhat))
#      nA   = sum(AugData %>% filter(Utrue == "A") %>%pull(w_Uhat))
#      CPCP = sum( AugData %>% filter(Utrue == "C") %>% pull(PCP) )/nC;CPCP
#      NPCP = sum( AugData %>% filter(Utrue == "N") %>% pull(PCP) )/nN;NPCP
#      APCP = sum( AugData %>% filter(Utrue == "A") %>% pull(PCP) )/nA;APCP
#    }
    
    #results
    ACE = list( CACE   = CACE  , CACE.adj = CACE.adj, 
                NACE   = NACE  , NACE.adj = NACE.adj, 
                AACE   = AACE  , AACE.adj = AACE.adj,  
                CAACE  = CAACE , CAACE.adj = CAACE.adj,
                beta.a = beta.a, beta.n   = beta.n  ,
                pi.a   = pi.a  , pi.n     = pi.n    , pi.c = pi.c,
          #      nC     = nC    , nA  = nA, nN=nN, 
          #      CPCP = CPCP, NPCP = NPCP, APCP = APCP,
                AugData = AugData)
    
  }else{    
    stop(paste("At least one of pi.n,pi.a,pi.c has negative value: pi.n=",pilist[1],"pi.a=",pilist[2], "pi.c=",pilist[3]))
  }
  
  return( ACE )
  
}
