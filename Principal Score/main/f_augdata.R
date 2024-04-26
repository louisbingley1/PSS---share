# create AugData, put each subject into corresponding strata based on observed Z,D
f_augdata = function(data,Z,D,X,beta.a_old,beta.n_old,USUBJID){   
                      
  AugData = NULL
  for(i in 1: nrow(data)) {
    if(Z[i]==1 & D[i]==1) {
      #posterior probabilities
      prob.c  = 1/(1 + exp(t(beta.a_old)%*%X[i, ]))
      prob.a  = 1 - prob.c
      AugData = rbind.data.frame(AugData, c(PS=1, X[i, ], w_iter = prob.c, USUBJID = USUBJID[i], Uhat = "C",  indexZU="1c"))
      AugData = rbind.data.frame(AugData, c(PS=2, X[i, ], w_iter = prob.a, USUBJID = USUBJID[i], Uhat = "A",  indexZU="1a"))
    }else if(Z[i]==1 & D[i]==0) { 
      AugData = rbind.data.frame(AugData, c(PS=3, X[i, ], w_iter = 1     , USUBJID = USUBJID[i], Uhat = "N",  indexZU="1n"))  
    }else if(Z[i]==0 & D[i]==1) { 
      AugData = rbind.data.frame(AugData, c(PS=2, X[i, ], w_iter = 1     , USUBJID = USUBJID[i], Uhat = "A",  indexZU="0a"))  
    }else if(Z[i]==0 & D[i]==0) {
      #posterior probabilities
      prob.c  = 1/(1 + exp(t(beta.n_old)%*%X[i, ]))
      prob.n  = 1 - prob.c
      AugData = rbind.data.frame(AugData, c(PS=1, X[i, ], w_iter = prob.c,USUBJID = USUBJID[i], Uhat = "C", indexZU="0c"))
      AugData = rbind.data.frame(AugData, c(PS=3, X[i, ], w_iter = prob.n,USUBJID = USUBJID[i], Uhat = "N",  indexZU="0n"))  
    } 
  }
  colnames(AugData)<-c("PS","one","X_1","X_2","BASE","w_iter","USUBJID","Uhat","indexZU")
  
  AugData$PS     = as.numeric(AugData$PS)
  AugData$one    = as.numeric(AugData$one)
  AugData$X_1    = as.numeric(AugData$X_1)
  AugData$X_2    = as.numeric(AugData$X_2)
  AugData$BASE   = as.numeric(AugData$BASE)
  AugData$w_iter = as.numeric(AugData$w_iter)
  AugData$USUBJID     = as.numeric(AugData$USUBJID)
 
  return(AugData)
} 
