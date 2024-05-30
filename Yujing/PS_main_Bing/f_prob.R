
# compute principal score (matrix PROB) - A function of betas
f_PROB=function(AugData,beta.a,beta.n){
  
  N    = nrow(AugData)
  X    = as.matrix(AugData[,2:5])
  PROB = matrix(0, N, 3)
  for(i in 1:N) { 
                  prob.c   = 1
                  prob.a   = exp(t(beta.a)%*%X[i, ])
                  prob.n   = exp(t(beta.n)%*%X[i, ])
                  sum      = prob.c + prob.a + prob.n
                  PROB[i,] = c(prob.c/sum, prob.a/sum, prob.n/sum ) 
                }
  return(PROB)

}

 