
# w - weight
f_w = function(AugData,pi.a,pi.c,pi.n,ep1,ep0){
  
   P = AugData %>% select(ps.score.1,ps.score.2,ps.score.3) 
 
    for(i in 1:nrow(AugData)){

        if(AugData$indexZU[i]=="1c"){
                                     AugData$w_Uhat[i] = ep1*P[i,1]/(ep1*P[i,1]+P[i,2]); 
                                     AugData$w_adj[i]  = pi.c/(pi.c+pi.a); 
                                     AugData$w[i]      = AugData$w_Uhat[i]/AugData$w_adj[i]
                                
                                      }else
        if(AugData$indexZU[i]=="1a"){
                                     AugData$w_Uhat[i] = P[i,2]/(ep1*P[i,1]+P[i,2]); 
                                     AugData$w_adj[i]  = pi.a/(pi.c+pi.a); 
                                     AugData$w[i]      = AugData$w_Uhat[i]/AugData$w_adj[i]
                                      
                                     }else
        if(AugData$indexZU[i]=="0c"){
                                     AugData$w_Uhat[i] = ep0*P[i,1]/(ep0*P[i,1]+P[i,3]); 
                                     AugData$w_adj[i]  = pi.c/(pi.c+pi.n);
                                     AugData$w[i]      = AugData$w_Uhat[i]/AugData$w_adj[i]
                                      
                                     }else
        if(AugData$indexZU[i]=="0n"){
                                     AugData$w_Uhat[i] = P[i,3]/(ep0*P[i,1]+P[i,3]); 
                                     AugData$w_adj[i]  = pi.n/(pi.c+pi.n); 
                                     AugData$w[i]      = AugData$w_Uhat[i]/AugData$w_adj[i]
                                      
                                     }else{
                                       
                                     AugData$w_Uhat[i] = 1 
                                     AugData$w_adj[i]  = 1
                                     AugData$w[i]      = AugData$w_Uhat[i]/AugData$w_adj[i]
                                
                                     }
      
   }
  
    
return(AugData)
} 

#f_w = function(index11,index00,ps.score,pi.a,pi.c,pi.n,ep1,ep0){
#  w1c = ep1*ps.score[index11, 1]/(ep1*ps.score[index11, 1] + ps.score[index11, 2])/pi.c*(pi.c + pi.a)
#  w0c = ep0*ps.score[index00, 1]/(ep0*ps.score[index00, 1] + ps.score[index00, 3])/pi.c*(pi.c + pi.n)
#  w0n =     ps.score[index00, 3]/(ep0*ps.score[index00, 1] + ps.score[index00, 3])/pi.n*(pi.c + pi.n)
#  w1a =     ps.score[index11, 2]/(ep1*ps.score[index11, 1] + ps.score[index11, 2])/pi.a*(pi.c + pi.a)
#  return(list(w1c=w1c,w0c=w0c,w1n=1,w0n=w0n,w1a=w1a,w0a=1))
#} 
