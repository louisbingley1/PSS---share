# Create I

# Create matrix I[i,j] based on the following stratum definition 
# Note: Each stratum represent a unique combination of S(0),S(1) 
#       There are 4 unique combinations of S(0),S(1) : 01,11,00,10  
#---------------------------------------------------------------------------
# stratum  S(0) S(1)       stratum                                      
#---------------------------------------------------------------------------
#   1       0    1        Harmed(01)     /Control only compliers         
#   2       1    1        Doomed(11)     /Never-compliers                 
#   3       0    0        Immune(00)     /Always compliers                
#   4       1    0        Benefitters(10 /Experimental-only-compliers     
#---------------------------------------------------------------------------

f_I = function(dat){
  
    I = matrix(0,nrow = nrow(dat),ncol=4)
    for(i in 1:nrow(dat)){
      if(dat$Z[i]==1 & dat$S[i]==0){             # If observe S(1)=0 -> assign the subject to stratum=3/4  
        I[i,3]=1
        I[i,4]=1
      } else if(dat$Z[i]==1 & dat$S[i]==1){      # If observe S(1)=1 -> assign the subject to stratum=1/2 
        I[i,1]=1
        I[i,2]=1
      } else if(dat$Z[i]==0 & dat$S[i]==0){      # If observe S(0)=0 -> assign the subject to stratum=1/3
        I[i,1]=1
        I[i,3]=1
      } else{                                    # If observe S(0)=1 ->s assign the subject to tratum=2/4   
        I[i,2]=1
        I[i,4]=1
      }
    }
  
    return(I)  
  
}