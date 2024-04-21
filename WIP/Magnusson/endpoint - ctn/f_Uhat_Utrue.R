f_Uhat =  function(data){
  for(i in 1:nrow(data)){
    if(data$S0_imp[i]==0 & data$S1_imp[i]==1){data$Uhat[i]="H"}else
      if(data$S0_imp[i]==1 & data$S1_imp[i]==1){data$Uhat[i]="D"}else
        if(data$S0_imp[i]==0 & data$S1_imp[i]==0){data$Uhat[i]="I"}else
          if(data$S0_imp[i]==1 & data$S1_imp[i]==0){data$Uhat[i]="B"} 
  }
  return(data)
}

f_Utrue = function(data){
  for(i in 1:nrow(data)){
    if(data$U[i]=="AdhereToPBO/01/N"){data$Utrue[i]="H"}else
      if(data$U[i]=="NeverAdhere/11/D"){data$Utrue[i]="D"}else
        if(data$U[i]=="AlwaysAdhere/00/C"){data$Utrue[i]="I"}else
          if(data$U[i]=="AdhereToACT/10/A"){data$Utrue[i]="B"} 
            
  }
  return(data)
}

