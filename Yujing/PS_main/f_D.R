f_D = function(data){
  for(i in 1:nrow(data)){
    if(data$TRT[i]==1){
        if(data$U[i]=='AlwaysAdhere/00/C'){data$D[i]=1}else{
        if(data$U[i]=='AdhereToPBO/01/N'){data$D[i]=0}else{
        if(data$U[i]=='AdhereToACT/10/A'){data$D[i]=1}else{
        if(data$U[i]=='NeverAdhere/11/D'){data$D[i]=0}
        }}}
    }else if(data$TRT[i]==0){
        if(data$U[i]=='AlwaysAdhere/00/C'){data$D[i]=0}else{
        if(data$U[i]=='AdhereToPBO/01/N'){data$D[i]=0}else{
        if(data$U[i]=='AdhereToACT/10/A'){data$D[i]=1}else{
        if(data$U[i]=='NeverAdhere/11/D'){data$D[i]=1}
        }}}
    }
  }
  return(data)
}
