
f_U = function(A0,A1){
  if(A0==0 & A1==0){U='NeverAdhere/11/D'}else
    if(A0==1 & A1==1){U='AlwaysAdhere/00/C'}else
      if(A0==0 & A1==1){U='AdhereToACT/10/A'}else
        if(A0==1 & A1==0){U='AdhereToPBO/01/N'}
  return(U)
}
