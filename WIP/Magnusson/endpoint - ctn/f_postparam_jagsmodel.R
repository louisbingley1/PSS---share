f_postparam_jags.model =  function(file,dat.jags,inits,n.chains,n.adapt,parSave,n.iter,thin){
  
  # <1> run jags.model()
  
  mod = jags.model(file, dat.jags, inits, n.chains , n.adapt, quiet = T)
  
  # <2> Save posterior estimates to a list 'postparam_chainlist' with length eqs nchains
  # Note: In each list element of 'postparam_chainlist':
  #       - rows   : iteration id , nrows =  (n.iter-n.burnin)/thin
  #       - columns: as requested in parSave     
  
  postparam_chainlist2  = coda.samples(mod, parSave, n.iter, thin  )  # length(postparam_chainlist2); dim(postparam_chainlist2[[1]])
  
  # <3> rbind nchains lists in 'postparam_chainlist' to 1 dataframe 'postparam'
  # Note: 
  #       - rows   : iteration id of nchains , nrows =  ( (n.iter-n.burnin)/thin )* nchains
  #       - columns: as requested in parSave     
  
    postparam2  = NULL;
  for(j in 1:n.chains ){ 
    postparam2=rbind.data.frame(postparam2,postparam_chainlist2[[j]]) 
  };  #dim(postparam2)
  
  
  return(postparam2)
} 
