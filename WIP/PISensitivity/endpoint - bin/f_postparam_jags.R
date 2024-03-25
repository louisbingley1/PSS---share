# Function to compute postparam

f_postparam_jags = function(dat.jags,inits,parSave,text,n.chains,n.iter,n.burnin,thin){
  
  # <1> run jags()

  PS=jags(data               = dat.jags, 
          inits              = inits,
          parameters.to.save = parSave,
          model.file         = textConnection(text),
          n.chains           = n.chains,
          n.iter             = n.iter,
          n.burnin           = n.burnin ,
          n.thin             = thin )
  PS
  
  # <2> Save posterior estimates to a list 'postparam_chainlist' with length eqs nchains
  # Note: In each list element of 'postparam_chainlist':
  #       - rows   : iteration id , nrows =  (n.iter-n.burnin)/thin
  #       - columns: as requested in parSave     

  postparam_chainlist = as.mcmc(PS) 
  
  # <3> rbind nchains lists in 'postparam_chainlist' to 1 dataframe 'postparam'
  # Note: 
  #       - rows   : iteration id of nchains , nrows =  ( (n.iter-n.burnin)/thin )* nchains
  #       - columns: as requested in parSave     

  postparam  = NULL;
  for(j in 1:n.chains ){ 
    postparam=rbind.data.frame(postparam,postparam_chainlist[[j]]) 
  };  #dim(postparam)
  
  return(postparam)
}  
