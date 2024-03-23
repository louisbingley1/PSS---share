f_postparam_jags = function(dat.jags,inits,parSave,text,n.chains,n.iter,n.burnin,thin){
  
  # <1> run jags()
  #     Note: jags() works for
  #     - k chains, k inits (k>=1) | inits is a func [applied]
  #     - 1 chain , 1 inits        | inits is a list         
  #     - k chains, k inits (k>1)  | inits is a list of list < works when length(inits) == n.chains (k) >
  
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
  #     In each list element of 'postparam_chainlist':
  #     - rows   : iteration id                              |  nrows =  (n.iter-n.burnin)/thin
  #     - columns: alpha[j], beta[j], S1[i], Y0[i] deviance  |  i=1:n, j=1:3, ncols=2n+6+1
  postparam_chainlist = as.mcmc(PS) 
  
  # <3> rbind nchains lists in 'postparam_chainlist' to 1 dataframe 'postparam'
  #     nrows = ( (n.iter-n.burnin)/thin ) * n.chains
  #     ncols = 2n+6+1
  postparam  = NULL;
  for(j in 1:n.chains ){ 
    postparam=rbind.data.frame(postparam,postparam_chainlist[[j]]) 
  };  #dim(postparam)
  
  return(postparam)
}  
