
f_postparam_jags.model =  function(file,dat.jags,inits,n.chains,n.adapt,parSave,n.iter,thin){
  
  # jags.model() works for
  # k chains, k inits (k>=1)  | inits is a func [applied]
  # 1 chain , 1 inits         | inits is a list 
  # Note: 
  # inits cannot be a list of list.
  
  # <1> run jags.model()
  mod = jags.model(file, dat.jags, inits, n.chains , n.adapt, quiet = T)
  
  # <2> Save posterior estimates to a list 'postparam_chainlist' with length eqs nchains
  # In each list element of 'postparam_chainlist2':
  # - rows   : iteration id                     |  nrows =  (n.iter)/thin
  # - columns: alpha[j], beta[j], S1[i], Y0[i]  |  i=1:n, j=1:3, ncols = 2n+6  
  postparam_chainlist2  = coda.samples(mod, parSave, n.iter, thin  )  # length(postparam_chainlist2); dim(postparam_chainlist2[[1]])
  
  # <3> rbind nchains lists in 'postparam_chainlist2' to 1 dataframe 'postparam2'
  # nrows = ( n.iter/thin ) * n.chains
  # ncols = 2n+6
  postparam2  = NULL;
  for(j in 1:n.chains ){ 
    postparam2=rbind.data.frame(postparam2,postparam_chainlist2[[j]]) 
  };  #dim(postparam2)
  
  
  return(postparam2)
} 