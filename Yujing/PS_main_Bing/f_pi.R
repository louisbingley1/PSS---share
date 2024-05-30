
# compute pi's - membership proportion from observed Z,D
f_pi=function(Z,D){
  pi.n = sum(Z*(1 - D))/sum(Z)
  pi.a = sum((1 - Z)*D)/sum(1-Z)
  pi.c = 1 - pi.n - pi.a
  return(list(pi.n=pi.n, pi.a=pi.a,pi.c=pi.c))
}