
f_coeff_vali=function(AugData){
  
  data1c = AugData %>% filter(indexZU=='1c')
  data0c = AugData %>% filter(indexZU=='0c')
  data1a = AugData %>% filter(indexZU=='1a')
  data0a = AugData %>% filter(indexZU=='0a')
  data1n = AugData %>% filter(indexZU=='1n')
  data0n = AugData %>% filter(indexZU=='0n')
  data1ca =  AugData %>% filter(indexZU=='1c'|indexZU=='1a')
  data0ca =  AugData %>% filter(indexZU=='0c'|indexZU=='0a')
  
  
  coeff.1c=lm(Y ~ 1 + X_1 + X_2 + X_3 + X_4 + X_5, weights=w, data = data1c)$coefficients
  coeff.0c=lm(Y ~ 1 + X_1 + X_2 + X_3 + X_4 + X_5, weights=w, data = data0c)$coefficients
  coeff.1a=lm(Y ~ 1 + X_1 + X_2 + X_3 + X_4 + X_5, weights=w, data = data1a)$coefficients
  coeff.0a=lm(Y ~ 1 + X_1 + X_2 + X_3 + X_4 + X_5, weights=w, data = data0a)$coefficients
  coeff.1n=lm(Y ~ 1 + X_1 + X_2 + X_3 + X_4 + X_5, weights=w, data = data1n)$coefficients
  coeff.0n=lm(Y ~ 1 + X_1 + X_2 + X_3 + X_4 + X_5, weights=w, data = data0n)$coefficients
  coeff.1ca=lm(Y ~ 1 + X_1 + X_2 + X_3 + X_4 + X_5, weights=w, data = data1ca)$coefficients
  coeff.0ca=lm(Y ~ 1 + X_1 + X_2 + X_3 + X_4 + X_5, weights=w, data = data0ca)$coefficients
  
  coeff = cbind.data.frame(indexZU         = c('1c','0c','1a','0a','1n','0n'),
                           coeff.intercept = c(coeff.1c[1], coeff.0c[1], coeff.1a[1],coeff.0a[1], coeff.1n[1], coeff.0n[1]),
                           coeff.X_1       = c(coeff.1c[2], coeff.0c[2], coeff.1a[2],coeff.0a[2], coeff.1n[2], coeff.0n[2]),
                           coeff.X_2       = c(coeff.1c[3], coeff.0c[3], coeff.1a[3],coeff.0a[3], coeff.1n[3], coeff.0n[3]),
                           coeff.X_3       = c(coeff.1c[4], coeff.0c[4], coeff.1a[4],coeff.0a[4], coeff.1n[4], coeff.0n[4]),
                           coeff.X_4       = c(coeff.1c[5], coeff.0c[5], coeff.1a[5],coeff.0a[5], coeff.1n[5], coeff.0n[5]),
                           coeff.X_5       = c(coeff.1c[6], coeff.0c[6], coeff.1a[6],coeff.0a[6], coeff.1n[6], coeff.0n[6]),
                           nsubjZU         = c(  nrow(data1c), nrow(data0c),nrow(data1a),nrow(data0a),nrow(data1n),nrow(data0n) )
  )
    
  coefflist=list(coeff    = coeff, 
                 coeff.1c = coeff.1c,
                 coeff.0c = coeff.0c,
                 coeff.1a = coeff.1a,
                 coeff.0a = coeff.0a,
                 coeff.1n = coeff.1n,
                 coeff.0n = coeff.0n,
                 coeff.1ca = coeff.1ca,
                 coeff.0ca = coeff.0ca
  )
  return(coefflist)
}
