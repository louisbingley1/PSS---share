#' Multiple robust estimators of average causal effects within principal strata (pace)
#'
#' Implements various estimators of PACE from randomized experiments and observational studies
#' @param X is a matrix of pre-treatment covariates without intercept  (n x p).
#' @param Z is a vector of treatment  (n x 1).
#' @param S is a vector of binary intermidiate outcome  (n x 1).
#' @param surv_time is observed failure time U = min(T,C), T: death time; C: censor time.
#' @param surv_status is censoring indicator delta = I(T <= C),  1: death, 0: censor.
#' @param u is target time for causal effect delta(u) = S1(u) - S0(u).
#' @param family specifies the family for the outcome model.
#' \code{"time"}: a survival cox proportional hazard model for the time-to-event outcome.
#'
#' @param nboot is the number of bootstrap samples.
#'
#' @return
#'
#' \itemize{
#'
#' \item \code{tau10w}: a principal score weighting estimator of tau10
#' \item \code{tau10sw}: a stabilized weighting estimator of tau10
#' \item \code{tau10reg}: a regression estimator of tau10 using outcome mean regression and inverse probability of treatment weighting
#' \item \code{tau10reg2}: a regression estimator of tau10 using outcome mean and principal score regression
# \item \code{tau10aw}: a triply robust estimator of tau10
#' \item bootstrap variance estimator
#' \itemize{
#' \item \code{ve.tau10w}
#' \item \code{ve.tau10sw}
#' \item \code{ve.tau10reg}
#' \item \code{ve.tau10reg2}
# \item \code{ve.tau10aw}
#' }
#'
#' \item \code{tau00w}: a principal score weighting estimator of tau00
#' \item \code{tau00sw}: a stabilized weighting estimator of tau00
#' \item \code{tau00reg}: a regression estimator of tau00 using outcome mean regression and inverse probability of treatment weighting
#' \item \code{tau00reg2}: a regression estimator of tau00 using outcome mean and principal score regression
# \item \code{tau00aw}: a triply robust estimator of tau00
#' \item bootstrap variance estimator
#' \itemize{
#' \item \code{ve.tau00w}
#' \item \code{ve.tau00sw}
#' \item \code{ve.tau00reg}
#' \item \code{ve.tau00reg2}
# \item \code{ve.tau00aw}
#' }
#'
#' \item \code{tau11w}: a principal score weighting estimator of tau11
#' \item \code{tau11sw}: a stabilized weighting estimator of tau11
#' \item \code{tau11reg}: a regression estimator of tau11 using outcome mean regression and inverse probability of treatment weighting
#' \item \code{tau11reg2}: a regression estimator of tau11 using outcome mean and principal score regression
# \item \code{tau11aw}: a triply robust estimator of tau11
#' \item bootstrap variance estimator
#' \itemize{
#' \item \code{ve.tau11w}
#' \item \code{ve.tau11sw}
#' \item \code{ve.tau11reg}
#' \item \code{ve.tau11reg2}
# \item \code{ve.tau11aw}
#' }
#' }
#'
#' @details
#' Details will be provided in the reference paper.
#'
#' @import stats
#'
#' @references
#'
#'A reference paper will come up soon.
#'
#' @examples
#'
#' library(stats)
#'
#' set.seed(1)
#'#X consists of 5 covariates
#'n <- 500
#'X <- rnorm(n,0.25,1)
#'for(k in 2:4){
#'  X <- cbind(X,rnorm(n,0.25,1))
#'}
#'X <- cbind(X,rbinom(n,1,0.5))
#'
#'# treatment assignment
#'Xtilde1 <- (X-0.25)/1
#'theta <- 0
#'eta <- c(0,1,1,1,1,theta)/2.5
#'px0 <- exp(cbind(1,Xtilde1)%*%eta)
#'pix  <- px0/(1+px0)
#'Z   <- rbinom(n,1,pix)
#'
#'# S-model
#'eta1 <- c( 2,-1,1,-1,1,theta)/2.5
#'eta0 <- c(-2,1,-1,1,-1,theta)/2.5
#'p1xtemp <- exp(cbind(1,Xtilde1)%*%eta1)
#'p0xtemp <- exp(cbind(1,Xtilde1)%*%eta0)
#'p1x <- p1xtemp/(1+p1xtemp)
#'p0x <- p0xtemp/(1+p0xtemp)
#'s1<-rbinom(n,1,p1x)
#'s0<-rbinom(n,1,p0x)
#'S<-s1*Z+s0*(1-Z)
#'
#'# Y-model (continuous outcome)
#'Y1<- Xtilde1%*%rep(1,5)*(1+S+Z) +rnorm(n)
#'# Y-model (binary outcome)
#'leYtemp <- Xtilde1%*%rep(1,5)*(1+S+Z)/4
#'eY<-exp(leYtemp)/(1+exp(leYtemp) )
#'Y2<-rbinom(n,1,eY)
#'
#'out1<-pace(X,Z,S,Y1,family.Y="gaussian",nboot=50)
#'out1$tau10aw
#'out1$ve.tau10aw
#'
#'out2<-pace(X,Z,S,Y2,family.Y="binomial",nboot=50)
#'out2$tau10aw
#'out2$ve.tau10aw
#' @export

pace_time<-function(X, Z, S, 
                    surv_time, 
                    surv_status,
                    u,
                    family = "time",
                    nboot=0){

  n<-length(Z)
  est.out<-estforboot_time(X, Z, S, 
                           surv_time, 
                           surv_status,
                           u,
                           family)
  tau10w <- est.out$tau10w
  tau10reg <- est.out$tau10reg
  tau10reg2 <- est.out$tau10reg2
  tau10sw <- est.out$tau10sw
  # tau10aw <- est.out$tau10aw
  tau00w <- est.out$tau00w
  tau00reg <- est.out$tau00reg
  tau00reg2 <- est.out$tau00reg2
  tau00sw <- est.out$tau00sw
  # tau00aw <- est.out$tau00aw
  tau11w <- est.out$tau11w
  tau11reg <- est.out$tau11reg
  tau11reg2 <- est.out$tau11reg2
  tau11sw <- est.out$tau11sw
  # tau11aw <- est.out$tau11aw

  if(nboot>0){
    tau10w.boot<- rep(NA,nboot)
    tau10reg.boot<- rep(NA,nboot)
    tau10reg2.boot<- rep(NA,nboot)
    tau10sw.boot<- rep(NA,nboot)
    # tau10aw.boot<- rep(NA,nboot)
    
    tau00w.boot<- rep(NA,nboot)
    tau00reg.boot<- rep(NA,nboot)
    tau00reg2.boot<- rep(NA,nboot)
    tau00sw.boot<- rep(NA,nboot)
    # tau00aw.boot<- rep(NA,nboot)
    
    tau11w.boot<- rep(NA,nboot)
    tau11reg.boot<- rep(NA,nboot)
    tau11reg2.boot<- rep(NA,nboot)
    tau11sw.boot<- rep(NA,nboot)
    # tau11aw.boot<- rep(NA,nboot)
    
    for(kk in 1:nboot){
      bootsample<-sample.int(n,size=n,replace=TRUE)
      
      Z.boot<-Z[bootsample]
      X.boot<-X[bootsample,]
      S.boot<-S[bootsample]
      
      surv_time.boot <- surv_time[bootsample]
      surv_status.boot <- surv_status[bootsample]
      u.boot <- u[bootsample]
      
      est.out<-estforboot_time(X.boot, Z.boot, S.boot, 
                               surv_time.boot, 
                               surv_status.boot,
                               u.boot,
                               family)
      
      tau10w.boot[kk]<- est.out$tau10w
      tau10reg.boot[kk]<- est.out$tau10reg
      tau10reg2.boot[kk]<- est.out$tau10reg2
      tau10sw.boot[kk]<- est.out$tau10sw
      # tau10aw.boot[kk]<- est.out$tau10aw
      
      tau00w.boot[kk]<- est.out$tau00w
      tau00reg.boot[kk]<- est.out$tau00reg
      tau00reg2.boot[kk]<- est.out$tau00reg2
      tau00sw.boot[kk]<- est.out$tau00sw
      # tau00aw.boot[kk]<- est.out$tau00aw
      
      tau11w.boot[kk]<- est.out$tau11w
      tau11reg.boot[kk]<- est.out$tau11reg
      tau11reg2.boot[kk]<- est.out$tau11reg2
      tau11sw.boot[kk]<- est.out$tau11sw
      # tau11aw.boot[kk]<- est.out$tau11aw
    }
    ve.tau10w<- stats::var(tau10w.boot,na.rm=TRUE)
    ve.tau10reg<- stats::var(tau10reg.boot,na.rm=TRUE)
    ve.tau10reg2<- stats::var(tau10reg2.boot,na.rm=TRUE)
    ve.tau10sw<- stats::var(tau10sw.boot,na.rm=TRUE)
    # ve.tau10aw<- stats::var(tau10aw.boot,na.rm=TRUE)
    
    ve.tau00w<- stats::var(tau00w.boot,na.rm=TRUE)
    ve.tau00reg<-stats::var(tau00reg.boot,na.rm=TRUE)
    ve.tau00reg2<- stats::var(tau00reg2.boot,na.rm=TRUE)
    ve.tau00sw<- stats::var(tau00sw.boot,na.rm=TRUE)
    # ve.tau00aw<- stats::var(tau00aw.boot,na.rm=TRUE)
    
    ve.tau11w<- stats::var(tau11w.boot,na.rm=TRUE)
    ve.tau11reg<- stats::var(tau11reg.boot,na.rm=TRUE)
    ve.tau11reg2<- stats::var(tau11reg2.boot,na.rm=TRUE)
    ve.tau11sw<- stats::var(tau11sw.boot,na.rm=TRUE)
    # ve.tau11aw<- stats::var(tau11aw.boot,na.rm=TRUE)
  }

  if(nboot==0){
    ve.tau10w <- NA
    ve.tau10reg <- NA
    ve.tau10reg2 <- NA
    ve.tau10sw <- NA
    # ve.tau10aw <- NA
    ve.tau00w <- NA
    ve.tau00reg <-NA
    ve.tau00reg2 <- NA
    ve.tau00sw <- NA
    # ve.tau00aw <- NA
    ve.tau11w <- NA
    ve.tau11reg <- NA
    ve.tau11reg2 <- NA
    ve.tau11sw <- NA
    # ve.tau11aw <- NA
  }
  return( list(tau10w=tau10w,tau10sw=tau10sw,
               tau10reg=tau10reg,tau10reg2=tau10reg2,
               # tau10aw=tau10aw,
               tau00w=tau00w,tau00sw=tau00sw,
               tau00reg=tau00reg,tau00reg2=tau00reg2,
               # tau00aw=tau00aw,
               tau11w=tau11w,tau11sw=tau11sw,
               tau11reg=tau11reg,tau11reg2=tau11reg2,
               # tau11aw=tau11aw,
               ve.tau10w=ve.tau10w,ve.tau10sw=ve.tau10sw,
               ve.tau10reg=ve.tau10reg,ve.tau10reg2=ve.tau10reg2,
               # ve.tau10aw=ve.tau10aw,
               ve.tau00w=ve.tau00w,ve.tau00sw=ve.tau00sw,
               ve.tau00reg=ve.tau00reg,ve.tau00reg2=ve.tau00reg2,
               # ve.tau00aw=ve.tau00aw,
               ve.tau11w=ve.tau11w,ve.tau11sw=ve.tau11sw,
               ve.tau11reg=ve.tau11reg,ve.tau11reg2=ve.tau11reg2
               # ve.tau11aw=ve.tau11aw
               )
  )
}


#' estforboot (estforboot)
#'
#' A function calculates all estimators
#' @param X is a matrix of pre-treatment covariates without intercept  (n x p).
#' @param Z is a vector of treatment  (n x 1).
#' @param S is a vector of binary intermidiate outcome  (n x 1).
#' @param surv_time is observed failure time U = min(T,C), T: death time; C: censor time.
#' @param surv_status is censoring indicator delta = I(T <= C),  1: death, 0: censor.
#' @param u is target time for causal effect delta(u) = S1(u) - S0(u).
#' @param family specifies the family for the outcome model.
#' \code{"gaussian"}: a linear regression model for the continuous outcome.
#'
#' \code{"binomial"}: a logistic regression model for the binary outcome.
#' @return all point estimators
#' @import stats
#'
#' @export
#'
estforboot_time <- function(X, Z, S, 
                            surv_time, 
                            surv_status,
                            u,
                            family = "time"){
  
  X <- as.matrix(X)
  n <- length(Z)

  loc.z0  <- which(Z==0)
  loc.z1  <- which(Z==1)
  loc.s0  <- which(S==0)
  loc.s1  <- which(S==1)

  #predict p1(X)=P(S=1|Z=1,X)
  newx<-cbind(1,X)
  thisy<-S[loc.z1]
  thisx<-X[loc.z1,]
  glm1<-stats::glm(thisy~thisx,family = "binomial" )
  glm.coeff<-glm1$coefficients
  loc.na<-which(is.na(glm.coeff))
  if(length(loc.na)>0){glm.coeff[loc.na]<-0}
  lp1x <- newx%*%glm.coeff
  p1x <-exp(lp1x)/(1+exp(lp1x) )
  p1x <- as.vector(p1x)

  #predict p0(X)=P(S=1|Z=0,X)
  thisy<-S[loc.z0]
  thisx<-X[loc.z0,]
  glm1<-stats::glm(thisy~thisx,family = "binomial" )
  glm.coeff<-glm1$coefficients
  loc.na<-which(is.na(glm.coeff))
  if(length(loc.na)>0){glm.coeff[loc.na]<-0}
  lp0x <- newx%*%glm.coeff
  p0x <-exp(lp0x)/(1+exp(lp0x) )
  p0x <- as.vector(p0x)

  if(family=="time"){
    
    # indicator I(U >= u)
    surv_indi <- (surv_time >= u)
    data_pred_surv <- data.frame(X, this_surv_time = rep(u, n), this_surv_status = surv_status)
    data_pred_censor <- data.frame(X, this_censor_time = rep(u, n), this_censor_status = 1 - surv_status)
    
    
    ## Observed strata: Z=1, S=1
    {
      # newx<-cbind(1,X)
      temp<-which( (Z==1)&(S==1) )
      thisx<-X[temp, ]
      
      this_surv_time <- surv_time[temp]
      this_surv_status <- surv_status[temp]
      this_censor_time <- surv_time[temp]
      this_censor_status <- 1 - surv_status[temp]
      
      
      #predict S_11^T(X)=P(C>= u|X,Z=1,S=1) 
      data_surv <- data.frame(thisx, this_surv_time, this_surv_status)
      surv_model <- survival::coxph(survival::Surv(this_surv_time, this_surv_status) ~ ., data_surv)
      # surv_model_coef <-  surv_model$coefficients
      
      #calculate the cumulative hazard function
      # method 1
      # temp_data <- data_surv[1, 1:ncol(thisx)]*0
      # surv_model_base <- survival::survfit(surv_model, newdata = temp_data)
      # method 2
      # temp1 <- survival::basehaz(surv_model, newdata = data.frame(X1 = 0,
      #                                                             X2 = 0,
      #                                                             X3 = 0,
      #                                                             X4 = 0,
      #                                                             X5 = 0))
      
      # predict part: 
      surv_preds_11 <- predict(surv_model, newdata = data_pred_surv, type = "survival", se.fit = F)
      # temp_data <- data_surv
      # temp_data$this_surv_time <- rep(u, nrow(temp_data))
      # surv_preds_11 <- predict(surv_model, newdata = temp_data, type = "survival", se.fit = F)
      # surv_est_u[temp] <-  surv_preds_11
      
      
      
      # The following part is calculate the true value of the survival function
      # True <- exp(-u*exp(-1+0.5-0.3*temp_data$X4 + 0.2 * temp_data$X5))
      # True[1]
      # plot(True - surv_preds_11$fit)
      
      # The following part is to try another prediction method - wrong
      # cumhaz_11 <- data.frame(time = surv_model_base$time,
      #                         cumhaz = surv_model_base$cumhaz)
      # base_intens_u <- sapply(u, lambda0_fn, b = 1, surv = cumhaz_11)
      # temp1 <- exp( - base_intens_u * exp(thisx %*% surv_model_coef))
      # plot(temp1, surv_preds_11$fit)
      
      
      #predict S_11^C(X)=P(C>= u|X,Z=1,S=1)
      data_censor <- data.frame(thisx, this_censor_time, this_censor_status)
      censor_model <- survival::coxph(survival::Surv(this_censor_time, this_censor_status) ~ ., data_censor)
      
      # predict part: 
      censor_preds_11 <- predict(censor_model, newdata = data_pred_censor, type = "survival", se.fit = F)
      # temp_data <- data_censor
      # temp_data$this_censor_time <- rep(u, nrow(temp_data))
      # censor_preds_11 <- predict(censor_model, newdata = temp_data, type = "survival", se.fit = F)
      # censor_est_u[temp] <-  censor_preds_11
      
    }
    
    ## Observed strata: Z=0, S=0
    {
      # newx<-cbind(1,X)
      temp<-which( (Z==0)&(S==0) )
      thisx<-X[temp, ]
      
      this_surv_time <- surv_time[temp]
      this_surv_status <- surv_status[temp]
      this_censor_time <- surv_time[temp]
      this_censor_status <- 1 - surv_status[temp]
      
      
      #predict S_11^T(X)=P(C>= u|X,Z=1,S=1) 
      data_surv <- data.frame(thisx, this_surv_time, this_surv_status)
      surv_model <- survival::coxph(survival::Surv(this_surv_time, this_surv_status) ~ ., data_surv)
      # surv_model_coef <-  surv_model$coefficients
    
      
      # predict part: 
      surv_preds_00 <- predict(surv_model, newdata = data_pred_surv, type = "survival", se.fit = F)
      # temp_data <- data_surv
      # temp_data$this_surv_time <- rep(u, nrow(temp_data))
      # surv_preds_00 <- predict(surv_model, newdata = temp_data, type = "survival", se.fit = F)
      # surv_est_u[temp] <-  surv_preds_00
      
      
      #predict S_11^C(X)=P(C>= u|X,Z=1,S=1)
      data_censor <- data.frame(thisx, this_censor_time, this_censor_status)
      censor_model <- survival::coxph(survival::Surv(this_censor_time, this_censor_status) ~ ., data_censor)
      
      # predict part: 
      censor_preds_00 <- predict(censor_model, newdata = data_pred_censor, type = "survival", se.fit = F)
      # temp_data <- data_censor
      # temp_data$this_censor_time <- rep(u, nrow(temp_data))
      # censor_preds_00 <- predict(censor_model, newdata = temp_data, type = "survival", se.fit = F)
      # censor_est_u[temp] <-  censor_preds_00
      
      
    }
    
    ## Observed strata: Z=0, S=1
    {
      # newx<-cbind(1,X)
      temp<-which( (Z==0)&(S==1) )
      thisx<-X[temp, ]
      
      this_surv_time <- surv_time[temp]
      this_surv_status <- surv_status[temp]
      this_censor_time <- surv_time[temp]
      this_censor_status <- 1 - surv_status[temp]
      
      
      #predict S_11^T(X)=P(C>= u|X,Z=1,S=1) 
      data_surv <- data.frame(thisx, this_surv_time, this_surv_status)
      surv_model <- survival::coxph(survival::Surv(this_surv_time, this_surv_status) ~ ., data_surv)
      # surv_model_coef <-  surv_model$coefficients
      
      
      # predict part: 
      surv_preds_01 <- predict(surv_model, newdata = data_pred_surv, type = "survival", se.fit = F)
      # temp_data <- data_surv
      # temp_data$this_surv_time <- rep(u, nrow(temp_data))
      # surv_preds_01 <- predict(surv_model, newdata = temp_data, type = "survival", se.fit = F)
      # surv_est_u[temp] <-  surv_preds_01
      
      
      #predict S_11^C(X)=P(C>= u|X,Z=1,S=1)
      data_censor <- data.frame(thisx, this_censor_time, this_censor_status)
      censor_model <- survival::coxph(survival::Surv(this_censor_time, this_censor_status) ~ ., data_censor)
      
      # predict part: 
      censor_preds_01 <- predict(censor_model, newdata = data_pred_censor, type = "survival", se.fit = F)
      # temp_data <- data_censor
      # temp_data$this_censor_time <- rep(u, nrow(temp_data))
      # censor_preds_01 <- predict(censor_model, newdata = temp_data, type = "survival", se.fit = F)
      # censor_est_u[temp] <-  surv_preds_01
      
      
    }
    
    ## Observed strata: Z=1, S=0
    {
      # newx<-cbind(1,X)
      temp<-which( (Z==1)&(S==0) )
      thisx<-X[temp, ]
      
      this_surv_time <- surv_time[temp]
      this_surv_status <- surv_status[temp]
      this_censor_time <- surv_time[temp]
      this_censor_status <- 1 - surv_status[temp]
      
      
      #predict S_11^T(X)=P(C>= u|X,Z=1,S=1) 
      data_surv <- data.frame(thisx, this_surv_time, this_surv_status)
      surv_model <- survival::coxph(survival::Surv(this_surv_time, this_surv_status) ~ ., data_surv)
      # surv_model_coef <-  surv_model$coefficients
      
      
      # predict part: 
      surv_preds_10 <- predict(surv_model, newdata = data_pred_surv, type = "survival", se.fit = F)
      # temp_data <- data_surv
      # temp_data$this_surv_time <- rep(u, nrow(temp_data))
      # surv_preds_10 <- predict(surv_model, newdata = temp_data, type = "survival", se.fit = F)
      # surv_est_u[temp] <-  surv_preds_10
      
      
      #predict S_11^C(X)=P(C>= u|X,Z=1,S=1)
      data_censor <- data.frame(thisx, this_censor_time, this_censor_status)
      censor_model <- survival::coxph(survival::Surv(this_censor_time, this_censor_status) ~ ., data_censor)
      
      # predict part: 
      censor_preds_10 <- predict(censor_model, newdata = data_pred_censor, type = "survival", se.fit = F)
      # temp_data <- data_censor
      # temp_data$this_censor_time <- rep(u, nrow(temp_data))
      # censor_preds_10 <- predict(censor_model, newdata = temp_data, type = "survival", se.fit = F)
      # censor_est_u[temp] <-  censor_preds_10
      
      
    }
    
   
    
  }

  #predict pix=P(Z=1|X)
  newx<-cbind(1,X)
  thisy<-Z
  thisx<-X
  glm1<-stats::glm(thisy~thisx,family = "binomial" )
  glm.coeff<-glm1$coefficients
  loc.na<-which(is.na(glm.coeff))
  if(length(loc.na)>0){glm.coeff[loc.na]<-0}
  lpix <- newx%*%glm.coeff
  pix <-exp(lpix)/(1+exp(lpix) )
  pix <- as.vector(pix)

  #weighting 1 & stablized weighting
  p1 <- mean(S[loc.z1])

  p0 <- mean(S[loc.z0])

  pi <- mean(Z)

  p1.bar<-mean( Z*(S-p1x)/pix + p1x )

  p0.bar<-mean( (1-Z)*(S-p0x)/(1-pix) + p0x )

  # principal weights
  w1.10 <- (1-p0x/p1x)/((p1.bar-p0.bar)/p1)

  w0.10 <- ((p1x-p0x)/(1-p0x))/((p1.bar-p0.bar)/(1-p0))

  w1.00 <- 1

  w0.00 <- ((1-p1x)/(1-p0x))/((1-p1.bar)/(1-p0))

  w1.11 <- (p0x/p1x)/(p0.bar/p1)

  w0.11 <- 1
  
  
  # weighting formula for tau10,tau00,tau11 - done, estimation result not good
  {
    
    est1.10w <- mean(w1.10 *S*Z * surv_indi /p1 / pix/ censor_preds_11, na.rm=TRUE)
    
    est0.10w <- mean(w0.10 *(1-S)*(1-Z)* surv_indi /(1-p0) / (1-pix) / censor_preds_00, na.rm=TRUE)
    
    tau10w   <- est1.10w - est0.10w
    
    est1.00w <- mean(w1.00 * (1-S)*Z * surv_indi/(1-p1)/pix / censor_preds_10, na.rm=TRUE)
    
    est0.00w <- mean(w0.00 *(1-S)*(1-Z) * surv_indi/(1-p0)/(1-pix) / censor_preds_00, na.rm=TRUE)
    
    tau00w   <- est1.00w - est0.00w
    
    est1.11w <- mean(w1.11*S*Z * surv_indi/p1/pix / censor_preds_11, na.rm=TRUE)
    
    est0.11w <- mean(w0.11*S*(1-Z)* surv_indi/p0/(1-pix)/ censor_preds_01, na.rm=TRUE)
    
    tau11w   <- est1.11w - est0.11w
    
  }

  
  # stablized weighting formula for tau10,tau00,tau11 - done
  {
    sw1.10 <- w1.10 *n /sum( w1.10*S*Z/p1/pix,na.rm=TRUE)
    
    sw0.10 <- w0.10 *n /sum( w0.10*(1-S)*(1-Z)/(1-p0)/(1-pix),na.rm=TRUE)
    
    sw1.00 <- w1.00 *n /sum( w1.00*(1-S)*Z/(1-p1)/pix,na.rm=TRUE)
    
    sw0.00 <- w0.00 *n /sum( w0.00*(1-S)*(1-Z)/(1-p0)/(1-pix),na.rm=TRUE)
    
    sw1.11 <- w1.11 *n /sum( w1.11*S*Z/p1/pix,na.rm=TRUE)
    
    sw0.11 <- w0.11 *n /sum( w0.11*S*(1-Z)/p0/(1-pix),na.rm=TRUE)
    
    
    est1.10sw <- mean(sw1.10*S*Z/p1/pix * surv_indi / censor_preds_11, na.rm=TRUE)
    
    est0.10sw <- mean(sw0.10*(1-S)*(1-Z)/(1-p0)/(1-pix) * surv_indi / censor_preds_00, na.rm=TRUE)
    
    tau10sw   <- est1.10sw - est0.10sw
    
    est1.00sw <- mean(sw1.00*(1-S)*Z/(1-p1)/pix * surv_indi / censor_preds_10,na.rm=TRUE)
    
    est0.00sw <- mean(sw0.00*(1-S)*(1-Z)/(1-p0)/(1-pix) * surv_indi / censor_preds_00,na.rm=TRUE)
    
    tau00sw   <- est1.00sw - est0.00sw
    
    est1.11sw <- mean(sw1.11*S*Z/p1/pix * surv_indi / censor_preds_11,na.rm=TRUE)
    
    est0.11sw <- mean(sw0.11*S*(1-Z)/p0/(1-pix) * surv_indi / censor_preds_01,na.rm=TRUE)
    
    tau11sw   <- est1.11sw - est0.11sw
  }
  
  
  #regression 1 esitmators - done
  {
    est1.10reg <- mean( surv_preds_11 /(p1.bar-p0.bar)*( Z*S/pix-(1-Z)*S/(1-pix) ),na.rm=TRUE)
    
    est0.10reg <- mean( surv_preds_00 /(p1.bar-p0.bar)*( Z*S/pix-(1-Z)*S/(1-pix) ),na.rm=TRUE)
    
    tau10reg   <- est1.10reg - est0.10reg
    
    est1.00reg <- mean( surv_preds_10 /(1-p1.bar)*(  1 - S*Z/pix ),na.rm=TRUE)
    
    est0.00reg <- mean( surv_preds_00 /(1-p1.bar)*( 1 - S*Z/pix ),na.rm=TRUE)
    
    tau00reg   <- est1.00reg - est0.00reg
    
    est1.11reg <- mean( surv_preds_11 /(p0.bar)*( (1-Z)*S/(1-pix) ),na.rm=TRUE )
    
    est0.11reg <- mean( surv_preds_01 /(p0.bar)*( (1-Z)*S/(1-pix) ),na.rm=TRUE)
    
    tau11reg   <- est1.11reg - est0.11reg
    
  }
  
  
  #regression 2 estimators - done
  {
    tau10reg2   <- mean((p1x-p0x)/(p1.bar-p0.bar)*surv_preds_11 - (p1x-p0x)/(p1.bar-p0.bar)*surv_preds_00,na.rm=TRUE)
    
    tau00reg2   <- mean((1-p1x)/(1-p1.bar)*surv_preds_10 - (1-p1x)/(1-p1.bar)*surv_preds_00,na.rm=TRUE)
    
    tau11reg2   <- mean((p0x)/(p0.bar)*surv_preds_11 - (p0x)/(p0.bar)*surv_preds_01,na.rm=TRUE)
    
  }
  
  
  #aw estimator - not done!
  {
    # est1.10aw <- mean(  w1.10*Y*S*Z/p1/pix-
    #                       mu11x/(p1.bar-p0.bar)*(S-p0x)*(1-Z)/(1-pix)+
    #                       mu11x/(p1.bar-p0.bar)*p0x/p1x*(S-p1x)*Z/pix-
    #                       w1.10*mu11x*p1x/p1*(Z-pix)/pix      ,na.rm=TRUE)
    # 
    # est0.10aw <- mean(  w0.10*Y*(1-S)*(1-Z)/(1-p0)/(1-pix)-
    #                       mu00x/(p1.bar-p0.bar)*(1-p1x)/(1-p0x)* (S-p0x)*(1-Z)/(1-pix)+
    #                       mu00x/(p1.bar-p0.bar)*(S-p1x)*Z/pix+
    #                       w0.10*mu00x*(1-p0x)/(1-p0)*(Z-pix)/(1-pix)      ,na.rm=TRUE)
    # 
    # tau10aw   <- est1.10aw - est0.10aw
    # 
    # est1.00aw <- mean(    Y*(1-S)*Z/(1-p1.bar)/pix-
    #                         mu10x*(1-p1x)/(1-p1.bar)*(Z-pix)/pix      ,na.rm=TRUE)
    # 
    # est0.00aw <- mean(    w0.00*Y*(1-S)*(1-Z)/(1-p0)/(1-pix)+
    #                         mu00x/(1-p1.bar)*(1-p1x)/(1-p0x)* (S-p0x)*(1-Z)/(1-pix)-
    #                         mu00x/(1-p1.bar)*(S-p1x)*Z/pix+
    #                         w0.00*mu00x*(1-p0x)/(1-p0)*(Z-pix)/(1-pix)      ,na.rm=TRUE)
    # 
    # tau00aw   <- est1.00aw - est0.00aw
    # 
    # est1.11aw <- mean(  w1.11*Y*S*Z/p1/pix+
    #                       mu11x/(p0.bar)*(S-p0x)*(1-Z)/(1-pix)-
    #                       mu11x/(p0.bar)*p0x/p1x*(S-p1x)*Z/pix-
    #                       w1.11*mu11x*(p1x)/(p1)*(Z-pix)/pix      ,na.rm=TRUE)
    # 
    # est0.11aw <- mean(    Y*S*(1-Z)/(p0.bar)/(1-pix)+
    #                         mu01x*(p0x)/(p0.bar)*(Z-pix)/(1-pix)      ,na.rm=TRUE)
    # 
    # tau11aw   <- est1.11aw - est0.11aw
  }
 

  return( list(tau10w=tau10w,tau10reg=tau10reg,tau10reg2=tau10reg2, tau10sw=tau10sw,
               # tau10aw=tau10aw,
               tau00w=tau00w,tau00reg=tau00reg,tau00reg2=tau00reg2, tau00sw=tau00sw,
               # tau00aw=tau00aw,
               tau11w=tau11w,tau11reg=tau11reg,tau11reg2=tau11reg2, tau11sw=tau11sw
               # tau11aw=tau11aw
  ) )
}
