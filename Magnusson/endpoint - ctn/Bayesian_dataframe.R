#===========================
# Bayesian Dataframe
#===========================

rm(list=ls())
library(R2jags)  ;
library(dplyr)

#------------------#
# Functions
#------------------#
{
  source('WIP/Magnusson/endpoint - ctn/main/f_sim.r')                                   # Function to simulate data  
  source('WIP/Magnusson/endpoint - ctn/main/f_mod.r')                                   # Function to define model and write to file
  source('WIP/Magnusson/endpoint - ctn/main/f_I.r')                                     # Function to create matrix I
  source('WIP/Magnusson/endpoint - ctn/main/f_pm.r')                                    # Function to calculate prior means of delta using sample treatment effect estimates 
  source('WIP/Magnusson/endpoint - ctn/main/f_datjags.r')                               # Function to define dat.jags 
  source('WIP/Magnusson/endpoint - ctn/main/f_inits.r')                                 # Function to define all initial values  - starting values for MCMC
  source('WIP/Magnusson/endpoint - ctn/main/f_postparam_jags.r')                        # Function to compute postparam
  source('WIP/Magnusson/endpoint - ctn/main/f_postparam_jagsmodel.r')                   # Function to compute postparam (another way)
  source('WIP/Magnusson/endpoint - ctn/main/f_ace_1sim.r')                              # Function to compute ACE within 1 dataset
  source('WIP/Magnusson/endpoint - ctn/main/f_Uhat_Utrue.r')
  source('WIP/Magnusson/endpoint - ctn/main/f_rubins_rule.r')
}

#------------------#
# set parameters
#------------------#
{
  n              = 500                                # argument of f_sim: sample size in each 1 simulated trial
  seed           = seq(1,n,1)                         # argument of f_sim: set seed
  alpha1         = c(1.3,  0.3, -0.3)                 # argument of f_sim: coefficient of lm(Z_1 ~ X_1 X_2) at time 1, Z_1 is set to be the BASELINE variable
  alpha2         = c(  0,    0,    0)                 # argument of f_sim: coefficient of lm(Z_2 ~ X_1 X_2) at time 2, set to 0 s.t. there's no other covariates besides X & BASE
  alpha3         = c(  0,    0,    0)                 # argument of f_sim: coefficient of lm(Z_3 ~ X_1 X_2) at time 3, set to 0 s.t. there's no other covariates besides X & BASE
  beta           = c(0.3,  0.1, -0.3,  0.2,0,0 )      # argument of f_sim: coefficient of Y ~ X_1 X_2 Z_1 Z_2 Z_3 at time 3, beta[5,6] set to 0  s.t. Y is determined only by X & BASE
  gamma1         = c(2.5, -0.1, -0.2, -0.3)           # argument of f_sim: coefficient of lm(A ~ X_1 X_2 Z_1) at time 1
  gamma2         = c(2.4, -0.1, -0.2, -0.5)           # argument of f_sim: coefficient of lm(A ~ X_1 X_2 Z_2) at time 2    
  gamma3         = c(2.3, -0.1, -0.2, -0.5)           # argument of f_sim: coefficient of lm(A ~ X_1 X_2 Z_1) at time 3, note that Z_3 has been replaced with Z_1, s.t. A is determined only by X & BASE
  TrtEff_adhpbo  = 0.5                                # argument of f_sim: true treatment/causal effect in stratum [H][1]
  TrtEff_adhnei  = 0                                  # argument of f_sim: true treatment/causal effect in stratum [D][2]
  TrtEff_adhboth = 2                                  # argument of f_sim: true treatment/causal effect in stratum [I][3]
  TrtEff_adhact  = 1.5                                # argument of f_sim: true treatment/causal effect in stratum [B][4]
  nSim           = 3                                  # number of simulated trials
  parSave        = c("delta","S0","S1","Y0","Y1","w") # argument of jags() 
  n.chains       = 2                                  # argument of jags()
  n.burnin       = 20                                # argument of jags()
  n.iter         = 100                               # argument of jags()
  thin           = 2                                  # argument of jags()
  file           = "mod.txt"                          # argument of jags()
  n.adapt        = 1000                               # argument of jags.model()
}

#------------------#
# dataframe
#------------------#
result_df  =  NULL
i          = 1
# simulate 1 dataset and prepare for variables

sim           = f_sim(seed[i],n,alpha1,alpha2,alpha3,beta,gamma1,gamma2,gamma3,TrtEff_adhnei,TrtEff_adhboth,TrtEff_adhact,TrtEff_adhpbo)
dat_          = sim$full_long %>% filter(AVISITN==3) 
dat_in        = dat_ %>%  mutate(Y0                = ifelse(TRT==0, Y, NA),
                                 Y1                = ifelse(TRT==1, Y, NA),
                                 Z                 = TRT,
                                 S                 = ICE,
                                 S0                = ifelse(TRT==0, S, NA),
                                 S1                = ifelse(TRT==1, S, NA),
                                 X_1_standardized  = X_1, #X_1-mean(X_1),
                                 X_2_standardized  = X_2, #X_2-mean(X_2),
                                 base_standardized = BASE #-mean(BASE)   
)
for(r in 1:nrow(dat_in)){dat_in$Utrue[r] = strsplit(dat_in$U[r],"/")[[1]][3] }

# true causal effect (the true 'd' of stratum D/I/H/B)
trued_H       = sim$true_d_T3$true_d_T3_adhpbo
trued_D       = sim$true_d_T3$true_d_T3_adhnei
trued_I       = sim$true_d_T3$true_d_T3_adhboth
trued_B       = sim$true_d_T3$true_d_T3_adhact 

# ace calculation for 1 simulated dataset

I             = f_I(dat=dat_in)
pm_results    = f_pm(sim)
dat.jags      = f_datjags(dat = dat_in, I=I,pm_results=pm_results) 
inits         = f_inits(dat=dat_in) 
postparam     = f_postparam_jags(dat.jags,inits,parSave,text,n.chains,n.iter,n.burnin,thin) 

niter          = nrow(postparam)

#---------------------------------------------------
# Extract results from postparam
#---------------------------------------------------
dat <- dat_in
{ 
  # "S0"
  
  S0_pot_        = postparam[,grep("S0",    names(postparam), value=TRUE)]     # nrows=niter,ncols=n
  col_order      = paste0("S0[",1:nrow(dat),"]")
  S0_pot         = S0_pot_[,col_order]
  S0_pot         = t(S0_pot)
  
  # "S1"
  
  S1_pot_        = postparam[,grep("S1",    names(postparam), value=TRUE)]     # nrows=niter,ncols=n
  col_order      = paste0("S1[",1:nrow(dat),"]")
  S1_pot         = S1_pot_[,col_order]
  S1_pot         = t(S1_pot)
  
  # "Y0"
  
  Y0_pot_        = postparam[,grep("Y0",   names(postparam), value=TRUE)]
  col_order      = paste0("Y0[",1:nrow(dat),"]")
  Y0_pot         = Y0_pot_[,col_order]
  Y0_pot         = t(Y0_pot)
  
  #  "Y1"
  
  Y1_pot_        = postparam[,grep("Y1",   names(postparam), value=TRUE)]
  col_order      = paste0("Y1[",1:nrow(dat),"]")
  Y1_pot         = Y1_pot_[,col_order]
  Y1_pot         = t(Y1_pot)
  
  # "delta"
  
  delta_pot      = postparam[,grep("delta", names(postparam), value=TRUE)]     # nrows=niter,ncols=4  
  
  # "w"
  w             = postparam[,grep("w", names(postparam), value=TRUE)] 
  index_i       = rep(1:nrow(dat_in),each=4)
  index_j       = rep(1:4, nrow(dat_in))
  col_order     = paste0("w[",index_i,",",index_j,"]")
  w             = w[,col_order]
  
}

#---------------------------------------------------
# Pull ACE,PCP from all iterations
#---------------------------------------------------

ITTmean_H  = ITTmean_D  = ITTmean_I  = ITTmean_B  = ITTmean_IB  = rep(NA, niter)
ITTse_H    = ITTse_D    = ITTse_I    = ITTse_B    = ITTse_IB    = rep(NA, niter)
delta_se_H = delta_se_D = delta_se_I = delta_se_B = delta_se_IB = rep(NA, niter)
PCP_H      = PCP_D      = PCP_I      = PCP_B                    = rep(NA, niter)
nH         = nD         = nI         = nB         = nIB         = rep(NA, niter)

iter = 1
  
  W = w[iter,] 
  W = matrix(unlist(W),nrow=nrow(dat_in),ncol=4,byrow = T) 
  W = as.data.frame(W) 
  colnames(W) <-c("H","D","I","B")
  
  # Create ITT table for 1 iteration
 
    ITT_tb   =  cbind.data.frame(USUBJID = seq(1,nrow(dat),1), 
                                 S0_pot  = S0_pot[,iter],
                                 S1_pot  = S1_pot[,iter],
                                 Y0_pot  = Y0_pot[,iter],
                                 Y1_pot  = Y1_pot[,iter],
                                 U       = dat$U,
                                 Z       = dat$TRT,
                                 S       = dat$S,
                                 S0      = dat$S0,
                                 S1      = dat$S1,
                                 Y       = dat$Y,
                                 Y0      = dat$Y0,
                                 Y1      = dat$Y1,
                                 I       = I,
                                 W       = W
    ) %>% 
      mutate(S0_imp  = ifelse(is.na(S0), S0_pot, S0),
             S1_imp  = ifelse(is.na(S1), S1_pot, S1),
             Y0_imp  = ifelse(is.na(Y0), Y0_pot, Y0),
             Y1_imp  = ifelse(is.na(Y1), Y1_pot, Y1),
             ITT_imp = Z*(Y1_imp-Y0_imp) + (1-Z)*(Y1_imp-Y0_imp)) %>%
      select(USUBJID,S,Z,Y,I.1,I.2,I.3,I.4,W.H,W.D,W.I,W.B,S0,S0_imp,S1,S1_imp,Y0,Y0_imp,Y1,Y1_imp,ITT_imp,U)%>% 
      rename(stratum_H=I.1,stratum_D=I.2,stratum_I=I.3,stratum_B=I.4)%>%
      `rownames<-`(NULL)
    ITT_tb = f_Uhat(data=ITT_tb) ;head(ITT_tb)
    ITT_tb = f_Utrue(data=ITT_tb) ;head(ITT_tb)
    ITT_tb = ITT_tb %>% mutate(IU=as.numeric(Uhat==Utrue))
    
    for(i in 1:nrow(ITT_tb)){
      if(ITT_tb$Utrue[i]=="H"){ITT_tb$PCP[i]= ITT_tb$IU[i]*as.numeric(ITT_tb$W.H[i])  
      }else if(ITT_tb$Utrue[i]=="D"){ITT_tb$PCP[i]= ITT_tb$IU[i]*as.numeric(ITT_tb$W.D[i])  
      }else if(ITT_tb$Utrue[i]=="I"){ITT_tb$PCP[i]= ITT_tb$IU[i]*as.numeric(ITT_tb$W.I[i])  
      }else if(ITT_tb$Utrue[i]=="B"){ITT_tb$PCP[i]= ITT_tb$IU[i]*as.numeric(ITT_tb$W.B[i])  
      }
    }
      
B= ITT_tb 
write.csv(B,"WIP/Magnusson/endpoint - ctn/resultprint/Bayesian_dataframe.csv")
  
