rm(list=ls())
library("pace")

# Input of this function
# X| is a matrix of pre-treatment covariates without intercept  (n x p).
# Z| is a vector of treatment  (n x 1).
# S| is a vector of binary intermediate outcome  (n x 1).
# Y| is a vector of outcome  ( n x 1).
# family.Y| specifies the family for the outcome model.
# * |"gaussian": a linear regression model for the continuous outcome.
# * |"binomial": a logistic regression model for the binary outcome.
# nboot| is the number of bootstrap samples.



# fludata example
setwd("C:/Users/gaoyuji/OneDrive - Merck Sharp & Dohme LLC/Desktop/Development R code/2. MR method")

fl = read.table("fludata.txt", header=TRUE, quote="\"")
Z = fl$assign 
S = fl$receive
Y = fl$outcome
X = as.matrix(fl[, -c(1, 2, 3)])
N = length(Z)

system.time(out.flu <- pace::pace(X,Z,S,Y,family.Y="binomial",nboot=200))

cname<-c("Est","SE","95%CI","")
rname<-c("w","sw","reg","reg2")
table10<-rbind( 
  c(out.flu$tau10w, sqrt(out.flu$ve.tau10w) , out.flu$tau10w-1.96* sqrt(out.flu$ve.tau10w), out.flu$tau10w+1.96*sqrt(out.flu$ve.tau10w) ),
  c(out.flu$tau10sw, sqrt(out.flu$ve.tau10sw) , out.flu$tau10sw-1.96* sqrt(out.flu$ve.tau10sw), out.flu$tau10sw+1.96*sqrt(out.flu$ve.tau10sw) ),
  c(out.flu$tau10reg, sqrt(out.flu$ve.tau10reg) , out.flu$tau10reg-1.96* sqrt(out.flu$ve.tau10reg), out.flu$tau10reg+1.96*sqrt(out.flu$ve.tau10reg) ),
  c(out.flu$tau10reg2, sqrt(out.flu$ve.tau10reg2) , out.flu$tau10reg2-1.96* sqrt(out.flu$ve.tau10reg2), out.flu$tau10reg2+1.96*sqrt(out.flu$ve.tau10reg2) )
)
colnames(table10)<-cname
row.names(table10)<-rname

table00<-rbind( 
  c(out.flu$tau00w, sqrt(out.flu$ve.tau00w) , out.flu$tau00w-1.96* sqrt(out.flu$ve.tau00w), out.flu$tau00w+1.96*sqrt(out.flu$ve.tau00w) ),
  c(out.flu$tau00sw, sqrt(out.flu$ve.tau00sw) , out.flu$tau00sw-1.96* sqrt(out.flu$ve.tau00sw), out.flu$tau00sw+1.96*sqrt(out.flu$ve.tau00sw) ),
  c(out.flu$tau00reg, sqrt(out.flu$ve.tau00reg) , out.flu$tau00reg-1.96* sqrt(out.flu$ve.tau00reg), out.flu$tau00reg+1.96*sqrt(out.flu$ve.tau00reg) ),
  c(out.flu$tau00reg2, sqrt(out.flu$ve.tau00reg2) , out.flu$tau00reg2-1.96* sqrt(out.flu$ve.tau00reg2), out.flu$tau00reg2+1.96*sqrt(out.flu$ve.tau00reg2) )
)
colnames(table00)<-cname
row.names(table00)<-rname

table11<-rbind( 
  c(out.flu$tau11w, sqrt(out.flu$ve.tau11w) , out.flu$tau11w-1.96* sqrt(out.flu$ve.tau11w), out.flu$tau11w+1.96*sqrt(out.flu$ve.tau11w) ),
  c(out.flu$tau11sw, sqrt(out.flu$ve.tau11sw) , out.flu$tau11sw-1.96* sqrt(out.flu$ve.tau11sw), out.flu$tau11sw+1.96*sqrt(out.flu$ve.tau11sw) ),
  c(out.flu$tau11reg, sqrt(out.flu$ve.tau11reg) , out.flu$tau11reg-1.96* sqrt(out.flu$ve.tau11reg), out.flu$tau11reg+1.96*sqrt(out.flu$ve.tau11reg) ),
  c(out.flu$tau11reg2, sqrt(out.flu$ve.tau11reg2) , out.flu$tau11reg2-1.96* sqrt(out.flu$ve.tau11reg2), out.flu$tau11reg2+1.96*sqrt(out.flu$ve.tau11reg2) )
)
colnames(table11)<-cname
row.names(table11)<-rname
library(knitr)
print(kable( table10, caption="Results for compliers" ))
print(kable( table00, caption="Results for never takers" ))
print(kable( table11, caption="Results for always takers" ))
