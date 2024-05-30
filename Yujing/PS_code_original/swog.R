##Preprocessing the data
sg = read.table("~/Desktop/DingLu2015JRSSB_Code/swogdata.txt", header=TRUE, quote="\"")
Z = sg$Z
N = length(Z)
D = ifelse(is.na(sg$score12), 0, 1)
Y = sg$score12 - sg$score0
Y[is.na(Y)] = 0
X = as.matrix(cbind(sg$AGE, sg$RACEB, sg$RACEO, sg$score0))

#decide the range of sensitivity parameter xi
p1 = sum(Z == 1 & D == 1)/sum(Z == 1)
p0 = sum(Z == 0 & D == 1)/sum(Z == 0)
xi.max = (p1 - p0)/min(p1, 1 - p0)

xi = seq(0, .2, .02)
SACE = rep(NA, length(xi))
SACE.reg = rep(NA, length(xi))


##Estimation under Monotonicity
require(nnet)
point = PSPS_M_weighting_SA(Z, D, X, Y)
#SACE (i.e., AACE)
point$AACE.reg

##sensitivity analysis for Monotonicity
#for example let \eta = 0.1
point = PSPS_M_weighting_SA(Z, D, X, Y, eta = 0.1)
#SACE (i.e., AACE)
point$AACE.reg


##Notes
#1. For variance estimation, apply function ``PSPS_M_weighting_SA'' on bootstraped samples
#2. For balancing checking, do for example ``PSPS_M_weighting_SA(Z, D, X, X[, 1])

