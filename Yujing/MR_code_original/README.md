
# pace

The goal of *pace* is to implement various estimators of Average Causal
Effects within principal strata (PACEs) from randomized experiments and
observational studies.

## Installation with `devtools`:

``` r
devtools::install_github("shuyang1987/pace")
```

### Main Paper: coming soon

Jiang et al. (2020) Multiply robust estimation of causal effects under
principal
ignorability.

### Usage

pace(X,Z,S,Y,family.Y,nboot)

### Arguments

| Argument |                                                                    |
| -------- | ------------------------------------------------------------------ |
| X        | is a matrix of pre-treatment covariates without intercept (n x p). |
| Z        | is a vector of treatment (n x 1).                                  |
| S        | is a vector of binary intermediate outcome (n x 1).                |
| Y        | is a vector of outcome ( n x 1).                                   |
| family.Y | specifies the family for the outcome model.                        |
| \*       | “gaussian”: a linear regression model for the continuous outcome.  |
| \*       | “binomial”: a logistic regression model for the binary outcome.    |
| nboot    | is the number of bootstrap samples.                                |

### Value

|                              |                                                                                                              |
| ---------------------------- | ------------------------------------------------------------------------------------------------------------ |
| Complier                     |                                                                                                              |
| tau10w                       | a principal score weighting estimator of tau10                                                               |
| tau10sw                      | a stabilized weighting estimator of tau10                                                                    |
| tau10reg                     | a regression estimator of tau10 using outcome mean regression and inverse probability of treatment weighting |
| tau10reg2                    | a regression estimator of tau10 using outcome mean and principal score regression                            |
| tau10aw                      | a triply robust estimator of tau10                                                                           |
| bootstrap variance estimator | ve.tau10w,ve.tau10sw,ve.tau10reg,ve.tau10reg2,ve.tau10aw                                                     |

|                              |                                                                                                              |
| ---------------------------- | ------------------------------------------------------------------------------------------------------------ |
| Never Taker                  |                                                                                                              |
| tau00w                       | a principal score weighting estimator of tau00                                                               |
| tau00sw                      | a stabilized weighting estimator of tau00                                                                    |
| tau00reg                     | a regression estimator of tau00 using outcome mean regression and inverse probability of treatment weighting |
| tau00reg2                    | a regression estimator of tau00 using outcome mean and principal score regression                            |
| tau00aw                      | a triply robust estimator of tau00                                                                           |
| bootstrap variance estimator | ve.tau00w,ve.tau00sw,ve.tau00reg,ve.tau00reg2,ve.tau00aw                                                     |

|                              |                                                                                                              |
| ---------------------------- | ------------------------------------------------------------------------------------------------------------ |
| Always Taker                 |                                                                                                              |
| tau11w                       | a principal score weighting estimator of tau11                                                               |
| tau11sw                      | a stabilized weighting estimator of tau11                                                                    |
| tau11reg                     | a regression estimator of tau11 using outcome mean regression and inverse probability of treatment weighting |
| tau11reg2                    | a regression estimator of tau11 using outcome mean and principal score regression                            |
| tau11aw                      | a triply robust estimator of tau11                                                                           |
| bootstrap variance estimator | ve.tau11w,ve.tau11sw,ve.tau11reg,ve.tau11reg2,ve.tau11aw                                                     |

## Example

Application: An encouragement experiment with non-compliance

``` r

fl = read.table("fludata.txt", header=TRUE, quote="\"")
Z = fl$assign 
S = fl$receive
Y = fl$outcome
X = as.matrix(fl[, -c(1, 2, 3)])
N = length(Z)

out.flu<-pace::pace(X,Z,S,Y,family.Y="binomial",nboot=100)

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
#> 
#> 
#> Table: Results for compliers
#> 
#>                Est          SE        95%CI            
#> -----  -----------  ----------  -----------  ----------
#> w       -0.0166239   0.0191541   -0.0541660   0.0209183
#> sw      -0.0162693   0.0190716   -0.0536496   0.0211111
#> reg     -0.0152307   0.0190876   -0.0526424   0.0221809
#> reg2    -0.0149633   0.0191615   -0.0525198   0.0225931
print(kable( table00, caption="Results for never takers" ))
#> 
#> 
#> Table: Results for never takers
#> 
#>                Est          SE        95%CI            
#> -----  -----------  ----------  -----------  ----------
#> w       -0.0061187   0.0119362   -0.0295136   0.0172762
#> sw      -0.0061003   0.0119541   -0.0295303   0.0173297
#> reg     -0.0059885   0.0118392   -0.0291934   0.0172164
#> reg2    -0.0060646   0.0118694   -0.0293285   0.0171994
print(kable( table11, caption="Results for always takers" ))
#> 
#> 
#> Table: Results for always takers
#> 
#>                Est          SE        95%CI            
#> -----  -----------  ----------  -----------  ----------
#> w       -0.0466259   0.0244611   -0.0945697   0.0013179
#> sw      -0.0465378   0.0245400   -0.0946361   0.0015605
#> reg     -0.0472388   0.0244514   -0.0951636   0.0006860
#> reg2    -0.0461420   0.0243074   -0.0937846   0.0015006
```
