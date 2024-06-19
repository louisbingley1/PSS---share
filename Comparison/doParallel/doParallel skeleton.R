# library packages
library(parallel)
library(doParallel)
library(foreach)


n_core      <- detectCores()                                                    # number of cores; or: detectCores(logical = TRUE)  
cl          <- makeCluster(n_core-1)                                            # or: parallel::makeCluster( k-1, type = "PSOCK")
registerDoParallel(cl)                                                          # or: doParallel::registerDoParallel(cl); register it to be used by %dopar%
# foreach::getDoParRegistered()                                                 # check if it is registered (optional)
# foreach::getDoParWorkers()                                                    # how many workers are available? (optional)

#----------------------------------
# foreach parallel - 1 secs
#----------------------------------
cl  <- makeCluster(n_core-1)                                            
registerDoParallel(cl)                                                         
system.time(
  foreach(i = 1:6, .combine=cbind) %dopar% {
    Sys.sleep(4)
  }
)
stopCluster(cl)


# default without using parallel - 10 secs
system.time(
  for(i in 1:6){
    Sys.sleep(6)
  }
)




#===============================
#  template
#===============================
f_1simf = function(i){ x    = i+1;
y    = i  ;
r1   = cbind.data.frame(c1=x+y,c2=x+y+1);
r2   = cbind.data.frame(c3=x-y,c4=x-y-1); 
r3   = cbind.data.frame(c5=x-y-1,c6=x+y-1)
list = list(r1=r1,r2=r2,r3=r3);
return(list)
}

timer_start <- proc.time()

n_core      <- detectCores()                                                    # number of cores; or: detectCores(logical = TRUE)  
cl          <- makeCluster(n_core-1)
registerDoParallel(cl)                                                          # comb <- function(x, ...) {  lapply(seq_along(x), function(i) c(x[[i]], lapply(list(...), function(y) y[[i]]))) }
data_out <- foreach (i         = 1:20,                                          # export   =c("f_1simf"),
                     .combine  ='rbind'                                         # , .multicombine=TRUE
) %dopar% {   f_1simf(i)}                                  # { df_1simf = f_1simf(x=i,y=i+1) ; list(df_1simf) }
stopCluster(cl)

timer_stop  <- proc.time() 
dur = timer_stop - timer_start ; dur


result_df_PS = result_df_BS = result_df_AD= NULL
for(i in 1:20){
  result_df_PS = rbind.data.frame(result_df_PS, data_out[i,]$r1)
  result_df_BS = rbind.data.frame(result_df_BS, data_out[i,]$r2)
  result_df_AD = rbind.data.frame(result_df_AD, data_out[i,]$r3)
}
result_df_PS; result_df_BS; result_df_AD

