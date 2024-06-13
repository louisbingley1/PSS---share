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



