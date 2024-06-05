# library packages
library(parallel)
library(doParallel)
library(foreach)

detectCores() # 12 cores


# no_cores <- detectCores(logical = TRUE) 
# cl <- makeCluster(no_cores-1, type='PSOCK')  
my.cluster <- parallel::makeCluster(
  10, 
  type = "PSOCK"
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered (optional)
foreach::getDoParRegistered()

#how many workers are available? (optional)
foreach::getDoParWorkers()

# foreach parallel - 1 secs
system.time(
  foreach(i = 1:10, .combine=cbind) %dopar% {
    Sys.sleep(1)
  }
  )

stopCluster(my.cluster)


# default without using parallel - 10 secs
system.time(
  for(i in 1:10){
    Sys.sleep(1)
  }
)



