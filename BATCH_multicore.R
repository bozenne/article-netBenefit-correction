## path <- "p:/Cluster/GPC/Article-correction-Julien/"
## setwd(path)
## source("BATCH_multicore.R")
## R CMD BATCH BATCH_multicore.R &
## 20381

library(BuyseTest)

## * Settings
source("FCT_multicore.R")

grid.s1 <- expand.grid(n = c(25,50,100,200), ## sample size (each group)
                              hazard.T = c(0.005,0.01), ## hazard for the event in the treatment group
                              hazard.C = 0.01, ## hazard for the event in the control group
                              hazard.censoring = c(0.0000001,0.0017,0.0045,0.007,0.017,0.028,0.068) ## hazard for the censoring mechanism
                              )

gridR.s1 <- grid.s1[grid.s1$n == 200 & grid.s1$hazard.censoring %in% c(0.0000001,0.0017,0.007,0.028),]

grid.s2 <- expand.grid(n = c(25,50,100,200), ## sample size (each group)
                    hazard.T = c(0.005,0.01), ## hazard for the event in the treatment group
                    hazard.C = 0.01, ## hazard for the event in the control group
                    hazard.censoring = c(0.0000001,0.0017,0.0045,0.007,0.017,0.028,0.068) ## hazard for the censoring mechanism
                    )
gridR.s2 <- grid.s2[grid.s2$n == 200 & grid.s2$hazard.censoring %in% c(0.0000001,0.0017,0.007,0.028),]

grid.s3 <- expand.grid(n = c(25,50,100,200), ## sample size (each group)
                              HR = c(0.3), ## hazard for the event in the treatment group
                              hazard.C = 0.01, ## hazard for the event in the control group
                              type = "early",
                              hazard.censoring = c(0.0000001,0.0017,0.0045,0.007,0.017,0.028,0.068) ## hazard for the censoring mechanism
                              )
gridR.s3 <- grid.s3[grid.s3$n == 200 & grid.s3$hazard.censoring %in% c(0.0000001,0.0017,0.007,0.028),]

grid.s4 <- expand.grid(n = c(25,50,100,200), ## sample size (each group)
                              HR = c(0.3), ## hazard for the event in the treatment group
                              hazard.C = 0.01, ## hazard for the event in the control group
                              type = "late",
                              hazard.censoring = c(0.0000001,0.0017,0.0045,0.007,0.017,0.028,0.068) ## hazard for the censoring mechanism
                              )

gridR.s4 <- grid.s4[grid.s4$n == 200 & grid.s4$hazard.censoring %in% c(0.0000001,0.0017,0.007,0.028),]

## ## * RUN (point estimate)
BuyseTest.options(method.inference = "none",
                  trace = 0)

simulation <- 100
ncpus <- 50
n.sim <- 50
parallel::mclapply(1:n.sim, function(icpus){
    myBATCH_scenario1(iter_sim = icpus, n.iter_sim = n.sim, grid = grid.s1, simulation = simulation, filename = "repetition")
    myBATCH_scenario4(iter_sim = icpus, n.iter_sim = n.sim, grid = grid.s4, simulation = simulation, filename = "repetition")
}, mc.cores = ncpus)

simulation <- 500
ncpus <- 10
n.sim <- 10
parallel::mclapply(1:n.sim, function(icpus){
    myBATCH_scenario2(iter_sim = icpus, n.iter_sim = n.sim, grid = grid.s2, simulation = simulation, filename = "repetition")
    myBATCH_scenario3(iter_sim = icpus, n.iter_sim = n.sim, grid = grid.s3, simulation = simulation, filename = "repetition")
}, mc.cores = ncpus)


## * RUN (resampling)
BuyseTest.options(method.inference = "bootstrap",
                  n.resampling = 5e2,
                  trace = 0)

simulation <- 100
ncpus <- 50
n.sim <- 50
cat("scenario 1\n")
parallel::mclapply(1:n.sim, function(icpus){
    myBATCH_scenario1(iter_sim = icpus, n.iter_sim = n.sim, grid = gridR.s1, simulation = simulation, filename = "inference")
}, mc.cores = ncpus)
cat("scenario 2\n")
parallel::mclapply(1:n.sim, function(icpus){
    myBATCH_scenario2(iter_sim = icpus, n.iter_sim = n.sim, grid = gridR.s2, simulation = simulation, filename = "inference")
}, mc.cores = ncpus)
cat("scenario 3\n")
parallel::mclapply(1:n.sim, function(icpus){
    myBATCH_scenario3(iter_sim = icpus, n.iter_sim = n.sim, grid = gridR.s3, simulation = simulation, filename = "inference")
}, mc.cores = ncpus)
cat("scenario 4\n")
parallel::mclapply(1:n.sim, function(icpus){
    myBATCH_scenario4(iter_sim = icpus, n.iter_sim = n.sim, grid = gridR.s4, simulation = simulation, filename = "inference")
}, mc.cores = ncpus)

