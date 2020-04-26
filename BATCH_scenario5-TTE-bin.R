## path <- "p:/Cluster/GPC/Article-correction-Julien/"
## setwd(path)
## source("BATCH_scenario5-TTE-bin.R")

## * seed
iter_sim <- as.numeric(Sys.getenv("SGE_TASK_ID"))
n.iter_sim <- as.numeric(Sys.getenv("SGE_TASK_LAST"))
if(is.na(iter_sim)){iter_sim <- 87}
if(is.na(n.iter_sim)){n.iter_sim <- 100}
cat("iteration ",iter_sim," over ",n.iter_sim,"\n", sep = "")

set.seed(1)
seqSeed <- sample(1:max(1e5,n.iter_sim),size=n.iter_sim,replace=FALSE)
iSeed <- seqSeed[iter_sim]
set.seed(iSeed)

cat("seed: ",iSeed,"\n")

## * path
path <- "."
path.res <- file.path(path,"Results","scenario5-TTE-bin")
if(dir.exists(path.res)==FALSE){
    dir.create(path.res)
}
path.output <- file.path(path,"output","scenario5-TTE-bin")
if(dir.exists(path.output)==FALSE){
    dir.create(path.output)
}

## * R packages
library(BuyseTest)
library(survival)

confint2 <- function(object){
    CI <- confint(object)
    out <- data.frame(endpoint = rownames(CI),
                      index.endpoint = 1:NROW(CI),
                      pc.neutral = as.double(object@count.neutral/object@n.pairs),
                      CI,
                      stringsAsFactors = FALSE)
    rownames(out) <- NULL
    return(out)
}

## * Settings
simulation <- 250 # nombre de simulations par condition
BuyseTest.options(method.inference = "none",
                  n.resampling = 1e3,
                  trace = 0)

grid <- expand.grid(n = c(25,50,100,200), ## sample size (each group)
                    hazard.T = c(0.005), ## hazard for the event in the treatment group
                    hazard.C = 0.01, ## hazard for the event in the control group
                    p.T = 0.8,
                    p.C = 0.5,
                    hazard.censoring = c(0.0000001,0.0017,0.0045,0.007,0.017,0.028,0.068) ## hazard for the censoring mechanism
                    )
n.grid <- NROW(grid)
method.inference.correction <- "u-statistic"

formula.BT <- group ~ tte(time, status = status, threshold = 50) + bin(toxicity)
formula.GS <- group ~ cont(timetoevent, threshold = 50) + bin(toxicity)

## * generative distribution

simData <- function(n.C, n.T, hazard.C, hazard.T, hazard.censoring, p.T, p.C){

    group <- c(rep("T", n.T), rep("C", n.C)) # on attribut un groupe a chacun des patients
    toxicity <- c(rbinom(n.T, size = 1, prob = p.T), rbinom(n.C, size = 1, prob = p.C)) # CJ1 ex:OS
    TimeToEvent <- c(rexp(n.T, rate=hazard.T), rexp(n.C, rate=hazard.C)) # CJ1 ex:OS
    TimeToCens <- rexp(n.C+n.T, hazard.censoring) # censure uniforme de meme loi dans les 2 grpes
    Time <- pmin(TimeToCens, TimeToEvent) # on prend le minimum entre le temps jusque censure et le temps jusque evenement
    Event <- as.numeric(Time==TimeToEvent) # 1 si evenement 0 si censure
    df <- data.frame(group = group, time = Time, status = Event, timetoevent = TimeToEvent, toxicity = toxicity)
    return(df)
    
}

## * run simulation
res <- NULL ## start with an empty dataset
for (iGrid in 1:n.grid){   ## iGrid <- 1

    iN <- grid[iGrid,"n"]
    iHazard.T <- grid[iGrid,"hazard.T"]
    iHazard.C <- grid[iGrid,"hazard.C"]
    iHazard.censoring <- grid[iGrid,"hazard.censoring"]
    iP.T <- grid[iGrid,"p.T"]
    iP.C <- grid[iGrid,"p.C"]
    
    cat("Grid (",iGrid,"/",n.grid,") : n=",iN," | hazard.T=",iHazard.T," | hazard.C=",iHazard.C," | hazard.censoring=",iHazard.censoring," | p.T=",iP.T," | p.C=",iP.C,"\n", sep = "")

        for (iRep in 1:simulation){ ## iRep <- 1
            cat(iRep," ")
            
            ## ** simulate data
            iData <- simData(n.C = iN,
                             n.T = iN,
                             hazard.C = iHazard.C,
                             hazard.T = iHazard.T,
                             p.C = iP.C,
                             p.T = iP.T,
                             hazard.censoring = iHazard.censoring)

            ## ** run BuyseTest
            BuyseGS <- BuyseTest(formula.GS, data=iData,
                                 seed = NULL)
            
            BuyseGehan <- BuyseTest(formula.BT, data=iData,
                                    scoring.rule ="Gehan",
                                    correction.uninf = 0,
                                    seed = NULL)
            
            BuyseGehan_Corr <- BuyseTest(formula.BT, data=iData,
                                         scoring.rule ="Gehan",
                                         correction.uninf = 1,
                                         seed = NULL)

            BuysePeron <- BuyseTest(formula.BT, data=iData, 
                                    scoring.rule ="Peron",
                                    correction.uninf = 0,
                                    seed = NULL)
                            
            BuysePeron_Corr <- BuyseTest(formula.BT, data=iData, 
                                         scoring.rule ="Peron",
                                         correction.uninf = 1,
                                         seed = NULL)

            ## ** store results
            iRes <- rbind(cbind(scoring.rule = "GS", confint2(BuyseGS)),
                          cbind(scoring.rule = "Gehan", confint2(BuyseGehan)),
                          cbind(scoring.rule = "Gehan.C", suppressWarnings(confint2(BuyseGehan_Corr))),
                          cbind(scoring.rule = "Peron", confint2(BuysePeron)),
                          cbind(scoring.rule = "Peron.C", suppressWarnings(confint2(BuysePeron_Corr)))
                          )

            res <- rbind(res,
                         cbind(rep = iRep,
                               grid = iGrid,
                               pc.censoring = 1-mean(iData$status),
                               iRes))
        }

    cat("\n")
    ## ** export
    resTempo <- res
    attr(resTempo,"grid") <- grid
    filename <- paste0("repetition(tempo)_",iter_sim,".rds")
    saveRDS(resTempo, file = file.path(path.res,filename))
}
## * export
attr(res,"grid") <- grid
filename <- paste0("repetition_",iter_sim,".rds")
saveRDS(res, file = file.path(path.res,filename))

## * display
print(res)
print(sessionInfo())
