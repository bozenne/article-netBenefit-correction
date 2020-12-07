## path <- "p:/Cluster/GPC/Article-correction-Julien/"
## setwd(path)
## source("BATCH_scenario3-1TTE-earlyEffect.R")

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
path.res <- file.path(path,"Results","scenario3-1TTE-earlyEffect")
if(dir.exists(path.res)==FALSE){
    dir.create(path.res)
}
path.output <- file.path(path,"output","scenario3-1TTE-earlyEffect")
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

## * settings
simulation <- 250 ## number of simulations per CPU
BuyseTest.options(method.inference = "bootstrap",
                  n.resampling = 1e3,
                  trace = 0)

grid <- expand.grid(n = c(25,50,100,200), ## sample size (each group)
                    HR = c(0.3), ## hazard for the event in the treatment group
                    hazard.C = 0.01, ## hazard for the event in the control group
                    type = "early",
                    hazard.censoring = c(0.0000001,0.0017,0.0045,0.007,0.017,0.028,0.068) ## hazard for the censoring mechanism
                    )
n.grid <- NROW(grid)

formula.BT <- group ~ tte(time, status = status, threshold = 0)
formula.GS <- group ~ cont(timetoevent, threshold = 0)

## * generative distribution

simData <- function(n.C, n.T, hazard.C, HR, hazard.censoring, type){
    t <- c(40, 60, 80, 100)

    if(type=="late"){
        hazard.T <- hazard.C * c(1,
                                 0.75 + 0.25 * HR,
                                 0.5 + 0.5 * HR,
                                 0.25 + 0.75 * HR,
                                 HR)
    }else if(type=="early"){
        hazard.T <- hazard.C * c(HR,
                                 0.25 + 0.75 * HR,
                                 0.5 + 0.5 * HR,
                                 0.75 + 0.25 * HR,
                                 1)
    }
    
    group <- c(rep("T", n.T), rep("C", n.C)) # on attribut un groupe a chacun des patients

    TimeToEvent.C <- rexp(n.C, rate = hazard.C) # CJ1 ex:OS
    TimeToEvent.allT <- do.call(cbind,lapply(hazard.T, function(iH){rexp(n.T, rate = iH)}))

    TimeToEvent.T <- ifelse(TimeToEvent.allT[,1]<t[1],TimeToEvent.allT[,1],
                     ifelse(t[1]+TimeToEvent.allT[,2]<t[2],t[1]+TimeToEvent.allT[,2],
                     ifelse(t[2]+TimeToEvent.allT[,3]<t[3],t[2]+TimeToEvent.allT[,3],
                     ifelse(t[3]+TimeToEvent.allT[,4]<t[4],t[3]+TimeToEvent.allT[,4],
                            t[4]+TimeToEvent.allT[,5]))))

    TimeToEvent <- c(TimeToEvent.T,TimeToEvent.C)
    TimeToCens <- rexp(n.C+n.T, hazard.censoring) # censure uniforme de meme loi dans les 2 grpes
    Time <- pmin(TimeToCens, TimeToEvent) # on prend le minimum entre le temps jusque censure et le temps jusque evenement
    Event <- as.numeric(Time==TimeToEvent) # 1 si evenement 0 si censure
    df <- data.frame(group = group, time = Time, status = Event, timetoevent = TimeToEvent)
    return(df)
    
}

## * run simulation
res <- NULL ## start with an empty dataset
for (iGrid in 1:n.grid){   ## iGrid <- 1

    iN <- grid[iGrid,"n"]
    iHR <- grid[iGrid,"HR"]
    iHazard.C <- grid[iGrid,"hazard.C"]
    iHazard.censoring <- grid[iGrid,"hazard.censoring"]
    iType <- as.character(grid[iGrid,"type"])
    
    cat("Grid (",iGrid,"/",n.grid,") : n=",iN," | hazard ratio=", iHR," | hazard.C=",iHazard.C," | hazard.censoring=",iHazard.censoring," | type=",iType,"\n", sep = "")

        for (iRep in 1:simulation){ ## iRep <- 1
            cat(iRep," ")
            
            ## ** simulate data
            iData <- simData(n.C = iN,
                             n.T = iN,
                             HR = iHR,
                             hazard.C = iHazard.C,
                             hazard.censoring = iHazard.censoring,
                             type = iType)

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
            ## suppress message "Estimated p-value of 0 - consider increasing the number of boostrap samples"
            iRes <- rbind(cbind(scoring.rule = "GS", suppressMessages(confint2(BuyseGS))),
                          cbind(scoring.rule = "Gehan", suppressMessages(confint2(BuyseGehan))),
                          cbind(scoring.rule = "Gehan.C", suppressMessages(confint2(BuyseGehan_Corr))),
                          cbind(scoring.rule = "Peron", suppressMessages(confint2(BuysePeron))),
                          cbind(scoring.rule = "Peron.C", suppressMessages(confint2(BuysePeron_Corr)))
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
