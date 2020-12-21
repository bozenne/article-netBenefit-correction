### FCT-multicore.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: dec 13 2020 (09:12) 
## Version: 
## Last-Updated: dec 13 2020 (09:41) 
##           By: Brice Ozenne
##     Update #: 18
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * Scenario 1
myBATCH_scenario1 <- function(iter_sim, n.iter_sim, grid, simulation, filename){
    cat("run ",iter_sim," over ",n.iter_sim,"\n",sep="")

    iSeed <- iter_sim
    set.seed(iSeed)

    cat("seed: ",iSeed,"\n")

    ## ** path
    path <- "."
    path.res <- file.path(path,"Results","scenario1-1TTE-tau0-exp")
    if(dir.exists(path.res)==FALSE){
        dir.create(path.res)
    }
    path.output <- file.path(path,"output","scenario1-1TTE-tau0-exp")
    if(dir.exists(path.output)==FALSE){
        dir.create(path.output)
    }

    ## ** R packages
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

    ## ** settings
    n.grid <- NROW(grid)
    grid$HR <- grid$hazard.T / grid$hazard.C
    grid$Delta <- (1-grid$HR) / (1+grid$HR)

    formula.BT <- group ~ tte(time, status = status, threshold = 0)
    formula.GS <- group ~ cont(timetoevent, threshold = 0)

    ## ** generative distribution
    simData <- function(n.C, n.T, hazard.C, hazard.T, hazard.censoring){

        group <- c(rep("T", n.T), rep("C", n.C)) # on attribut un groupe a chacun des patients
        TimeToEvent <- c(rexp(n.T, rate=hazard.T), rexp(n.C, rate=hazard.C)) # CJ1 ex:OS
        TimeToCens <- rexp(n.C+n.T, hazard.censoring) # censure uniforme de meme loi dans les 2 grpes
        Time <- pmin(TimeToCens, TimeToEvent) # on prend le minimum entre le temps jusque censure et le temps jusque evenement
        Event <- as.numeric(Time==TimeToEvent) # 1 si evenement 0 si censure
        df <- data.frame(group = group, time = Time, status = Event, timetoevent = TimeToEvent)
        return(df)
    
    }

    ## ** simulation
    res <- NULL ## start with an empty dataset

    for (iGrid in 1:n.grid){   ## iGrid <- 44

        iN <- grid[iGrid,"n"]
        iHazard.T <- grid[iGrid,"hazard.T"]
        iHazard.C <- grid[iGrid,"hazard.C"]
        iHazard.censoring <- grid[iGrid,"hazard.censoring"]
    
        cat("Grid (",iGrid,"/",n.grid,") : n=",iN," | hazard.T=",iHazard.T," | hazard.C=",iHazard.C," | hazard.censoring=",iHazard.censoring,"\n", sep = "")

        for (iRep in 1:simulation){ ## iRep <- 1
            cat(iRep," ")
            
            ## ** simulate data
            iData <- simData(n.C = iN,
                             n.T = iN,
                             hazard.C = iHazard.C,
                             hazard.T = iHazard.T,
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

        ## ** export (tempo)
        resTempo <- res
        attr(resTempo,"grid") <- grid
        filename.tempo <- paste0(filename,"(tempo)_",iter_sim,".rds")
        saveRDS(resTempo, file = file.path(path.res,filename.tempo))
    }
    ## ** export
    attr(res,"grid") <- grid
    filename.final <- paste0(filename,"_",iter_sim,".rds")
    saveRDS(res, file = file.path(path.res,filename.final))
}

## * Scenario 2
myBATCH_scenario2 <- function(iter_sim, n.iter_sim, grid, simulation, filename){
    cat("run  ",iter_sim," over ",n.iter_sim,"\n",sep="")

    iSeed <- iter_sim
    set.seed(iSeed)

    cat("seed: ",iSeed,"\n")

    ## ** path
    path <- "."
    path.res <- file.path(path,"Results","scenario2-1TTE-tau50-exp")
    if(dir.exists(path.res)==FALSE){
        dir.create(path.res)
    }
    path.output <- file.path(path,"output","scenario2-1TTE-tau50-exp")
    if(dir.exists(path.output)==FALSE){
        dir.create(path.output)
    }

    ## ** R packages
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

    ## ** settings
    n.grid <- NROW(grid)
    grid$HR <- grid$hazard.T / grid$hazard.C
    grid$Delta <- (1-grid$HR) / (1+grid$HR)

    formula.BT <- group ~ tte(time, status = status, threshold = 50)
    formula.GS <- group ~ cont(timetoevent, threshold = 50)

    ## ** generative distribution

    simData <- function(n.C, n.T, hazard.C, hazard.T, hazard.censoring){

    group <- c(rep("T", n.T), rep("C", n.C)) # on attribut un groupe a chacun des patients
    TimeToEvent <- c(rexp(n.T, rate=hazard.T), rexp(n.C, rate=hazard.C)) # CJ1 ex:OS
    TimeToCens <- rexp(n.C+n.T, hazard.censoring) # censure uniforme de meme loi dans les 2 grpes
    Time <- pmin(TimeToCens, TimeToEvent) # on prend le minimum entre le temps jusque censure et le temps jusque evenement
    Event <- as.numeric(Time==TimeToEvent) # 1 si evenement 0 si censure
    df <- data.frame(group = group, time = Time, status = Event, timetoevent = TimeToEvent)
    return(df)
    
    }

    ## ** run simulation
    res <- NULL ## start with an empty dataset

    for (iGrid in 1:n.grid){   ## iGrid <- 36

        iN <- grid[iGrid,"n"]
        iHazard.T <- grid[iGrid,"hazard.T"]
        iHazard.C <- grid[iGrid,"hazard.C"]
        iHazard.censoring <- grid[iGrid,"hazard.censoring"]
    
        cat("Grid (",iGrid,"/",n.grid,") : n=",iN," | hazard.T=",iHazard.T," | hazard.C=",iHazard.C," | hazard.censoring=",iHazard.censoring,"\n", sep = "")

        for (iRep in 1:simulation){ ## iRep <- 1
            cat(iRep," ")
            
            ## *** simulate data
            iData <- simData(n.C = iN,
                             n.T = iN,
                             hazard.C = iHazard.C,
                             hazard.T = iHazard.T,
                             hazard.censoring = iHazard.censoring)

            ## *** run BuyseTest
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

            ## *** store results
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

        ## ** export (tempo)
        resTempo <- res
        attr(resTempo,"grid") <- grid
        filename.tempo <- paste0(filename,"(tempo)_",iter_sim,".rds")
        saveRDS(resTempo, file = file.path(path.res,filename.tempo))
    }
    ## ** export
    attr(res,"grid") <- grid
    filename.final <- paste0(filename,"_",iter_sim,".rds")
    saveRDS(res, file = file.path(path.res,filename.final))

}

## * Scenario 3
myBATCH_scenario3 <- function(iter_sim, n.iter_sim, grid, simulation, filename){
    cat("run ",iter_sim," over ",n.iter_sim,"\n",sep="")

    iSeed <- iter_sim
    set.seed(iSeed)

    cat("seed: ",iSeed,"\n")

    ## ** path
    path <- "."
    path.res <- file.path(path,"Results","scenario3-1TTE-earlyEffect")
    if(dir.exists(path.res)==FALSE){
        dir.create(path.res)
    }
    path.output <- file.path(path,"output","scenario3-1TTE-earlyEffect")
    if(dir.exists(path.output)==FALSE){
        dir.create(path.output)
    }

    ## ** R packages
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

    ## ** settings
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

    ## ** run simulation
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
        
        ## ** export (tempo)
        resTempo <- res
        attr(resTempo,"grid") <- grid
        filename.tempo <- paste0(filename,"(tempo)_",iter_sim,".rds")
        saveRDS(resTempo, file = file.path(path.res,filename.tempo))
    }
    ## ** export
    attr(res,"grid") <- grid
    filename.final <- paste0(filename,"_",iter_sim,".rds")
    saveRDS(res, file = file.path(path.res,filename.final))
}

## * Scenario 4
myBATCH_scenario4 <- function(iter_sim, n.iter_sim, grid, simulation, filename){
    cat("run ",iter_sim," over ",n.iter_sim,"\n",sep="")

    iSeed <- iter_sim
    set.seed(iSeed)

    cat("seed: ",iSeed,"\n")

    ## ** path
    path <- "."
    path.res <- file.path(path,"Results","scenario4-1TTE-delayEffect")
    if(dir.exists(path.res)==FALSE){
        dir.create(path.res)
    }
    path.output <- file.path(path,"output","scenario4-1TTE-delayEffect")
    if(dir.exists(path.output)==FALSE){
        dir.create(path.output)
    }

    ## ** R packages
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

    ## ** settings
    n.grid <- NROW(grid)

    formula.BT <- group ~ tte(time, status = status, threshold = 0)
    formula.GS <- group ~ cont(timetoevent, threshold = 0)

    ## ** generative distribution

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

    ## ** run simulation
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
            
            ## *** simulate data
            iData <- simData(n.C = iN,
                             n.T = iN,
                             HR = iHR,
                             hazard.C = iHazard.C,
                             hazard.censoring = iHazard.censoring,
                             type = iType)

            ## *** run BuyseTest
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

            ## *** store results
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
        
        ## ** export (tempo)
        resTempo <- res
        attr(resTempo,"grid") <- grid
        filename.tempo <- paste0(filename,"(tempo)_",iter_sim,".rds")
        saveRDS(resTempo, file = file.path(path.res,filename.tempo))
    }
    ## ** export
    attr(res,"grid") <- grid
    filename.final <- paste0(filename,"_",iter_sim,".rds")
    saveRDS(res, file = file.path(path.res,filename.final))
}


######################################################################
### FCT-multicore.R ends here
