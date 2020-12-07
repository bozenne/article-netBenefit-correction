library(data.table)
library(ggpubr)
library(ggplot2)


## * path to the results
path.scenario1 <- file.path("Results/scenario1-1TTE-tau0-exp")
path.scenario2 <- file.path("Results/scenario2-1TTE-tau50-exp")
path.scenario3 <- file.path("Results/scenario3-1TTE-earlyEffect")
path.scenario4 <- file.path("Results/scenario4-1TTE-delayEffect")
path.scenario5 <- file.path("Results/scenario5-TTE-bin")
path.scenario6 <- file.path("Results/scenario6-1TTE-tau0-weibull")
path.scenario7 <- file.path("Results/scenario7-1TTE-tau50-weibull")

## * function used to collect the results from different files
loadRes <- function(path, tempo.file = FALSE,
                    export.attribute = NULL, trace = TRUE){
    all.files <- list.files(path)
    file.tempo <- grep("(tempo)",all.files,value = TRUE)
    file.final <- setdiff(all.files, file.tempo)


    if(tempo.file){
        file.read <- file.tempo
    }else{
        file.read <- file.final
    }
    n.file <- length(file.read)

    myApply <- switch(as.character(as.logical(trace)),
                      "TRUE" = pbapply::pblapply,
                      "FALSE" = lapply)

    ls.out <- do.call(myApply, args = list(X = 1:n.file, FUN = function(iFile){
        iRead <- readRDS(file = file.path(path,file.read[iFile]))
        ## if("grip" %in% names(iRead)){
            ## names(iRead)[names(iRead)=="grip"] <- "grid"
        ## }
        iOut <- cbind(data.table::as.data.table(iRead),
                      file = file.read[iFile])
        if(!is.null(export.attribute)){
            attr(iOut,export.attribute) <- attr(iRead,export.attribute)
        }
        return(iOut)
    }))
    out <- do.call(rbind, ls.out)
    if(!is.null(export.attribute)){
        attr(out,export.attribute) <- attr(ls.out[[1]],export.attribute)
    }
    return(out)
}



## * Scenario 1
## ** load
dt.scenario1 <- loadRes(path.scenario1, export.attribute = "grid", tempo.file = TRUE)
dt.scenario1 <- data.table(attr(dt.scenario1,"grid")[dt.scenario1$grid,],dt.scenario1)

## ** summarize
dtS.scenario1 <- dt.scenario1[,.(rep= .N,
                                 pc.censoring = mean(pc.censoring, na.rm = TRUE),
                                 truth = mean(Delta),
                                 estimate.mean = mean(estimate, na.rm = TRUE),
                                 estimate.sd = sd(estimate, na.rm = TRUE)),
by = c("n","hazard.censoring","HR","scoring.rule")]

## ** export results
saveRDS(dtS.scenario1, file = "Results/dtS-scenario1.rds")

## * Scenario 2
## ** load
dt.scenario2 <- loadRes(path.scenario2, export.attribute = "grid", tempo.file = TRUE)
dt.scenario2 <- data.table(attr(dt.scenario2,"grid")[dt.scenario2$grid,],dt.scenario2)

## ** summarize
dtS.scenario2 <- dt.scenario2[,.(rep= .N,
                                 pc.censoring = mean(pc.censoring, na.rm = TRUE),
                                 truth = mean(Delta),
                                 estimate.mean = mean(estimate, na.rm = TRUE),
                                 estimate.sd = sd(estimate, na.rm = TRUE)),
                              by = c("n","hazard.censoring","HR","scoring.rule")]

## ** export results
saveRDS(dtS.scenario2, file = "Results/dtS-scenario2.rds")


## * Scenario 3
## ** load
dt.scenario3 <- loadRes(path.scenario3, export.attribute = "grid", tempo.file = TRUE)
dt.scenario3 <- data.table(attr(dt.scenario3,"grid")[dt.scenario3$grid,],dt.scenario3)

## ** summarize
dt.scenario3$Delta <- dt.scenario3[n == max(n) & scoring.rule == "GS", mean(estimate)]
dtS.scenario3 <- dt.scenario3[,.(rep= .N,
                                 pc.censoring = mean(pc.censoring, na.rm = TRUE),
                                 truth = mean(Delta),
                                 estimate.mean = mean(estimate, na.rm = TRUE),
                                 estimate.sd = sd(estimate, na.rm = TRUE)),
                              by = c("n","hazard.censoring","scoring.rule")]

## ** export results
saveRDS(dtS.scenario3, file = "Results/dtS-scenario3.rds")

## * Scenario 4
## ** load
dt.scenario4 <- loadRes(path.scenario4, export.attribute = "grid", tempo.file = TRUE)
dt.scenario4 <- data.table(attr(dt.scenario4,"grid")[dt.scenario4$grid,],dt.scenario4)

## ** summarize
dt.scenario4$Delta <- dt.scenario4[n == max(n) & scoring.rule == "GS", mean(estimate)]
dtS.scenario4 <- dt.scenario4[,.(rep= .N,
                                 pc.censoring = mean(pc.censoring, na.rm = TRUE),
                                 truth = mean(Delta),
                                 estimate.mean = mean(estimate, na.rm = TRUE),
                                 estimate.sd = sd(estimate, na.rm = TRUE)),
                              by = c("n","hazard.censoring","scoring.rule")]

## ** export results
saveRDS(dtS.scenario4, file = "Results/dtS-scenario4.rds")

## * Scenario 5
## ** load
dt.scenario5 <- loadRes(path.scenario5, export.attribute = "grid", tempo.file = TRUE)
dt.scenario5 <- data.table(attr(dt.scenario5,"grid")[dt.scenario5$grid,],dt.scenario5)

## ** summarize
dt.scenario5[, Delta := mean(.SD[n == max(n) & scoring.rule == "GS",estimate]), by = "index.endpoint"]
dtS.scenario5 <- dt.scenario5[,.(rep= .N,
                                 pc.censoring = mean(pc.censoring, na.rm = TRUE),
                                 truth = mean(Delta),
                                 estimate.mean = mean(estimate, na.rm = TRUE),
                                 estimate.sd = sd(estimate, na.rm = TRUE)),
                              by = c("n","hazard.censoring","scoring.rule","index.endpoint")]

## ** export results
saveRDS(dtS.scenario5, file = "Results/dtS-scenario5.rds")

## * Scenario 6
## ** load
dt.scenario6 <- loadRes(path.scenario6, export.attribute = "grid", tempo.file = TRUE)
dt.scenario6 <- data.table(attr(dt.scenario6,"grid")[dt.scenario6$grid,],dt.scenario6)

## ** summarize
dt.scenario6[, Delta := .SD[n == max(n) & scoring.rule == "GS", mean(estimate)], by = c("shape","hazard.T")]
dtS.scenario6 <- dt.scenario6[,.(rep= .N,
                                 pc.censoring = mean(pc.censoring, na.rm = TRUE),
                                 truth = mean(Delta),
                                 estimate.mean = mean(estimate, na.rm = TRUE),
                                 estimate.sd = sd(estimate, na.rm = TRUE)),
                              by = c("n","shape","hazard.T","HR","hazard.censoring","scoring.rule")]

## ** export results
saveRDS(dtS.scenario6, file = "Results/dtS-scenario6.rds")

## * Scenario 7
## ** load
dt.scenario7 <- loadRes(path.scenario7, export.attribute = "grid", tempo.file = TRUE)
dt.scenario7 <- data.table(attr(dt.scenario7,"grid")[dt.scenario7$grid,],dt.scenario7)

## ** summarize
dt.scenario7[, Delta := .SD[n == max(n) & scoring.rule == "GS", mean(estimate)], by = c("shape","hazard.T")]
dtS.scenario7 <- dt.scenario7[,.(rep= .N,
                                 pc.censoring = mean(pc.censoring, na.rm = TRUE),
                                 truth = mean(Delta),
                                 estimate.mean = mean(estimate, na.rm = TRUE),
                                 estimate.sd = sd(estimate, na.rm = TRUE)),
                              by = c("n","shape","hazard.T","HR","hazard.censoring","scoring.rule")]

## ** export results
saveRDS(dtS.scenario7, file = "Results/dtS-scenario7.rds")


