### BUILD_results.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: okt 17 2018 (09:40) 
## Version: 
## Last-Updated: apr 21 2020 (10:16) 
##           By: Brice Ozenne
##     Update #: 93
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

library(data.table)
library(ggplot2)

## * path
path <- "P:/Cluster/GPC/Article-correction-Julien"
setwd(path)

source("FCT.R")
path.results <- "Results"

path.scenario1 <- file.path(path.results,"scenario1-1TTE-tau0-exp")
path.scenario2 <- file.path(path.results,"scenario2-1TTE-tau50-exp")
path.scenario3 <- file.path(path.results,"scenario3-1TTE-earlyEffect")
path.scenario4 <- file.path(path.results,"scenario4-1TTE-delayEffect")
path.scenario5 <- file.path(path.results,"scenario5-TTE-bin")
path.scenario6 <- file.path(path.results,"scenario6-1TTE-tau0-weibull")
path.scenario7 <- file.path(path.results,"scenario7-1TTE-tau50-weibull")


## * Scenario 1
## ** load
dt.scenario1 <- loadRes(path.scenario1, export.attribute = "grid", tempo.file = TRUE)
dt.scenario1 <- data.table(attr(dt.scenario1,"grid")[dt.scenario1$grid,],dt.scenario1)

## ** summarize
dtS.scenario1 <- dt.scenario1[,.(rep= .N,
                                 pc.censoring = mean(pc.censoring, na.rm = TRUE),
                                 truth = mean(Delta),
                                 estimate.mean = mean(estimate, na.rm = TRUE),
                                 estimate.sd = sd(estimate, na.rm = TRUE),
                                 ## estimate.inf = quantile(estimate, probs = 0.025, na.rm = TRUE),
                                 ## estimate.sup = quantile(estimate, probs = 0.975, na.rm = TRUE),
                                 sd.mean = mean(se, na.rm = TRUE),
                                 coverage = mean((.SD$lower.ci <= .SD$Delta)*(.SD$Delta <= .SD$upper.ci), na.rm = TRUE)),
                              by = c("n","hazard.censoring","HR","scoring.rule")]

## ** display
## dtS.scenario1 <- readRDS("Results/dtS-scenario1.rds")

bias.scenario1.H1 <- GGarticle(dtS.scenario1[HR==0.5])
bias.scenario1.H1 <- bias.scenario1.H1 + ggtitle("Scenario 1",
                                                 subtitle = expression("Proportional hazards, HR = 0.5, "*tau*" = 0"))
coverage.scenario1.H0 <- GGcoverage(dtS.scenario1[HR==1])
coverage.scenario1.H1 <- GGcoverage(dtS.scenario1[HR==0.5])

## ** export results
ggsave(bias.scenario1.H1, filename = file.path(path.results,"fig-bias-scenario1-H1.png"))
ggsave(bias.scenario1.H1, filename = file.path(path.results,"fig-bias-scenario1-H1.eps"))

ggsave(coverage.scenario1.H0, filename = file.path(path.results,"fig-coverage-scenario1-H0.png"))
ggsave(coverage.scenario1.H0, filename = file.path(path.results,"fig-covarage-scenario1-H0.eps"))

ggsave(coverage.scenario1.H1, filename = file.path(path.results,"fig-coverage-scenario1-H1.png"))
ggsave(coverage.scenario1.H1, filename = file.path(path.results,"fig-covarage-scenario1-H1.eps"))

saveRDS(dtS.scenario1, file = file.path(path.results,"dtS-scenario1.rds"))

## * Scenario 2
## ** load
dt.scenario2 <- loadRes(path.scenario2, export.attribute = "grid", tempo.file = TRUE)
dt.scenario2 <- data.table(attr(dt.scenario2,"grid")[dt.scenario2$grid,],dt.scenario2)

## ** summarize
dtS.scenario2 <- dt.scenario2[,.(rep= .N,
                                 pc.censoring = mean(pc.censoring, na.rm = TRUE),
                                 truth = mean(Delta),
                                 estimate.mean = mean(estimate, na.rm = TRUE),
                                 estimate.sd = sd(estimate, na.rm = TRUE),
                                 ## estimate.inf = quantile(estimate, probs = 0.025, na.rm = TRUE),
                                 ## estimate.sup = quantile(estimate, probs = 0.975, na.rm = TRUE),
                                 sd.mean = mean(se, na.rm = TRUE),
                                 coverage = mean((.SD$lower.ci <= .SD$Delta)*(.SD$Delta <= .SD$upper.ci), na.rm = TRUE)),
                              by = c("n","hazard.censoring","HR","scoring.rule")]

## ** display
## dtS.scenario2 <- readRDS("Results/dtS-scenario2.rds")
bias.scenario2.H1 <- GGarticle(dtS.scenario2[HR==0.5])
bias.scenario2.H1 <- bias.scenario2.H1 + ggtitle("Scenario 2",
                                                 subtitle = expression("Proportional hazards, HR = 0.5, "*tau*" = 50"))
coverage.scenario2.H0 <- GGcoverage(dtS.scenario2[HR==1])
coverage.scenario2.H1 <- GGcoverage(dtS.scenario2[HR==0.5])

## ** export results
ggsave(bias.scenario2.H1, filename = file.path(path.results,"fig-bias-scenario2-H1.png"))
ggsave(bias.scenario2.H1, filename = file.path(path.results,"fig-bias-scenario2-H1.eps"))

ggsave(coverage.scenario2.H0, filename = file.path(path.results,"fig-coverage-scenario2-H0.png"))
ggsave(coverage.scenario2.H0, filename = file.path(path.results,"fig-covarage-scenario2-H0.eps"))

ggsave(coverage.scenario2.H1, filename = file.path(path.results,"fig-coverage-scenario2-H1.png"))
ggsave(coverage.scenario2.H1, filename = file.path(path.results,"fig-covarage-scenario2-H1.eps"))

saveRDS(dtS.scenario2, file = file.path(path.results,"dtS-scenario2.rds"))

## * Scenario 3
## ** load
dt.scenario3 <- loadRes(path.scenario3, export.attribute = "grid", tempo.file = TRUE)
setnames(dt.scenario3, old = "grip", new = "grid")
dt.scenario3 <- data.table(attr(dt.scenario3,"grid")[dt.scenario3$grid,],dt.scenario3)

## ** summarize
dt.scenario3$Delta <- dt.scenario3[n == max(n) & scoring.rule == "GS", mean(estimate)]
dtS.scenario3 <- dt.scenario3[,.(rep= .N,
                                 pc.censoring = mean(pc.censoring, na.rm = TRUE),
                                 truth = mean(Delta),
                                 estimate.mean = mean(estimate, na.rm = TRUE),
                                 estimate.sd = sd(estimate, na.rm = TRUE),
                                 ## estimate.inf = quantile(estimate, probs = 0.025, na.rm = TRUE),
                                 ## estimate.sup = quantile(estimate, probs = 0.975, na.rm = TRUE),
                                 sd.mean = mean(se, na.rm = TRUE),
                                 coverage = mean((.SD$lower.ci <= .SD$Delta)*(.SD$Delta <= .SD$upper.ci), na.rm = TRUE)),
                              by = c("n","hazard.censoring","scoring.rule")]

## ** display
## dtS.scenario3 <- readRDS("Results/dtS-scenario3.rds")
bias.scenario3 <- GGarticle(dtS.scenario3)
bias.scenario3 <- bias.scenario3 + ggtitle("Scenario 3",
                                           subtitle = expression("Early treatment effect, "*tau*" = 0"))
coverage.scenario3 <- GGcoverage(dtS.scenario3)

## ** export results
ggsave(bias.scenario3, filename = file.path(path.results,"fig-bias-scenario3.png"))
ggsave(bias.scenario3, filename = file.path(path.results,"fig-bias-scenario3.eps"))

ggsave(coverage.scenario3, filename = file.path(path.results,"fig-coverage-scenario3.png"))
ggsave(coverage.scenario3, filename = file.path(path.results,"fig-covarage-scenario3.eps"))

saveRDS(dtS.scenario3, file = file.path(path.results,"dtS-scenario3.rds"))

## * Scenario 4
## ** load
dt.scenario4 <- loadRes(path.scenario4, export.attribute = "grid", tempo.file = TRUE)
setnames(dt.scenario4, old = "grip", new = "grid")
dt.scenario4 <- data.table(attr(dt.scenario4,"grid")[dt.scenario4$grid,],dt.scenario4)

## ** summarize
dt.scenario4$Delta <- dt.scenario4[n == max(n) & scoring.rule == "GS", mean(estimate)]
dtS.scenario4 <- dt.scenario4[,.(rep= .N,
                                 pc.censoring = mean(pc.censoring, na.rm = TRUE),
                                 truth = mean(Delta),
                                 estimate.mean = mean(estimate, na.rm = TRUE),
                                 estimate.sd = sd(estimate, na.rm = TRUE),
                                 ## estimate.inf = quantile(estimate, probs = 0.025, na.rm = TRUE),
                                 ## estimate.sup = quantile(estimate, probs = 0.975, na.rm = TRUE),
                                 sd.mean = mean(se, na.rm = TRUE),
                                 coverage = mean((.SD$lower.ci <= .SD$Delta)*(.SD$Delta <= .SD$upper.ci), na.rm = TRUE)),
                              by = c("n","hazard.censoring","scoring.rule")]

## ** display
## dtS.scenario4 <- readRDS("Results/dtS-scenario4.rds")
bias.scenario4 <- GGarticle(dtS.scenario4)
bias.scenario4 <- bias.scenario4 + ggtitle("Scenario 4",
                                           subtitle = expression("Delayed treatment effect, "*tau*" = 0"))
coverage.scenario4 <- GGcoverage(dtS.scenario4)

## ** export results
ggsave(bias.scenario4, filename = file.path(path.results,"fig-bias-scenario4.png"))
ggsave(bias.scenario4, filename = file.path(path.results,"fig-bias-scenario4.eps"))

ggsave(coverage.scenario4, filename = file.path(path.results,"fig-coverage-scenario4.png"))
ggsave(coverage.scenario4, filename = file.path(path.results,"fig-covarage-scenario4.eps"))

saveRDS(dtS.scenario4, file = file.path(path.results,"dtS-scenario4.rds"))

## * Scenario 5
## ** load
dt.scenario5 <- loadRes(path.scenario5, export.attribute = "grid", tempo.file = TRUE)
setnames(dt.scenario5, old = "grip", new = "grid")
dt.scenario5 <- data.table(attr(dt.scenario5,"grid")[dt.scenario5$grid,],dt.scenario5)

## ** summarize
dt.scenario5$Delta <- dt.scenario5[n == max(n) & scoring.rule == "GS", mean(estimate)]
dtS.scenario5 <- dt.scenario5[,.(rep= .N,
                                 pc.censoring = mean(pc.censoring, na.rm = TRUE),
                                 truth = mean(Delta),
                                 estimate.mean = mean(estimate, na.rm = TRUE),
                                 estimate.sd = sd(estimate, na.rm = TRUE),
                                 ## estimate.inf = quantile(estimate, probs = 0.025, na.rm = TRUE),
                                 ## estimate.sup = quantile(estimate, probs = 0.975, na.rm = TRUE),
                                 sd.mean = mean(se, na.rm = TRUE),
                                 coverage = mean((.SD$lower.ci <= .SD$Delta)*(.SD$Delta <= .SD$upper.ci), na.rm = TRUE)),
                              by = c("n","hazard.censoring","scoring.rule")]

## ** display
## dtS.scenario5 <- readRDS("Results/dtS-scenario5.rds")
bias.scenario5 <- GGarticle(dtS.scenario5)
bias.scenario5 <- bias.scenario5 + ggtitle("Scenario 5",
                                           subtitle = expression("One time-to event outcome and one binary outcome"))
coverage.scenario5 <- GGcoverage(dtS.scenario5)

## ** export results
ggsave(bias.scenario5, filename = file.path(path.results,"fig-bias-scenario5.png"))
ggsave(bias.scenario5, filename = file.path(path.results,"fig-bias-scenario5.eps"))

ggsave(coverage.scenario5, filename = file.path(path.results,"fig-coverage-scenario5.png"))
ggsave(coverage.scenario5, filename = file.path(path.results,"fig-covarage-scenario5.eps"))

saveRDS(dtS.scenario5, file = file.path(path.results,"dtS-scenario5.rds"))

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
                                 estimate.sd = sd(estimate, na.rm = TRUE),
                                 ## estimate.inf = quantile(estimate, probs = 0.065, na.rm = TRUE),
                                 ## estimate.sup = quantile(estimate, probs = 0.975, na.rm = TRUE),
                                 sd.mean = mean(se, na.rm = TRUE),
                                 coverage = mean((.SD$lower.ci <= .SD$Delta)*(.SD$Delta <= .SD$upper.ci), na.rm = TRUE)),
                              by = c("shape","hazard.T","hazard.censoring","scoring.rule")]

## ** display
## dtS.scenario6 <- readRDS("Results/dtS-scenario6.rds")
bias.scenario6 <- GGarticle(dtS.scenario6[HR==0.5], formula.grid = hazard.T~shape)
bias.scenario6 <- bias.scenario6.H1 + ggtitle("Scenario 6",
                                                 subtitle = expression("Weibull distribution, "*tau*" = 0"))

## ** export results
ggsave(bias.scenario6, filename = file.path(path.results,"fig-bias-scenario6.png"))
ggsave(bias.scenario6, filename = file.path(path.results,"fig-bias-scenario6.eps"))

saveRDS(dtS.scenario6, file = file.path(path.results,"dtS-scenario6.rds"))


######################################################################
### BUILD_results.R ends here
