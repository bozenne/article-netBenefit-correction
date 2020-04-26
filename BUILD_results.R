### BUILD_results.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: okt 17 2018 (09:40) 
## Version: 
## Last-Updated: apr 26 2020 (18:19) 
##           By: Brice Ozenne
##     Update #: 108
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

library(data.table)
library(ggpubr)
library(ggplot2)

## * path
path <- "P:/Cluster/GPC/Article-correction-Julien"
setwd(path)

source("FCT.R")
path.results <- "Results"
path.figures <- "figures"

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
saveRDS(dtS.scenario1[,.(n,hazard.censoring,HR,scoring.rule,rep,pc.censoring,truth,estimate.mean)],
        file = file.path(path.results,"dtS-scenario1.rds"))

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
saveRDS(dtS.scenario2[,.(n,hazard.censoring,HR,scoring.rule,rep,pc.censoring,truth,estimate.mean)],
        file = file.path(path.results,"dtS-scenario2.rds"))


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
saveRDS(dtS.scenario3[,.(n,hazard.censoring,scoring.rule,rep,pc.censoring,truth,estimate.mean)],
        file = file.path(path.results,"dtS-scenario3.rds"))

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
saveRDS(dtS.scenario4[,.(n,hazard.censoring,scoring.rule,rep,pc.censoring,truth,estimate.mean)],
        file = file.path(path.results,"dtS-scenario4.rds"))

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
                                 estimate.sd = sd(estimate, na.rm = TRUE),
                                 ## estimate.inf = quantile(estimate, probs = 0.025, na.rm = TRUE),
                                 ## estimate.sup = quantile(estimate, probs = 0.975, na.rm = TRUE),
                                 sd.mean = mean(se, na.rm = TRUE),
                                 coverage = mean((.SD$lower.ci <= .SD$Delta)*(.SD$Delta <= .SD$upper.ci), na.rm = TRUE)),
                              by = c("n","hazard.censoring","scoring.rule","index.endpoint")]

## ** display
## dtS.scenario5 <- readRDS("Results/dtS-scenario5.rds")
## bias.scenario5 <- GGarticle(dtS.scenario5[index.endpoint == 1])
bias.scenario5 <- GGarticle(dtS.scenario5[index.endpoint == 2])
bias.scenario5 <- bias.scenario5 + ggtitle("Scenario 5",
                                           subtitle = expression("One time-to event outcome and one binary outcome"))
coverage.scenario5 <- GGcoverage(dtS.scenario5)

## ** export results
saveRDS(dtS.scenario5[,.(n,hazard.censoring,index.endpoint,scoring.rule,rep,pc.censoring,truth,estimate.mean)],
        file = file.path(path.results,"dtS-scenario5.rds"))

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
                              by = c("n","shape","hazard.T","HR","hazard.censoring","scoring.rule")]

## ** display
## dtS.scenario6 <- readRDS("Results/dtS-scenario6.rds")
bias.scenario6 <- GGarticle(dtS.scenario6[HR==0.5], formula.grid = ~shape)
bias.scenario6 <- bias.scenario6 + ggtitle("Scenario 6",
                                           subtitle = expression("Weibull distribution, "*tau*" = 0"))

## ** export results
saveRDS(dtS.scenario6[,.(n,hazard.censoring,HR,scoring.rule,rep,pc.censoring,truth,estimate.mean)],
        file = file.path(path.results,"dtS-scenario6.rds"))

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
                                 estimate.sd = sd(estimate, na.rm = TRUE),
                                 ## estimate.inf = quantile(estimate, probs = 0.075, na.rm = TRUE),
                                 ## estimate.sup = quantile(estimate, probs = 0.975, na.rm = TRUE),
                                 sd.mean = mean(se, na.rm = TRUE),
                                 coverage = mean((.SD$lower.ci <= .SD$Delta)*(.SD$Delta <= .SD$upper.ci), na.rm = TRUE)),
                              by = c("n","shape","hazard.T","HR","hazard.censoring","scoring.rule")]

## ** display
## dtS.scenario7 <- readRDS("Results/dtS-scenario7.rds")
bias.scenario7 <- GGarticle(dtS.scenario7[HR==0.5], formula.grid = ~shape)
bias.scenario7 <- bias.scenario7 + ggtitle("Scenario 7",
                                           subtitle = expression("Weibull distribution, "*tau*" = 0"))

## ** export results
saveRDS(dtS.scenario7[,.(n,hazard.censoring,HR,scoring.rule,rep,pc.censoring,truth,estimate.mean)],
        file = file.path(path.results,"dtS-scenario7.rds"))


## * Figure 1

dtS.scenario1 <- readRDS("Results/dtS-scenario1.rds")
dtS.scenario2 <- readRDS("Results/dtS-scenario2.rds")
dtS.scenario3 <- readRDS("Results/dtS-scenario3.rds")
dtS.scenario4 <- readRDS("Results/dtS-scenario4.rds")
dtS.scenario5 <- readRDS("Results/dtS-scenario5.rds")
## dtS.scenario6 <- readRDS("Results/dtS-scenario6.rds")
## dtS.scenario7 <- readRDS("Results/dtS-scenario7.rds")


gg <- ggplot(mapping = aes(x = pc.censoring,
                           y = estimate.mean,
                           group = scoring.rule,
                           shape = scoring.rule,
                           linetype = scoring.rule,
                           color = scoring.rule))
gg <- gg + geom_point() + geom_line()
gg <- gg + scale_color_manual(name = "",
                              values = c("Gehan" = "gray",
                                         "Gehan.C" = "black",
                                         "Peron" = "gray",
                                         "Peron.C" = "black"),
                              labels = c("Gehan's scoring rule, uncorrected",
                                         "Gehan's scoring rule, corrected",
                                         "Peron's scoring rule, uncorrected",
                                         "Peron's scoring rule, corrected"))
gg <- gg + scale_linetype_manual(name = "",
                                 values = c("Gehan" = 2,
                                            "Gehan.C" = 2,
                                            "Peron" = 1,
                                            "Peron.C" = 1),
                                 labels = c("Gehan's scoring rule, uncorrected",
                                            "Gehan's scoring rule, corrected",
                                            "Peron's scoring rule, uncorrected",
                                            "Peron's scoring rule, corrected"))
gg <- gg + scale_shape_manual(name = "",
                              values = c("Gehan" = 17,
                                         "Gehan.C" = 17,
                                         "Peron" = 19,
                                         "Peron.C" = 19),
                              labels = c("Gehan's scoring rule, uncorrected",
                                         "Gehan's scoring rule, corrected",
                                         "Peron's scoring rule, uncorrected",
                                         "Peron's scoring rule, corrected"))
gg <- gg + theme(legend.position = "bottom", legend.key.size = unit(2,"line"))
gg <- gg + guides(color=guide_legend(nrow=2,byrow=TRUE),linetype=guide_legend(nrow=2,byrow=TRUE))
gg <- gg + xlab("Proportion of censored observations")
gg <- gg + ylab(expression("mean "*hat(Delta)))
gg <- gg + scale_x_continuous(breaks=seq(0,1,0.2))

gg.figure1a <- gg %+% dtS.scenario1[n==200 & scoring.rule != "GS" & HR==0.5]
gg.figure1b <- gg %+% dtS.scenario2[n==200 & scoring.rule != "GS" & HR==0.5]
gg.figure1c <- gg %+% dtS.scenario3[n==200 & scoring.rule != "GS"]
gg.figure1d <- gg %+% dtS.scenario4[n==200 & scoring.rule != "GS"]
gg.figure1e <- gg %+% dtS.scenario5[n==200 & scoring.rule != "GS" & index.endpoint == 2]

gg.figure1a <- gg.figure1a  + ggtitle("Scenario 1",
                                      subtitle = expression("Proportional hazards, HR = 0.5, "*tau*" = 0"))
gg.figure1b <- gg.figure1b  + ggtitle("Scenario 2",
                                      subtitle = expression("Proportional hazards, HR = 0.5, "*tau*" = 50"))
gg.figure1c <- gg.figure1c  + ggtitle("Scenario 3",
                                      subtitle = expression("Early treatment effect, "*tau*" = 0"))
gg.figure1d <- gg.figure1d   + ggtitle("Scenario 4",
                                       subtitle = expression("Delayed treatment effect, "*tau*" = 0"))
gg.figure1e <- gg.figure1e  + ggtitle("Scenario 5",
                                      subtitle = expression("One time-to event outcome and one binary outcome"))

gg.figure1 <- ggarrange(gg.figure1a, gg.figure1b, gg.figure1c, gg.figure1d, gg.figure1e,
                        common.legend = TRUE, legend = "bottom")

ggsave(gg.figure1, filename = file.path(path.figures,"figure1.png"), width = 10)
ggsave(gg.figure1, filename = file.path(path.figures,"figure1.pdf"), width = 10)

## * Figure 1 bis
gg.figure1bis <- ggarrange(bias.scenario1.H1,
                           bias.scenario2.H1,
                           bias.scenario3,
                           bias.scenario4,
                           bias.scenario5,
                           common.legend = TRUE,
                           legend = "bottom",
                           nrow = 5)
ggsave(gg.figure1bis, filename = file.path(path.figures,"figure1bis.png"), width = 10, height = 20)
ggsave(gg.figure1bis, filename = file.path(path.figures,"figure1bis.pdf"), width = 10, height = 20)

## * Figure 1 ter
gg.figure1ter <- ggarrange(bias.scenario6,
                           bias.scenario7,
                           common.legend = TRUE,
                           legend = "bottom",
                           nrow = 2)
ggsave(gg.figure1ter, filename = file.path(path.figures,"figure1ter.png"), width = 10, height = 10)
ggsave(gg.figure1ter, filename = file.path(path.figures,"figure1ter.pdf"), width = 10, height = 10)

######################################################################
### BUILD_results.R ends here
 
