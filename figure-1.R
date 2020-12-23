## This file reproduces figure 1 of the article
save <- FALSE

## * Load R packages
library(data.table)
library(ggpubr)
library(ggplot2)

## * Load results
dtS.scenario1 <- readRDS("Results/dtS-scenario1.rds")
dtS.scenario2 <- readRDS("Results/dtS-scenario2.rds")
dtS.scenario3 <- readRDS("Results/dtS-scenario3.rds")
dtS.scenario4 <- readRDS("Results/dtS-scenario4.rds")
dtS.scenario5 <- readRDS("Results/dtS-scenario5.rds")

## * Figure 1
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

print(gg.figure1)

if(save){
    ggsave(gg.figure1, filename = file.path("figures/figure1.png"), width = 10)
    ggsave(gg.figure1, filename = file.path("figures/figure1.pdf"), width = 10)
}
