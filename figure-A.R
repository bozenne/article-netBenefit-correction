## This file reproduces figure A of the supplementary material
save <- FALSE

## * Load R packages
library(data.table)
library(ggpubr)
library(ggplot2)

## * Load results
dtS.scenario1 <- readRDS("Results/dtS-scenario1.rds")
dtS.scenario1[, n2 := factor(paste0("sample size: ",n," per group"),
                             levels = paste0("sample size: ",unique(n)," per group"))]

dtS.scenario2 <- readRDS("Results/dtS-scenario2.rds")
dtS.scenario2[, n2 := factor(paste0("sample size: ",n," per group"),
                             levels = paste0("sample size: ",unique(n)," per group"))]

dtS.scenario3 <- readRDS("Results/dtS-scenario3.rds")
dtS.scenario3[, n2 := factor(paste0("sample size: ",n," per group"),
                             levels = paste0("sample size: ",unique(n)," per group"))]

dtS.scenario4 <- readRDS("Results/dtS-scenario4.rds")
dtS.scenario4[, n2 := factor(paste0("sample size: ",n," per group"),
                             levels = paste0("sample size: ",unique(n)," per group"))]

dtS.scenario5 <- readRDS("Results/dtS-scenario5.rds")
dtS.scenario5[, n2 := factor(paste0("sample size: ",n," per group"),
                             levels = paste0("sample size: ",unique(n)," per group"))]

## * Figure A
gg <- ggplot(mapping = aes(x = pc.censoring,
                           y = estimate.mean,
                           group = scoring.rule,
                           shape = scoring.rule,
                           linetype = scoring.rule,
                           color = scoring.rule))
gg <- gg + geom_point() + geom_line() + facet_wrap(~n2, nrow = 1)
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

gg.figureAa <- gg %+% dtS.scenario1[scoring.rule != "GS" & HR==0.5]
gg.figureAb <- gg %+% dtS.scenario2[scoring.rule != "GS" & HR==0.5]
gg.figureAc <- gg %+% dtS.scenario3[scoring.rule != "GS"]
gg.figureAd <- gg %+% dtS.scenario4[scoring.rule != "GS"]
gg.figureAe <- gg %+% dtS.scenario5[scoring.rule != "GS" & index.endpoint == 2]

gg.figureAa <- gg.figureAa + ggtitle("Scenario 1",
                                      subtitle = expression("Proportional hazards, HR = 0.5, "*tau*" = 0"))
gg.figureAa <- gg.figureAa + theme(plot.title = element_text(margin=margin(0,0,0,0)),
                                   plot.subtitle = element_text(margin=margin(0,0,0,0))
                                   )

gg.figureAb <- gg.figureAb + ggtitle("Scenario 2",
                                      subtitle = expression("Proportional hazards, HR = 0.5, "*tau*" = 50"))
gg.figureAb <- gg.figureAb + theme(plot.title = element_text(margin=margin(0,0,0,0)),
                                   plot.subtitle = element_text(margin=margin(0,0,0,0))
                                   )

gg.figureAc <- gg.figureAc  + ggtitle("Scenario 3",
                                      subtitle = expression("Early treatment effect, "*tau*" = 0"))
gg.figureAc <- gg.figureAc + theme(plot.title = element_text(margin=margin(0,0,0,0)),
                                   plot.subtitle = element_text(margin=margin(0,0,0,0))
                                   )

gg.figureAd <- gg.figureAd   + ggtitle("Scenario 4",
                                       subtitle = expression("Delayed treatment effect, "*tau*" = 0"))
gg.figureAd <- gg.figureAd + theme(plot.title = element_text(size = 10, margin=margin(0,0,0,0)),
                                   plot.subtitle = element_text(size = 8, margin=margin(0,0,0,0))
                                   )

gg.figureAe <- gg.figureAe  + ggtitle("Scenario 5",
                                      subtitle = expression("One time-to event outcome and one binary outcome"))
gg.figureAe <- gg.figureAe + theme(plot.title = element_text(size = 10, margin=margin(0,0,0,0)),
                                   plot.subtitle = element_text(size = 8, margin=margin(0,0,0,0))
                                   )

gg.figureA <- ggarrange(gg.figureAa, gg.figureAb, gg.figureAc, gg.figureAd, gg.figureAe,
                        common.legend = TRUE, legend = "bottom", ncol = 1)

if(save){
    ggsave(gg.figureA, filename = file.path("figures/figureA.pdf"), width = 10, height = 20)
    ggsave(gg.figureA, filename = file.path("figures/figureA.png"), width = 10, height = 20)
}

## * [not used] extra figure
if(FALSE){
    dtS.scenario6 <- readRDS("Results/dtS-scenario6.rds")
    dtS.scenario6[, shape2 := factor(paste0("shape parameter: ",shape),
                                     levels = paste0("shape parameter: ",unique(shape)))]

    dtS.scenario7 <- readRDS("Results/dtS-scenario7.rds")
    dtS.scenario7[, shape2 := factor(paste0("shape parameter: ",shape),
                                 levels = paste0("shape parameter: ",unique(shape)))]


    gg <- ggplot(mapping = aes(x = pc.censoring,
                               y = estimate.mean,
                               group = scoring.rule,
                               shape = scoring.rule,
                               linetype = scoring.rule,
                               color = scoring.rule))
    gg <- gg + geom_point() + geom_line() + facet_wrap(~shape2, nrow = 1)
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

    gg.figureAf <- gg %+% dtS.scenario6[scoring.rule != "GS" & HR == 0.5 & n == 200]
    gg.figureAg <- gg %+% dtS.scenario7[scoring.rule != "GS" & HR == 0.5 & n == 200]

    gg.figureAf <- gg.figureAf + ggtitle("Scenario 6",
                                         subtitle = expression("Weibull distribution, "*tau*" = 0"))
    gg.figureAf <- gg.figureAf + theme(plot.title = element_text(margin=margin(0,0,0,0)),
                                       plot.subtitle = element_text(margin=margin(0,0,0,0))
                                       )

    gg.figureAg <- gg.figureAg + ggtitle("Scenario 7",
                                         subtitle = expression("Weibull distribution, "*tau*" = 0"))
    gg.figureAg <- gg.figureAg + theme(plot.title = element_text(margin=margin(0,0,0,0)),
                                       plot.subtitle = element_text(margin=margin(0,0,0,0))
                                       )

    gg.figureAfg <- ggarrange(gg.figureAf, gg.figureAg,
                            common.legend = TRUE, legend = "bottom", ncol = 1)

    ggsave(gg.figureAfg, filename = file.path("figures/figureA-extra.pdf"), width = 10, height = 12)
    ggsave(gg.figureAfg, filename = file.path("figures/figureA-extra.png"), width = 10, height = 12)

}
