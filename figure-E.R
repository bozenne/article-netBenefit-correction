## This file generates figure E of the supplementary materials
save <- FALSE

## * Load R packages
library(data.table)
library(ggthemes)

## * Load results
dtS.scenario1 <- readRDS("Results/dtS-scenario1-power.rds")[scoring.rule!="GS"]
dtS.scenario1[, HR.char := ifelse(HR!=1,"under H1","under H0")]

dtS.scenario2 <- readRDS("Results/dtS-scenario2-power.rds")[scoring.rule!="GS"]
dtS.scenario2[, HR.char := ifelse(HR!=1,"under H1","under H0")]

dtS.scenario3 <- readRDS("Results/dtS-scenario3-power.rds")[scoring.rule!="GS"]
dtS.scenario3[, HR.char := ifelse(HR!=1,"under H1","under H0")]

dtS.scenario4 <- readRDS("Results/dtS-scenario4-power.rds")[scoring.rule!="GS"]
dtS.scenario4[, HR.char := ifelse(HR!=1,"under H1","under H0")]

## * Figure
## Note: under H0, scenario 1, 3, and 4 are the same
df.line <- rbind(data.frame(scenario = "scenario 1 (\u03C4=0, PH)", yintercept = 0.05, HR.char = "under H0"),
                 data.frame(scenario = "scenario 2 (\u03C4=50, PH)", yintercept = 0.05, HR.char = "under H0"),
                 data.frame(scenario = "scenario 3 (\u03C4=0, early effect)", yintercept = 0.05, HR.char = "under H0"),
                 data.frame(scenario = "scenario 4 (\u03C4=0, delayed effect)", yintercept = 0.05, HR.char = "under H0")
                 )

dtS.scenario <- rbind(cbind(scenario = "scenario 1 (\u03C4=0, PH)", dtS.scenario1),
                      cbind(scenario = "scenario 2 (\u03C4=50, PH)", dtS.scenario2),
                      cbind(scenario = "scenario 3 (\u03C4=0, early effect)", dtS.scenario1[HR.char == "under H0"]),
                      cbind(scenario = "scenario 3 (\u03C4=0, early effect)", dtS.scenario3),
                      cbind(scenario = "scenario 4 (\u03C4=0, delayed effect)", dtS.scenario1[HR.char == "under H0"]),
                      cbind(scenario = "scenario 4 (\u03C4=0, delayed effect)", dtS.scenario4))

figureE <- ggplot(dtS.scenario, aes(x=pc.censoring, y = rejection.rate, group=scoring.rule, color = scoring.rule))
figureE <- figureE + geom_hline(data = df.line, aes(yintercept = yintercept), col = "red", size = 1.5)
figureE <- figureE + geom_line(aes(linetype=scoring.rule), position = position_dodge(width = 0.025), size = 1.25)
figureE <- figureE + geom_point(aes(shape=scoring.rule), position = position_dodge(width = 0.025), size = 2)
figureE <- figureE + facet_grid(HR.char~scenario) + ylab("rejection rate")
figureE <- figureE + scale_color_manual(name = "",
                                        values = c("Gehan" = ggthemes::colorblind_pal()(8)[1],
                                                   "Gehan.C" = ggthemes::colorblind_pal()(8)[2],
                                                   "Peron" = ggthemes::colorblind_pal()(8)[3],
                                                   "Peron.C" = ggthemes::colorblind_pal()(8)[4]),
                                        labels = c("Gehan's scoring rule, uncorrected",
                                                   "Gehan's scoring rule, corrected",
                                                   "Peron's scoring rule, uncorrected",
                                                   "Peron's scoring rule, corrected"))
figureE <- figureE + scale_linetype_manual(name = "",
                                           values = c("Gehan" = 2,
                                                      "Gehan.C" = 2,
                                                      "Peron" = 1,
                                                      "Peron.C" = 1),
                                           labels = c("Gehan's scoring rule, uncorrected",
                                                      "Gehan's scoring rule, corrected",
                                                      "Peron's scoring rule, uncorrected",
                                                      "Peron's scoring rule, corrected"))
figureE <- figureE + scale_shape_manual(name = "",
                                        values = c("Gehan" = 17,
                                                   "Gehan.C" = 17,
                                                   "Peron" = 19,
                                                   "Peron.C" = 19),
                                        labels = c("Gehan's scoring rule, uncorrected",
                                                   "Gehan's scoring rule, corrected",
                                                   "Peron's scoring rule, uncorrected",
                                                   "Peron's scoring rule, corrected"))
figureE <- figureE + theme(legend.position = "bottom", legend.key.size = unit(2,"line"))
figureE <- figureE + guides(color=guide_legend(nrow=2,byrow=TRUE),linetype=guide_legend(nrow=2,byrow=TRUE))
figureE <- figureE + xlab("Proportion of censored observations")
figureE <- figureE + scale_x_continuous(breaks=seq(0,1,0.2))

if(save){
    ggsave(figureE, filename = file.path("figures/figureE.png"), width = 10)
    ggsave(figureE, filename = file.path("figures/figureE.pdf"), width = 10)
}
