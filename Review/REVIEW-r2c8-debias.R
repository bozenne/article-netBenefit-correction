### debias.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: apr  9 2020 (15:22) 
## Version: 
## Last-Updated: apr  9 2020 (20:08) 
##           By: Brice Ozenne
##     Update #: 25
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

path <- "~/Dropbox/GPC/"

## * library
library(BuyseTest)
library(data.table)
library(pbapply)
library(ggplot2)
library(ggthemes)

## * Example with residual standard errorbar
p <- 10
mySim <- function(n){
    return(list(X = do.call(cbind, lapply(1:p, function(i){rnorm(n)})),
                Y = rnorm(n)))
}

statistic <- function(data, n.sim = 1e3){
    n.obs <- NROW(data$X)
        
    e <- .lm.fit(x = data$X, y = data$Y)
    out <- data.frame(ML = mean(e$residuals^2),
                      bias = 0.9*mean(e$residuals^2),
                      REML = sum(e$residuals^2)/(n.obs-e$rank),
                      ML.boot = as.numeric(NA),
                      bias.boot = as.numeric(NA))

    e.boot <- do.call(rbind,lapply(1:n.sim, function(i){
        iIndex <- sample.int(n.obs, replace = TRUE)
        iE <- .lm.fit(x = data$X[iIndex,,drop=FALSE], y = data$Y[iIndex])
        c("ML"=mean(iE$residuals^2), "bias" = 0.9*mean(iE$residuals^2))
    }))

    out["ML.boot"] <- 2*out["ML"] - mean(e.boot[,"ML"])
    out["bias.boot"] <- 2*out["bias"] - mean(e.boot[,"bias"])
    return(out)
}

set.seed(10)
ls.sim <- pblapply(1:200, function(iSim){
    rbind(cbind(sim = iSim, n = 15 ,statistic(mySim(20))),
          cbind(sim = iSim, n = 25 ,statistic(mySim(50))),
          cbind(sim = iSim, n = 40 ,statistic(mySim(100))),
          cbind(sim = iSim, n = 1000, statistic(mySim(1000))))
})

dt.sim <- as.data.table(do.call(rbind, ls.sim))
dtL.sim <- melt(dt.sim, id.vars = c("sim","n"),
                value.name = "estimate", variable.name = "estimator")
dtL.sim[, n.char := factor(n)]

gg.bias <- ggplot(dtL.sim, aes(x = n.char, y = estimate, color = estimator))
gg.bias <- gg.bias + geom_hline(yintercept = 1, color = "purple")
gg.bias <- gg.bias + geom_boxplot()
gg.bias <- gg.bias + scale_color_colorblind("estimator",
                                            labels = c("ML" = "ML", "ML.boot" = "ML + bootstrap correction", "REML" = "REML",
                                                       "bias" = "0.9*ML", "bias.boot" = "0.9*ML + bootstrap correction"))
gg.bias <- gg.bias + xlab("sample size") + ylab("estimate")
gg.bias <- gg.bias + theme(legend.position="bottom")
gg.bias

if(export){
    saveRDS(dtL.sim, file = file.path(path,"results-review","res-reviewer2c8-lm.rds"))
    ggsave(gg.bias, file = file.path(path,"figures-review","fig-reviewer2c8-lm.pdf"))
}

## * Application to BuyseTest
## ** Gehan
n <- 100
rates.C <- 1/4
HR <- 1/2
Delta.truth <- (1-HR)/(1+HR)

set.seed(10)
ls.sim2 <- pblapply(1:200, function(iSim){

    d <- simBuyseTest(n, argsTTE = list(rates.T = HR*rates.C, rates.C = rates.C))
    e.Gehan <- BuyseTest(treatment ~ tte(eventtime, status), data = d, scoring.rule = "Gehan",
                         method.inference = "bootstrap", trace = FALSE)

    e.GehanC <- BuyseTest(treatment ~ tte(eventtime, status), data = d, scoring.rule = "Gehan",
                          correction.uninf = 1, method.inference = "bootstrap", trace = FALSE)

    out <- c("sim" = iSim,
             "n" = n,
             "uninformative"= as.double(e.Gehan@count.uninf/e.Gehan@n.pairs),
             "Gehan" = e.Gehan@Delta[1,"netBenefit"],
             "Gehan+boot"= 2*e.Gehan@Delta[1,"netBenefit"] - mean(e.Gehan@DeltaResampling[,,"netBenefit"]),
             "Gehan+correction" = e.GehanC@Delta[1,"netBenefit"],
             "Gehan+correction+boot" = 2*e.GehanC@Delta[1,"netBenefit"] - mean(e.GehanC@DeltaResampling[,,"netBenefit"])
             )
    return(out)
})

dt.sim2 <- as.data.table(do.call(rbind,ls.sim2))
quantile(dt.sim2[["uninformative"]])
##       0%      25%      50%      75%     100% 
## 0.738400 0.825625 0.843450 0.865925 0.916000 
quantile(dt.sim2[["Gehan"]]-dt.sim2[["Gehan+boot"]])
##            0%           25%           50%           75%          100% 
## -3.489154e-03 -8.980612e-04 -5.553893e-06  7.392316e-04  3.064180e-03 
quantile(dt.sim2[["Gehan+correction"]]-dt.sim2[["Gehan+correction+boot"]])
##            0%           25%           50%           75%          100% 
## -2.270159e-02 -4.438209e-03 -9.617892e-05  4.228331e-03  1.811012e-02 


dtL.sim2 <- melt(dt.sim2, id.vars = c("sim","n","uninformative"),
                value.name = "estimate", variable.name = "estimator")

gg.bias2 <- ggplot(dtL.sim2, aes(y = estimate, color = estimator))
gg.bias2 <- gg.bias2 + geom_hline(yintercept = Delta.truth, color = "purple")
gg.bias2 <- gg.bias2 + geom_boxplot()
gg.bias2 <- gg.bias2 + xlab("sample size") + ylab("estimate")
gg.bias2 <- gg.bias2 + scale_color_colorblind("estimator",  guide=guide_legend(ncol=2),
                                              labels = c("Gehan" = "\u0394", "Gehan+correction" = "\u0394 (c)",
                                                         "Gehan+boot" = "\u0394 + bootstrap correction", "Gehan+correction+boot" = "\u0394 (c) + bootstrap correction"))
gg.bias2 <- gg.bias2 + theme(legend.position="bottom", text = element_text(size=16))
gg.bias2 <- gg.bias2 + ggtitle("Gehan's scoring rule")
gg.bias2

if(export){
    saveRDS(dtL.sim2, file = file.path(path,"results-review","res-reviewer2c8-Gehan.rds"))
    ggsave(gg.bias2, file = file.path(path,"figures-review","fig-reviewer2c8-Gehan.pdf"), device = cairo_pdf)
}

## ** Peron
n <- 100
rates.C <- 1/4
HR <- 1/2
Delta.truth <- (1-HR)/(1+HR)

set.seed(10)
ls.sim3 <- pblapply(1:200, function(iSim){

    d <- simBuyseTest(n, argsTTE = list(rates.T = HR*rates.C, rates.C = rates.C))
    e.Peron <- BuyseTest(treatment ~ tte(eventtime, status), data = d, scoring.rule = "Peron",
                         method.inference = "bootstrap", trace = FALSE, n.resampling = 100)

    e.PeronC <- BuyseTest(treatment ~ tte(eventtime, status), data = d, scoring.rule = "Peron",
                          correction.uninf = 1, method.inference = "none", trace = FALSE)

    out <- c("sim" = iSim,
             "n" = n,
             "uninformative"= as.double(e.Peron@count.uninf/e.Peron@n.pairs),
             "Peron" = e.Peron@Delta[1,"netBenefit"],
             "Peron+boot"= 2*e.Peron@Delta[1,"netBenefit"] - mean(e.Peron@DeltaResampling[,,"netBenefit"]),
             "Peron+correction" = e.PeronC@Delta[1,"netBenefit"]
             )
    return(out)
})

dt.sim3 <- as.data.table(do.call(rbind,ls.sim3))
quantile(dt.sim3[["uninformative"]])
##        0%       25%       50%       75%      100% 
## 0.0000000 0.1421162 0.2510794 0.3480318 0.6244158 
quantile(dt.sim3[["Peron"]]-dt.sim3[["Peron+boot"]])
##           0%          25%          50%          75%         100% 
## -0.221564448 -0.024676740 -0.004073774  0.006895081  0.183947209 


dtL.sim3 <- melt(dt.sim3, id.vars = c("sim","n","uninformative"),
                 value.name = "estimate", variable.name = "estimator")

gg.bias3 <- ggplot(dtL.sim3, aes(y = estimate, color = estimator))
gg.bias3 <- gg.bias3 + geom_hline(yintercept = Delta.truth, color = "purple")
gg.bias3 <- gg.bias3 + geom_boxplot()
gg.bias3 <- gg.bias3 + xlab("sample size") + ylab("estimate")
gg.bias3 <- gg.bias3 + scale_color_colorblind("estimator",
                                              labels = c("Peron" = "\u0394", "Peron+correction" = "\u0394 (c)",
                                                         "Peron+boot" = "\u0394 + bootstrap correction", "Peron+correction+boot" = "\u0394 (c) + bootstrap correction"))
gg.bias3 <- gg.bias3 + theme(legend.position="bottom", text = element_text(size=20))
gg.bias3

if(export){
    saveRDS(dtL.sim3, file = file.path(path,"results-review","res-reviewer2c8-Peron.rds"))
    ggsave(gg.bias3, file = file.path(path,"figures-review","fig-reviewer2c8-Peron.pdf"), device = cairo_pdf)
}

##----------------------------------------------------------------------
### debias.R ends here
