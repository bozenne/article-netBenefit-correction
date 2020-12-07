## This file reproduces figure C in the appendix

save <- FALSE

## * Load R packages
library(survival)

## * Load Data
df.eortc <- read.csv("data/data_EORTC-22961.csv")

## the time to event is 0 for some individuals which is a problem for the software
## so they are set to a very small value just after 0
df.eortc[df.eortc$dsurvyears==0,"dsurvyears"] <- 1e-5
df.eortc$trt2 <- relevel(as.factor(df.eortc$trt2), "Short ADT")

## * Figure B
vec.time <- seq(0,10,by=1)

if(save){
    pdf("figures/figureC.pdf")
    ## png("figures/figureC.png")
}
layout(matrix(c(1,1,1,2), 4, 1, byrow = TRUE))
par(mar = c(4,12,2,2), mgp = c(10, 1, 0), adj = 0.5)
plot(survfit(Surv(dsurvyears, DC) ~ trt2, data = df.eortc), 
     ylab = "Overall survival", cex = 1.5,
     mark.time = TRUE, lty = 1:2, bty = "l", lwd = 2,
     col = c("grey","black"), cex.lab=1.5, axes = FALSE, xlim = c(0,10))
axis(1, at = vec.time, cex.axis=1.5)
axis(2, at = seq(0,1,by=0.25), cex.axis=1.5)
box(bty = "l")
legend(x = 1, y = 0.75, title = "Treatment group", legend=c("short-ADT", "long-ADT"),
       col=c("grey", "black"), lty=1:2, cex=1.5, bty = "n", pch=c("|",'|'))
text(x = 1, y = 0.25, label = "HR = 1.43 \n 95%CI (1.10 - 1.89) \n P=.0075",
     pos = 4, offset = 0, cex = 1.35)
par(mar = c(4,12,2,2), mgp = c(3, 1, 0), adj = 0.5)
title(xlab = "Time (years)", cex.lab = 1.5)

df.atRisk <- rbind(data.frame(value = sapply(vec.time, function(iThreshold){sum(iThreshold<=df.eortc[df.eortc$trt2=="Short ADT","dsurvyears"])}),
                              trt2 = "Short ADT",
                              time = vec.time),
                   data.frame(value = sapply(vec.time, function(iThreshold){sum(iThreshold<=df.eortc[df.eortc$trt2=="Long ADT","dsurvyears"])}),
                              trt2 = "Long ADT",
                              time = vec.time)
                   )
df.atRisk$trt2 <- relevel(as.factor(df.atRisk$trt2), "Long ADT")
par(mar = c(4,12,2,2), mgp = c(10, 1, 0))
plot(x = rep(df.atRisk$time,4), y = rep(c(0.5,1,2,2.5),NROW(df.atRisk)), type = "n", ylab = "Treatment group",
     cex.lab = 1.5, xlim = range(vec.time), ylim = c(0,2.5), axes = FALSE)
par(mar = c(4,12,2,2), mgp = c(3, 1, 0), adj = 0)
title(main = "Number at risk", cex.main = 1.5)
par(mar = c(4,12,2,2), mgp = c(3, 1, 0), adj = 0.5)
title(xlab = "Time (years)", cex.lab = 1.5)
box(bty = "l")
axis(1, at = vec.time, cex.axis = 1.5)
axis(2, at = c(1:2), label = unique(sort(df.atRisk$trt2)), cex.axis = 1.5, las = 2)
text(x = df.atRisk$time, y = df.atRisk$trt2, label = df.atRisk$value, cex = 2, bty = "l")

if(save){
    dev.off()
}
