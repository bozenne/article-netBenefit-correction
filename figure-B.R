## This file reproduces figure B in the appendix

save <- FALSE
## * Load R packages
library(survival)

## * Load Data
df.prodige <- read.csv("data/data_PRODIGE.csv")

## * Figure B
vec.time <- seq(0,36,by=6)

if(save){
    pdf("figures/figureB.pdf")
    ## png("figures/figureB.png")
}
layout(matrix(c(1,1,1,2), 4, 1, byrow = TRUE))
par(mar = c(4,12,2,2), mgp = c(10, 1, 0), adj = 0.5)
plot(survfit(Surv(OS, etat) ~ bras, data = df.prodige), 
     ylab = "Overall survival", lwd = 2,
     mark.time = TRUE, lty = 1:2, bty = "l",
     col = c("grey","black"), cex.lab=1.5, axes = FALSE, xlim = c(0,36))
axis(1, at = vec.time, cex.axis=1.5)
axis(2, at = seq(0,1,by=0.25), cex.axis=1.5)
box(bty = "l")
legend(x = 24, y = 0.75, title = "Treatment group", legend=c("Gemcitabine", "FOLFIRINOX"),
       col=c("grey", "black"), lty=1:2, cex=1.5, bty = "n", pch=c("|",'|'))
text(x = -0.5, y = 0.25, label = "HR = 0.57 \n 95%CI (0.45 - 0.72) \n P<.0001",
     pos = 4, offset = 0, cex = 1.35)
par(mar = c(4,12,2,2), mgp = c(3, 1, 0), adj = 0.5)
title(xlab = "Time (months)", cex.lab = 1.5)

df.atRisk <- rbind(data.frame(value = sapply(vec.time, function(iThreshold){sum(iThreshold<=df.prodige[df.prodige$bras==1,"OS"])}),
                              bras = "FOLFIRINOX",
                              time = vec.time),
                   data.frame(value = sapply(vec.time, function(iThreshold){sum(iThreshold<=df.prodige[df.prodige$bras==0,"OS"])}),
                              bras = "Gemcitabine",
                              time = vec.time)
                   )
df.atRisk$bras <- relevel(as.factor(df.atRisk$bras), "FOLFIRINOX")
par(mar = c(4,12,2,2), mgp = c(10, 1, 0))
plot(x = rep(df.atRisk$time,4), y = rep(c(0.5,1,2,2.5),NROW(df.atRisk)), type = "n", ylab = "Treatment group",
     cex.lab = 1.5, xlim = range(vec.time), ylim = c(0,2.5), axes = FALSE)
par(mar = c(4,12,2,2), mgp = c(3, 1, 0), adj = 0)
title(main = "Number at risk", cex.main = 1.5)
par(mar = c(4,12,2,2), mgp = c(3, 1, 0), adj = 0.5)
title(xlab = "Time (months)", cex.lab = 1.5)
box(bty = "l")
axis(1, at = vec.time, cex.axis = 1.5)
axis(2, at = c(1:2), label = unique(sort(df.atRisk$bras)), cex.axis = 1.5, las = 2)
text(x = df.atRisk$time, y = df.atRisk$bras, label = df.atRisk$value, cex = 2, bty = "l")

if(save){
    dev.off()
}
