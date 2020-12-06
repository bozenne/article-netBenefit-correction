## This file was used to generate the results of the section "The Prodige 4 study: metastatic pancreatic cancer randomized trial"

path <- "~/Documents/GitHub/article-netBenefit-correction/"
setwd(path)
options(width = 130)

## * Load R packages
library(BuyseTest)
if(packageVersion("BuyseTest") < "2.2.0"){
    warning("This script has been check for BuyseTest version 2.2.0 (and above). \n",
            "Consider using devtools::install_github(\"bozenne/BuyseTest\") to get the latest stable version of the BuyseTest package. \n")
}
library(survival)
library(prodlim)

## * Load Data
df.prodige <- read.csv("data/data_PRODIGE.csv")

## * Descriptive statistics
## ** sample size and randomization ratio
NROW(df.prodige)
## [1] 342

table(df.prodige$bras)
##   0   1 
## 171 171 

## ** death
sum(df.prodige$etat==1)
## [1] 273
tapply(df.prodige$etat==1,df.prodige$bras,sum)
##   0   1 
## 147 126 

## ** censoring
100*mean(df.prodige$etat==0)
## [1] 0.2017544

## ** Hazard ratio
summary(coxph(Surv(OS, etat)~bras, data = df.prodige))

##   n= 342, number of events= 273 

##         coef exp(coef) se(coef)      z Pr(>|z|)    
## bras -0.5667    0.5674   0.1229 -4.612 3.99e-06 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##      exp(coef) exp(-coef) lower .95 upper .95
## bras    0.5674      1.762     0.446    0.7219

## ** Figure B
vec.time <- seq(0,36,by=6)

## dev.off()
layout(matrix(c(1,1,1,2), 4, 1, byrow = TRUE))
par(mar = c(4,12,2,2), mgp = c(10, 1, 0), adj = 0.5)
plot(survfit(Surv(OS, etat) ~ bras, data = df.prodige), 
     ylab = "Overall survival",
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

df.atRisk <- rbind(data.frame(value = sapply(seq(0,36,by=6), function(iThreshold){sum(iThreshold<=df.prodige[df.prodige$bras==0,"OS"])}),
                              bras = "Gemcitabine",
                              time = vec.time),
                   data.frame(value = sapply(seq(0,36,by=6), function(iThreshold){sum(iThreshold<=df.prodige[df.prodige$bras==1,"OS"])}),
                              bras = "FOLFIRINOX",
                              time = vec.time)
                   )
df.atRisk$bras <- relevel(df.atRisk$bras, "Gemcitabine")
par(mar = c(4,12,2,2), mgp = c(10, 1, 0))
plot(x = rep(df.atRisk$time,4), y = rep(c(0.5,1,2,2.5),NROW(df.atRisk)), type = "n", ylab = "Treatment group",
     cex.lab = 1.5, xlim = range(vec.time), ylim = c(0,2.5), axes = FALSE)
par(mar = c(4,12,2,2), mgp = c(3, 1, 0), adj = 0)
title(main = "Number at risk", cex.main = 1.5)
par(mar = c(4,12,2,2), mgp = c(3, 1, 0), adj = 0.5)
title(xlab = "Time (months)", cex.lab = 1.5)
box(bty = "l")
axis(1, at = vec.time, cex.axis = 1.5)
axis(2, at = c(1:2), label = unique(df.atRisk$bras), cex.axis = 1.5, las = 2)
text(x = df.atRisk$time, y = df.atRisk$bras, label = df.atRisk$value, cex = 2, bty = "l")

## * Analysis
BuyseTest.options(n.resampling = 1e4, 
                  method.inference = "bootstrap",
                  cpus = 3,
                  trace = 1)

ff <- bras~tte(OS, status=etat, threshold=6)

BuyseGehan <- BuyseTest(ff, data = df.prodige,
                        scoring.rule ="Gehan",
                        seed = 10,
                        correction.uninf = FALSE)

BuyseGehan_corr <- BuyseTest(ff, data = df.prodige,
                             scoring.rule ="Gehan",
                             seed = 10,
                             correction.uninf = TRUE)

BuysePeron <- BuyseTest(ff, data = df.prodige,
                        scoring.rule ="Peron",
                        seed = 10,
                        correction.uninf = FALSE)

BuysePeron_corr <- BuyseTest(ff, data = df.prodige,
                             scoring.rule ="Peron",
                             seed = 10,
                             correction.uninf = TRUE)


## ** Figure A
plot(prodlim(Hist(OS, etat)~bras, data = df.prodige))

## Gehan's scoring rule
summary(BuyseGehan)
 ## endpoint threshold total(%) favorable(%) unfavorable(%) neutral(%) uninf(%)  delta  Delta CI [2.5% ; 97.5%]    p.value    
 ##       OS         6      100        28.19          10.66      33.01    28.15 0.1753 0.1753   [0.0991;0.2519] < 2.22e-16 ***

## corrected Gehan's scoring rule
summary(BuyseGehan_corr)
## endpoint threshold total(%) favorable(%) unfavorable(%) neutral(%) uninf(%) delta Delta CI [2.5% ; 97.5%]    p.value    
##        OS         6      100        39.23          14.83      45.94        0 0.244 0.244   [0.1388;0.3456] < 2.22e-16 ***

## Peron's scoring rule
summary(BuysePeron)
 ## endpoint threshold total(%) favorable(%) unfavorable(%) neutral(%) uninf(%)  delta  Delta CI [2.5% ; 97.5%]    p.value    
 ##       OS         6      100         40.2          14.64      45.06      0.1 0.2556 0.2556    [0.156;0.3548] < 2.22e-16 ***

## corrected Peron's scoring rule
summary(BuysePeron_corr)
 ## endpoint threshold total(%) favorable(%) unfavorable(%) neutral(%) uninf(%)  delta  Delta CI [2.5% ; 97.5%]    p.value    
 ##       OS         6      100        40.24          14.65       45.1        0 0.2559 0.2559   [0.1542;0.3557] < 2.22e-16 ***


## * data management (original dataset)
if(FALSE){
    df.prodige = read.csv(file.path("source","data_PRODIGE.csv"),sep=";" ,header = TRUE)

    ## ** process
    df.prodige$d_dn2 <- as.Date(df.prodige$d_dn, "%d/%m/%Y")
    df.prodige$randodt2 <- as.Date(df.prodige$randodt, "%d/%m/%Y")
    df.prodige$bras <- ifelse(df.prodige$bras==2,0,1)
    df.prodige$OS <- as.numeric(difftime(df.prodige$d_dn2,df.prodige$randodt2,units="days")/30.44)

    ## ** export
    write.csv(df.prodige[,c("OS","etat","bras")],file.path(path.data,"data_PRODIGE.csv"))
}
