## This file was used to generate the results of the section "EPRTC 22961 study: advanced prostate cancer randomized trial"
save <- FALSE

## * Package
library(BuyseTest)
if(packageVersion("BuyseTest") < "2.2.0"){
    warning("This script has been check for BuyseTest version 2.2.0 (and above). \n",
            "Consider using devtools::install_github(\"bozenne/BuyseTest\") to get the latest stable version of the BuyseTest package. \n")
}
library(survival)
library(prodlim)

## * Load data
df.eortc <- read.csv("data/data_EORTC-22961.csv")

## the time to event is 0 for some individuals which is a problem for the software
## so they are set to a very small value just after 0
df.eortc[df.eortc$dsurvyears==0,"dsurvyears"] <- 1e-5
df.eortc$trt2 <- relevel(as.factor(df.eortc$trt2), "Short ADT")

## * Descriptive statistics
## ** sample size
NROW(df.eortc)
## [1] 970

table(df.eortc$trt2)
## Short ADT  Long ADT 
##       483       487 

## ** death
sum(df.eortc$DC==1)
## [1] 230
tapply(df.eortc$DC==1,df.eortc$trt2,sum)
## Short ADT  Long ADT 
##       132        98 

## ** censoring
100*mean(df.eortc$DC==0)
## [1] 0.7628866

## ** hazard ratio
df.eortc$trt2 <- relevel(df.eortc$trt2,"Long ADT")
summary(coxph(Surv(dsurvyears, DC)~trt2, data = df.eortc))
##                 coef exp(coef) se(coef)     z Pr(>|z|)   
## trt2Short ADT 0.3547    1.4258   0.1334 2.659  0.00784 **
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##               exp(coef) exp(-coef) lower .95 upper .95
## trt2Short ADT     1.426     0.7014     1.098     1.852


## * GPC analysis
## ** method used to estimate confidence intervals/p-values
## non-parametric bootstrap: slow but accurate version used in the article
BuyseTest.options(n.resampling = 1e4, ## number of bootstrap samples 
                  method.inference = "bootstrap",
                  cpus = 1) ## number of CPU's used for parallel computing)
## fast approximation (wrong p-values/CI in presence of the correction)
## BuyseTest.options(method.inference = "u-statistic")

## ** Perform GPC calculations
## formula:
## - left hand side: group variable
## - right hand side: outcome (name, censoring status, threshold of clinical relevance)
ff <- trt2~tte(dsurvyears, status=DC, threshold=2)
df.eortc$trt2 <- relevel(df.eortc$trt2,"Short ADT")

## Gehan scoring rule without correction
BuyseGehan <- BuyseTest(ff,
                        data = df.eortc,
                        scoring.rule ="Gehan",
                        seed = 10,
                        correction.uninf = FALSE)

## Gehan scoring rule with correction at the pair level
BuyseGehan_corr <- BuyseTest(ff, data = df.eortc,
                             scoring.rule ="Gehan",
                             seed = 10,
                             correction.uninf = TRUE)

## Peron scoring rule without correction
BuysePeron <- BuyseTest(ff, data = df.eortc,
                        scoring.rule ="Peron",
                        seed = 10,
                        correction.uninf = FALSE)

## Peron scoring rule with correction at the pair level
BuysePeron_corr <- BuyseTest(ff, data = df.eortc,
                             scoring.rule ="Peron",
                             seed = 10,
                             correction.uninf = TRUE)

if(save){
    saveRDS(list(BuyseGehan = BuyseGehan,
                 BuyseGehan_corr = BuyseGehan_corr ,
                 BuysePeron = BuysePeron,
                 BuysePeron_corr = BuysePeron_corr),
            file = "Results/BT-EORTC.rds")
}

## ** Results
M <- rbind(c("scoring.rule" = "Gehan", correction = FALSE, summary(BuyseGehan, print = FALSE)$table.print[,3:10]),
           c("scoring.rule" = "Gehan", correction = TRUE, summary(BuyseGehan_corr, print = FALSE)$table.print[,3:10]),
           c("scoring.rule" = "Peron", correction = FALSE, summary(BuysePeron, print = FALSE)$table.print[,3:10]),
           c("scoring.rule" = "Peron", correction = FALSE, summary(BuysePeron_corr, print = FALSE)$table.print[,3:10]))
print(M, quote = FALSE)
##     scoring.rule correction total(%) favorable(%) unfavorable(%) neutral(%) uninf(%) Delta   CI [2.5% ; 97.5%]   p.value 
## [1,] "Gehan"      FALSE      100      9.51         12.77          2.89       74.83    -0.0326 "[-0.0699;0.0038]"  "0.0848"
## [2,] "Gehan"      TRUE       100      37.79        50.74          11.47      0        -0.1295 "[-0.2748;0.0155]"  "0.0848"
## [3,] "Peron"      FALSE      100      16.85        25.91          4.83       52.4     -0.0906 "[-0.1536;-0.0265]" "0.0056"
## [4,] "Peron"      FALSE      100      35.41        54.44          10.15      0        -0.1903 "[-0.3168;-0.0563]" "0.0056"


## * [not used] data management (original dataset)
if(FALSE){
    ## ** load
    df.eortc = read.csv(file.path("source","data_EORTC-22961.csv"),sep=";" ,header = TRUE)

    ## ** process
    df.eortc$dsurvmonths <- df.eortc$dsur/30.4
    df.eortc$dsurvyears <- df.eortc$dsur/365
    df.eortc$DC <- ifelse(df.eortc$ss=="Dead",1,0)

    ## ** export
    write.csv(df.eortc[,c("trt2","dsurvyears","DC")],file.path(path.data,"data_EORTC-22961.csv"))
}

