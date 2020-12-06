path <- "~/Documents/GitHub/article-netBenefit-correction/"
setwd(path)
options(width = 130)

path.data <- "data"

## * Package
library(BuyseTest) ## butils.base:::sourcePackage("BuyseTest", c.code = TRUE, trace = TRUE)
library(survival)
library(prodlim)

## * data management
df.eortc <- read.csv(file.path(path.data,"data_EORTC-22961.csv"))
df.eortc[df.eortc==0,"dsurvyears"] <- 1e-5
df.eortc$trt2 <- relevel(df.eortc$trt2, "Short ADT")

## * analysis
BuyseTest.options(n.resampling = 1e2,##1e4,
                  method.inference = "bootstrap",
                  cpus = 3,
                  trace = 1)

ff <- trt2~tte(dsurvyears, status=DC, threshold=2)

BuyseGehan <- BuyseTest(ff, data = df.eortc,
                        scoring.rule ="Gehan",
                        seed = 10,
                        correction.uninf = FALSE)

BuyseGehan_corr <- BuyseTest(ff, data = df.eortc,
                             scoring.rule ="Gehan",
                             seed = 10,
                             correction.uninf = TRUE)

BuysePeron <- BuyseTest(ff, data = df.eortc,
                        scoring.rule ="Peron",
                        seed = 10,
                        correction.uninf = FALSE)

BuysePeron_corr <- BuyseTest(ff, data = df.eortc,
                             scoring.rule ="Peron",
                             seed = 10,
                             correction.uninf = TRUE)

## * Results
## ** sample size
NROW(df.eortc)
## [1] 970

table(df.eortc$trt2)
## Short ADT  Long ADT 
##       483       487 

## ** follow-up
median(df.eortc$dsurvyears)
## [1] 5.990411
tapply(df.eortc$dsurvyears,df.eortc$trt2,median)
## Short ADT  Long ADT 
##  5.936986  6.021918 

survfit(Surv(dsurvyears, DC) ~ trt2, data = df.eortc) 
##                  n events median 0.95LCL 0.95UCL
## trt2=Long ADT  487     98     NA      NA      NA
## trt2=Short ADT 483    132   8.98    8.33      NA

## ** death
tapply(df.eortc$DC==1,df.eortc$trt2,sum)
## Short ADT  Long ADT 
##       132        98 

## ** censoring
tapply(df.eortc$DC==0,df.eortc$trt2,sum)
## Short ADT  Long ADT 
##       351       389 
tapply(df.eortc$DC==0,df.eortc$trt2,mean)
## Short ADT  Long ADT 
## 0.7267081 0.7987680 
mean(df.eortc$DC==0)
## [1] 0.7628866

## ** HR
df.eortc$trt2 <- relevel(df.eortc$trt2,"Long ADT")
summary(coxph(Surv(dsurvyears, DC)~trt2, data = df.eortc))

##   n= 970, number of events= 230 

##                 coef exp(coef) se(coef)     z Pr(>|z|)   
## trt2Short ADT 0.3547    1.4258   0.1334 2.659  0.00784 **
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##               exp(coef) exp(-coef) lower .95 upper .95
## trt2Short ADT     1.426     0.7014     1.098     1.852

## ** Figure B
plot(prodlim(Hist(dsurvyears, DC)~trt2, data = df.eortc))

## Gehan's scoring rule
summary(BuyseGehan)
 ## endpoint threshold total(%) favorable(%) unfavorable(%) neutral(%) uninf(%)  delta  Delta CI [2.5% ; 97.5%] p.value  
 ## dsurvyears         2      100        12.77           9.51       2.89    74.83 0.0326 0.0326  [-0.0038;0.0699]  0.0828 .

## corrected Gehan's scoring rule
summary(BuyseGehan_corr)
 ##   endpoint threshold total(%) favorable(%) unfavorable(%) neutral(%) uninf(%)  delta  Delta CI [2.5% ; 97.5%] p.value  
 ## dsurvyears         2      100        50.74          37.79      11.47        0 0.1295 0.1295  [-0.0177;0.2807]  0.0856 .

## Peron's scoring rule
summary(BuysePeron)
 ##   endpoint threshold total(%) favorable(%) unfavorable(%) neutral(%) uninf(%)  delta  Delta CI [2.5% ; 97.5%] p.value   
 ## dsurvyears         2      100        25.91          16.85       4.83     52.4 0.0906 0.0906   [0.0277;0.1548]   0.004 **

## corrected Peron's scoring rule
summary(BuysePeron_corr)
 ##   endpoint threshold total(%) favorable(%) unfavorable(%) neutral(%) uninf(%)  delta  Delta CI [2.5% ; 97.5%] p.value   
 ## dsurvyears         2      100        54.44          35.41      10.15        0 0.1903 0.1903    [0.0592;0.318]  0.0042 **

## * old (not used)
if(FALSE){ 
    packageurl <- "https://cran.r-project.org/src/contrib/Archive/BuyseTest/BuyseTest_1.7.tar.gz"
    install.packages(packageurl, repos=NULL, type="source")
    ## devtools::install_github("bozenne/BuyseTest", ref = "1bf7a7e5143560fbbc208611ed52525930e3f5bb") ## same as now ((1.6.1) update vignette with formula Peron inequalities)

    devtools::install_github("bozenne/BuyseTest", ref = "6e6c7b0f3fb69e4409ab6fd91554b5da666904e2") ## different as now ( (1.6.2) update tests )

    ## devtools::install_github("bozenne/BuyseTest", ref = "575565fe6f4ed6c82c50bb6eea8f7a4cf03878e2") ## different as now ( (1.6.1) add R_CheckUserInterrupt() when looping over the pairs  )

    
    ff <- trt2~tte(dsurvyears, censoring=DC, threshold=2)

    BuyseGehan <- BuyseTest(ff, data=df.eortc,
                            method.tte ="Gehan",
                            seed = 10,
                            correction.uninf = FALSE)

    BuysePeron <- BuyseTest(trt2~tte(dsurvyears, censoring=DC, threshold=2),
                            data=df.eortc,
                            method.tte ="Peron",
                            seed = 10,
                            method.inference = "none")
    summary(BuysePeron)

    BuysePeron <- BuyseTest(trt2~tte(dsurvyears, status=DC, threshold=1),
                            data=df.eortc,
                            scoring.rule="Peron",
                            seed = 10,
                            method.inference = "none")
    summary(BuysePeron)

    BuysePeron_corr <- BuyseTest(ff, data=df.eortc,
                                 method.tte ="Peron",
                                 seed = 10,
                                 n.resampling = 100,
                                 correction.uninf = TRUE)

    summary(BuysePeron_corr)
}


## * data management (original dataset)
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

