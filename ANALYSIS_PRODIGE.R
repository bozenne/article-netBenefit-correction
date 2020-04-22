path <- "~/Documents/GitHub/article-netBenefit-correction/"
setwd(path)
options(width = 130)

path.data <- "data"

## * Package
library(BuyseTest) ## butils.base:::sourcePackage("BuyseTest", c.code = TRUE, trace = TRUE)
library(survival)
library(prodlim)

## * data management
## ** load
df.prodige = read.csv(file.path(path.data,"data_PRODIGE.csv"),sep=";" ,header = TRUE)

## ** process
df.prodige$d_dn2 <- as.Date(df.prodige$d_dn, "%d/%m/%Y")
df.prodige$randodt2 <- as.Date(df.prodige$randodt, "%d/%m/%Y")
df.prodige$bras <- ifelse(df.prodige$bras==2,0,1)
df.prodige$OS <- as.numeric(difftime(df.prodige$d_dn2,df.prodige$randodt2,units="days")/30.44)

## * analysis
BuyseTest.options(n.resampling = 1e2, 
                  method.inference = "permutation",
                  cpus = 1,
                  trace = 1)

ff <- bras~tte(OS, status=etat, threshold=6)

BuyseGehan <- BuyseTest(ff, data = df.prodige,
                        scoring.rule ="Gehan",
                        seed = 10,
                        correction.uninf = FALSE)

## BuyseGehan_corr <- BuyseTest(ff, data = df.prodige,
##                              scoring.rule ="Gehan",
##                              seed = 10,
##                              correction.uninf = TRUE)

BuysePeron <- BuyseTest(ff, data = df.prodige,
                        scoring.rule ="Peron",
                        seed = 10,
                        correction.uninf = FALSE)

BuysePeron_corr <- BuyseTest(ff, data = df.prodige,
                             scoring.rule ="Peron",
                             seed = 10,
                             correction.uninf = TRUE)

## * Results
## ** sample size
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
mean(df.prodige$etat==0)
## [1] 0.2017544

## ** HR
summary(coxph(Surv(OS, etat)~bras, data = df.prodige))

##   n= 342, number of events= 273 

##         coef exp(coef) se(coef)      z Pr(>|z|)    
## bras -0.5667    0.5674   0.1229 -4.612 3.99e-06 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##      exp(coef) exp(-coef) lower .95 upper .95
## bras    0.5674      1.762     0.446    0.7219

## ** Figure A
plot(prodlim(Hist(OS, etat)~bras, data = df.prodige))

## Gehan's scoring rule
summary(BuyseGehan)
 ## endpoint threshold total(%) favorable(%) unfavorable(%) neutral(%) uninf(%)  delta  Delta    p.value    
 ##       OS         6      100        28.19          10.66      33.01    28.15 0.1753 0.1753 < 2.22e-16 ***

## Peron's scoring rule
summary(BuysePeron)
 ## endpoint threshold total(%) favorable(%) unfavorable(%) neutral(%) uninf(%)  delta  Delta    p.value    
 ##       OS         6      100         40.2          14.64      45.06      0.1 0.2556 0.2556 < 2.22e-16 ***

## corrected Peron's scoring rule
summary(BuysePeron_corr)
 ## endpoint threshold total(%) favorable(%) unfavorable(%) neutral(%) uninf(%)  delta  Delta    p.value    
 ##       OS         6      100        40.24          14.65       45.1        0 0.2559 0.2559 < 2.22e-16 ***


