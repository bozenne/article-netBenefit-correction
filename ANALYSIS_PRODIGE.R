## This file was used to generate the results of the section "The Prodige 4 study: metastatic pancreatic cancer randomized trial"
save <- FALSE

## * Load R packages
library(BuyseTest)
if(packageVersion("BuyseTest") < "2.2.0"){
    warning("This script has been check for BuyseTest version 2.2.0 (and above). \n",
            "Consider using devtools::install_github(\"bozenne/BuyseTest\") to get the latest stable version of the BuyseTest package. \n")
}
library(survival)
library(prodlim)

## * Load data
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

## ** hazard ratio
summary(coxph(Surv(OS, etat)~bras, data = df.prodige))
##         coef exp(coef) se(coef)      z Pr(>|z|)    
## bras -0.5667    0.5674   0.1229 -4.612 3.99e-06 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##      exp(coef) exp(-coef) lower .95 upper .95
## bras    0.5674      1.762     0.446    0.7219


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
ff <- bras~tte(OS, status=etat, threshold=6)

if(save){
    ## Gehan scoring rule without correction
    BuyseGehan <- BuyseTest(ff,
                            data = df.prodige, 
                            scoring.rule ="Gehan",
                            seed = 10,
                            correction.uninf = FALSE)

    ## Gehan scoring rule with correction at the pair level
    BuyseGehan_corr <- BuyseTest(ff, data = df.prodige,
                                 scoring.rule ="Gehan",
                                 seed = 10,
                                 correction.uninf = TRUE)

    ## Peron scoring rule without correction
    BuysePeron <- BuyseTest(ff, data = df.prodige,
                            scoring.rule = "Peron",
                            seed = 10,
                            correction.uninf = FALSE)

    ## Peron scoring rule with correction at the pair level
    BuysePeron_corr <- BuyseTest(ff, data = df.prodige,
                                 scoring.rule ="Peron",
                                 seed = 10,
                                 correction.uninf = TRUE)

    saveRDS(list(BuyseGehan = BuyseGehan,
                 BuyseGehan_corr = BuyseGehan_corr ,
                 BuysePeron = BuysePeron,
                 BuysePeron_corr = BuysePeron_corr),
            file = "Results/BT-PRODIGE.rds")
}else{
    BT.PRODIGE <- readRDS("Results/BT-PRODIGE.rds")
    BuyseGehan <- BT.PRODIGE$BuyseGehan
    BuyseGehan_corr <- BT.PRODIGE$BuyseGehan_corr
    BuysePeron <- BT.PRODIGE$BuysePeron
    BuysePeron_corr <- BT.PRODIGE$BuysePeron_corr
}
## ** Results
## note: messages are expected when calling summary
## the p-value is very small and would require a lot of samples to be precisely estimated
M <- rbind(c("scoring.rule" = "Gehan", correction = FALSE, suppressMessages(summary(BuyseGehan, print = FALSE)$table.print[,3:10])),
           c("scoring.rule" = "Gehan", correction = TRUE, suppressMessages(summary(BuyseGehan_corr, print = FALSE)$table.print[,3:10])),
           c("scoring.rule" = "Peron", correction = FALSE, suppressMessages(summary(BuysePeron, print = FALSE)$table.print[,3:10])),
           c("scoring.rule" = "Peron", correction = FALSE, suppressMessages(summary(BuysePeron_corr, print = FALSE)$table.print[,3:10])))
print(M, quote = FALSE)
##      scoring.rule correction total(%) favorable(%) unfavorable(%) neutral(%) uninf(%) Delta  CI [2.5% ; 97.5%] p.value     
## [1,] "Gehan"      FALSE      100      28.19        10.66          33.01      28.15    0.1753 "[0.0987;0.2516]" "1e-04"     
## [2,] "Gehan"      TRUE       100      39.23        14.83          45.94      0        0.244  "[0.1392;0.3473]" "< 2.22e-16"
## [3,] "Peron"      FALSE      100      40.2         14.64          45.06      0.1      0.2556 "[0.154;0.354]"   "< 2.22e-16"
## [4,] "Peron"      FALSE      100      40.24        14.65          45.1       0        0.2559 "[0.1537;0.3566]" "< 2.22e-16"


## * [not used] data management (original dataset)
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
