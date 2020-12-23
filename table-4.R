## This file generates table 4 of the article

## * Load R packages
library(BuyseTest)
if(packageVersion("BuyseTest") < "2.2.0"){
    warning("This script has been check for BuyseTest version 2.2.0 (and above). \n",
            "Consider using devtools::install_github(\"bozenne/BuyseTest\") to get the latest stable version of the BuyseTest package. \n")
}

## * Load data
BT.PRODIGE <- readRDS("Results/BT-PRODIGE.rds")

## * table 4
keep.cols <- c("favorable","unfavorable","neutral","uninf","Delta","CIinf.Delta","CIsup.Delta","p.value")
table4 <- do.call(rbind,lapply(BT.PRODIGE, function(x){suppressWarnings(summary(x,print=FALSE)$table[1,keep.cols])}))
## note: warnings are expected when calling summary
## the p-value is very small and would require a lot of samples to be precisely estimated
table4[,"Delta"] <- 100*table4[,"Delta"]
table4[,"CIinf.Delta"] <- 100*table4[,"CIinf.Delta"]
table4[,"CIsup.Delta"] <- 100*table4[,"CIsup.Delta"]
table4[,setdiff(keep.cols, "p.value")] <- round(table4[,setdiff(keep.cols, "p.value")],1)
table4[,"p.value"] <- format.pval(table4[,"p.value"], digit = 2)
table4[,"CIinf.Delta"] <- paste0(table4[,"CIinf.Delta"]," to ",table4[,"CIsup.Delta"])
table4[,"CIsup.Delta"] <- NULL
rownames(table4) <-  c("Gehan's scoring rule","Gehan's scoring rule with correction","Peron's scoring rule", "Peron's scoring rule with correction")
colnames(table4) <- c("Wins","Losses","Neutral","Uninformative","Estimate","95%CI","P")
print(table4)
##                                      Wins Losses Neutral Uninformative Estimate        95%CI      P
## Gehan's scoring rule                 28.2   10.7    33.0          28.2     17.5  9.9 to 25.2  1e-04
## Gehan's scoring rule with correction 39.2   14.8    45.9           0.0     24.4 13.9 to 34.7 <2e-16
## Peron's scoring rule                 40.2   14.6    45.1           0.1     25.6 15.4 to 35.4 <2e-16
## Peron's scoring rule with correction 40.2   14.7    45.1           0.0     25.6 15.4 to 35.7 <2e-16
