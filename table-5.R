## This file generates table 5 of the article

## * Load R packages
library(BuyseTest)
if(packageVersion("BuyseTest") < "2.2.0"){
    warning("This script has been check for BuyseTest version 2.2.0 (and above). \n",
            "Consider using devtools::install_github(\"bozenne/BuyseTest\") to get the latest stable version of the BuyseTest package. \n")
}

## * Load data
BT.EORTC <- readRDS("Results/BT-EORTC.rds")

## * table 5
keep.cols <- c("favorable","unfavorable","neutral","uninf","Delta","CIinf.Delta","CIsup.Delta","p.value")
table5 <- do.call(rbind,lapply(BT.EORTC, function(x){summary(x,print=FALSE)$table[1,keep.cols]}))
table5[,"Delta"] <- 100*table5[,"Delta"]
table5[,"CIinf.Delta"] <- 100*table5[,"CIinf.Delta"]
table5[,"CIsup.Delta"] <- 100*table5[,"CIsup.Delta"]
table5[,setdiff(keep.cols, "p.value")] <- round(table5[,setdiff(keep.cols, "p.value")],1)
table5[,"p.value"] <- format.pval(table5[,"p.value"], digit = 2)
table5[,"CIinf.Delta"] <- paste0(table5[,"CIinf.Delta"]," to ",table5[,"CIsup.Delta"])
table5[,"CIsup.Delta"] <- NULL
rownames(table5) <-  c("Gehan's scoring rule","Gehan's scoring rule with correction","Peron's scoring rule", "Peron's scoring rule with correction")
colnames(table5) <- c("Wins","Losses","Neutral","Uninformative","Estimate","95%CI","P")
table5
##                                      Wins Losses Neutral Uninformative Estimate        95%CI      P
## Gehan's scoring rule                 12.8    9.5     2.9          74.8      3.3    -0.4 to 7 0.0812
## Gehan's scoring rule with correction 50.7   37.8    11.5           0.0     13.0 -1.3 to 27.4 0.0723
## Peron's scoring rule                 25.9   16.9     4.8          52.4      9.1  2.7 to 15.7 0.0054
## Peron's scoring rule with correction 54.4   35.4    10.2           0.0     19.0  5.6 to 31.9 0.0048
