## This file generates table 3 of the article

## * Load R packages
library(data.table)

## * Load results
dtS.scenario1 <- readRDS("Results/dtS-scenario1.rds")
dtS.scenario1 <- dtS.scenario1[n == 200 & HR == 0.5 & scoring.rule != "GS" & hazard.censoring %in% c(1.0e-07, 1.7e-03, 7.0e-03, 1.7e-02, 6.8e-02)]

dtS.scenario2 <- readRDS("Results/dtS-scenario2.rds")
dtS.scenario2 <- dtS.scenario2[n == 200 & HR == 0.5 & scoring.rule != "GS" & hazard.censoring %in% c(1.0e-07, 1.7e-03, 7.0e-03, 1.7e-02, 6.8e-02)]

dtS.scenario3 <- readRDS("Results/dtS-scenario3.rds")
dtS.scenario3 <- dtS.scenario3[n == 200 & scoring.rule != "GS" & hazard.censoring %in% c(1.0e-07, 1.7e-03, 7.0e-03, 1.7e-02, 6.8e-02)]

dtS.scenario4 <- readRDS("Results/dtS-scenario4.rds")
dtS.scenario4 <- dtS.scenario4[n == 200 & scoring.rule != "GS" & hazard.censoring %in% c(1.0e-07, 1.7e-03, 7.0e-03, 1.7e-02, 6.8e-02)]

dtS.scenario5 <- readRDS("Results/dtS-scenario5.rds")
dtS.scenario5 <- dtS.scenario5[index.endpoint == 2 & n == 200 & scoring.rule != "GS" & hazard.censoring %in% c(1.0e-07, 1.7e-03, 7.0e-03, 1.7e-02, 6.8e-02)]

## * table 3
## ** scenario 1
dtS.scenario1$estimate.mean <- round(100*dtS.scenario1$estimate.mean,1)
dtS.scenario1$estimate.sd <- round(100*dtS.scenario1$estimate.sd,1)
dtS.scenario1[, censoring := paste0(100*round(pc.censoring,1),"%"), by = hazard.censoring]
Ucensoring <- unique(dtS.scenario1$censoring)
dtS.scenario1[, scoring.rule := factor(scoring.rule,
                                       levels = c("Peron.C","Peron","Gehan.C","Gehan"))]
table3.1 <- dcast(dtS.scenario1, value.var = c("estimate.mean","estimate.sd"), formula = scoring.rule ~ censoring)
names(table3.1) <- gsub("estimate\\.","",names(table3.1))
table3.1 <- table3.1[,.SD, .SDcols = c("scoring.rule",unlist(lapply(Ucensoring, function(x){paste0(c("mean_","sd_"),x)})))]

## ** scenario 2
dtS.scenario2$estimate.mean <- round(100*dtS.scenario2$estimate.mean,1)
dtS.scenario2$estimate.sd <- round(100*dtS.scenario2$estimate.sd,1)
dtS.scenario2[, censoring := paste0(100*round(pc.censoring,1),"%"), by = hazard.censoring]
Ucensoring <- unique(dtS.scenario2$censoring)
dtS.scenario2[, scoring.rule := factor(scoring.rule,
                                       levels = c("Peron.C","Peron","Gehan.C","Gehan"))]
table3.2 <- dcast(dtS.scenario2, value.var = c("estimate.mean","estimate.sd"), formula = scoring.rule ~ censoring)
names(table3.2) <- gsub("estimate\\.","",names(table3.2))
table3.2 <- table3.2[,.SD, .SDcols = c("scoring.rule",unlist(lapply(Ucensoring, function(x){paste0(c("mean_","sd_"),x)})))]

## ** scenario 3
dtS.scenario3$estimate.mean <- round(100*dtS.scenario3$estimate.mean,1)
dtS.scenario3$estimate.sd <- round(100*dtS.scenario3$estimate.sd,1)
dtS.scenario3[, censoring := paste0(100*round(pc.censoring,1),"%"), by = hazard.censoring]
Ucensoring <- unique(dtS.scenario3$censoring)
dtS.scenario3[, scoring.rule := factor(scoring.rule,
                                       levels = c("Peron.C","Peron","Gehan.C","Gehan"))]
table3.3 <- dcast(dtS.scenario3, value.var = c("estimate.mean","estimate.sd"), formula = scoring.rule ~ censoring)
names(table3.3) <- gsub("estimate\\.","",names(table3.3))
table3.3 <- table3.3[,.SD, .SDcols = c("scoring.rule",unlist(lapply(Ucensoring, function(x){paste0(c("mean_","sd_"),x)})))]

## ** scenario 4
dtS.scenario4$estimate.mean <- round(100*dtS.scenario4$estimate.mean,1)
dtS.scenario4$estimate.sd <- round(100*dtS.scenario4$estimate.sd,1)
dtS.scenario4[, censoring := paste0(100*round(pc.censoring,1),"%"), by = hazard.censoring]
Ucensoring <- unique(dtS.scenario4$censoring)
dtS.scenario4[, scoring.rule := factor(scoring.rule,
                                       levels = c("Peron.C","Peron","Gehan.C","Gehan"))]
table3.4 <- dcast(dtS.scenario4, value.var = c("estimate.mean","estimate.sd"), formula = scoring.rule ~ censoring)
names(table3.4) <- gsub("estimate\\.","",names(table3.4))
table3.4 <- table3.4[,.SD, .SDcols = c("scoring.rule",unlist(lapply(Ucensoring, function(x){paste0(c("mean_","sd_"),x)})))]

## ** scenario 5
dtS.scenario5$estimate.mean <- round(100*dtS.scenario5$estimate.mean,1)
dtS.scenario5$estimate.sd <- round(100*dtS.scenario5$estimate.sd,1)
dtS.scenario5[, censoring := paste0(100*round(pc.censoring,1),"%"), by = hazard.censoring]
Ucensoring <- unique(dtS.scenario5$censoring)
dtS.scenario5[, scoring.rule := factor(scoring.rule,
                                       levels = c("Peron.C","Peron","Gehan.C","Gehan"))]
table3.5 <- dcast(dtS.scenario5, value.var = c("estimate.mean","estimate.sd"), formula = scoring.rule ~ censoring)
names(table3.5) <- gsub("estimate\\.","",names(table3.5))
table3.5 <- table3.5[,.SD, .SDcols = c("scoring.rule",unlist(lapply(Ucensoring, function(x){paste0(c("mean_","sd_"),x)})))]

## ** assemble
cat("Scenario 1\n")
print(table3.1)
cat("Scenario 2\n")
print(table3.2)
cat("Scenario 3\n")
print(table3.3)
cat("Scenario 4\n")
print(table3.4)
cat("Scenario 5\n")
print(table3.5)

## Scenario 1
##    scoring.rule mean_0% sd_0% mean_20% sd_20% mean_50% sd_50% mean_70% sd_70% mean_90% sd_90%
## 1:      Peron.C    33.3   5.4     33.4    5.6     33.4    6.5     33.2   10.0     33.4   26.4
## 2:        Peron    33.3   5.4     33.4    5.6     33.3    6.5     31.9    9.8     21.8   20.8
## 3:      Gehan.C    33.3   5.4     33.4    6.0     33.4    7.5     33.3    9.7     33.3   17.3
## 4:        Gehan    33.3   5.4     27.2    4.9     17.3    4.0     10.2    3.1      3.3    1.8
## Scenario 2
##    scoring.rule mean_0% sd_0% mean_20% sd_20% mean_50% sd_50% mean_70% sd_70% mean_90% sd_90%
## 1:      Peron.C    31.7   4.9     31.8    5.2     32.0    6.3     33.2   10.2     34.0   30.1
## 2:        Peron    31.7   4.9     31.8    5.2     31.7    6.2     30.3    9.6     12.1   12.8
## 3:      Gehan.C    31.7   4.9     31.3    5.5     30.1    6.7     27.1    8.2      9.8    9.6
## 4:        Gehan    31.7   4.9     23.8    4.2     11.6    2.7      4.1    1.3      0.1    0.1
## Scenario 3
##    scoring.rule mean_0% sd_0% mean_20% sd_20% mean_50% sd_50% mean_70% sd_70% mean_90% sd_90%
## 1:      Peron.C    30.8   5.4     30.8    5.7     31.0    6.7     31.8   10.3     46.4   29.1
## 2:        Peron    30.8   5.4     30.8    5.7     30.9    6.7     30.9   10.0     28.8   23.0
## 3:      Gehan.C    30.8   5.4     34.2    5.9     42.2    7.1     49.3    9.2     53.7   16.8
## 4:        Gehan    30.8   5.4     27.9    4.9     21.1    3.8     14.0    3.0      4.7    1.7
## Scenario 4
##    scoring.rule mean_0% sd_0% mean_20% sd_20% mean_50% sd_50% mean_60% sd_60% mean_90% sd_90%
## 1:      Peron.C    14.3   5.8     14.1    5.9     14.2    6.7     12.7    9.3      3.5   23.9
## 2:        Peron    14.3   5.8     14.1    5.9     14.1    6.7     12.3    9.0      2.8   19.2
## 3:      Gehan.C    14.3   5.8     10.8    6.3      5.4    7.6      1.7    9.6      0.1   16.4
## 4:        Gehan    14.3   5.8      9.1    5.3      3.1    4.4      0.6    3.5      0.0    2.1
## Scenario 5
##    scoring.rule mean_0% sd_0% mean_20% sd_20% mean_50% sd_50% mean_70% sd_70% mean_90% sd_90%
## 1:      Peron.C    40.1   4.9     39.9    5.1     40.2    6.2     40.9    9.8     41.4   28.8
## 2:        Peron    40.1   4.9     39.9    5.1     40.1    6.2     40.0    9.1     34.7   11.5
## 3:      Gehan.C    40.1   4.9     39.7    5.3     39.5    6.6     38.5    7.9     33.2    9.0
## 4:        Gehan    40.1   4.9     37.4    4.5     33.7    4.2     31.3    4.2     30.0    4.5
