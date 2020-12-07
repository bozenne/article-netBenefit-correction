## * Load packages
library(lava)
library(data.table)
library(BuyseTest)
library(ggplot2)
library(pbapply)
library(parallel)

## * Simulation function
m <- lvm()
regression(m) <- Treatment ~ 1
distribution(m,~Treatment) <- binomial.lvm(p=0.5)
regression(m) <- t1 ~ x1+Treatment+rho*eta
regression(m) <- t2 ~ x2+Treatment+rho*eta
distribution(m,~t1) <- coxWeibull.lvm(scale=1)
distribution(m,~t2) <- coxWeibull.lvm(scale=1)
distribution(m,~cens1) <- coxWeibull.lvm(scale=0.25)
distribution(m,~cens2) <- coxWeibull.lvm(scale=0.25)
eventTime(m) <- time1 ~ min(t1=1,cens1=0)
eventTime(m) <- time2 ~ min(t2=1,cens2=0)

warper <- function(i, n, rho, m, model.tte = NULL){
    require(BuyseTest)
    require(lava)
    
    d <- data.table::as.data.table(lava::sim(m, n = n, p = c(rho = rho)))
    d[,Id := 1:.N]
    d[,status1 := as.numeric(t1 <= cens1)]
    d[,status2 := as.numeric(t2 <= cens2)]

    ## d$t1
    tau1 <- 0.5
    tau2 <- 0.5
    
    ffFull <- eval(parse(text = paste0("Treatment ~ cont(t1, threshold = ",tau1,") + cont(t2, threshold = ",tau2,")")))
    eFull.BT <- BuyseTest(ffFull,
                          data = d, method.inference = "none", trace = 0)

    ffCens <- eval(parse(text = paste0("Treatment ~ tte(time1, threshold = ",tau1,", status = status1) + tte(time2, threshold = ",tau2,", status = status2)")))
    eCens.BT <- BuyseTest(ffCens, model.tte = model.tte,
                          data = d, method.inference = "none", trace = 0)
    eCensCorr.BT <- BuyseTest(ffCens, model.tte = model.tte,
                              data = d, method.inference = "none", trace = 0, correction.uninf = 2)

    allPairs <- expand.grid(index.x = d[Treatment == 1,Id],
                            index.y = d[Treatment == 0,Id])
    allPairs$tte1.x <- d[allPairs$index.x,t1]
    allPairs$tte1.y <- d[allPairs$index.y,t1]
    allPairs$tte2.x <- d[allPairs$index.x,t2]
    allPairs$tte2.y <- d[allPairs$index.y,t2]

    A <- (allPairs$tte2.x >= allPairs$tte2.y + tau2) - (allPairs$tte2.y >= allPairs$tte2.x + tau2)
    B <- abs(allPairs$tte1.x - allPairs$tte1.y) < tau1

    return(list(covTime = cor(d$t1,d$t2),
                corPair = cor(A,B),
                covPair = cov(A,B),
                full = eFull.BT,
                censored = eCens.BT,
                censoredCorr = eCensCorr.BT,
                n = n,
                rho = rho))
}
## * Run simulation
n.sim <- 100
cl <- makeCluster(3)

set.seed(10)
ls.res <- c(pblapply(1:n.sim, warper, n = 500, m = m, rho=0, cl = cl),
            pblapply(1:n.sim, warper, n = 500, m = m, rho=1, cl = cl),
            pblapply(1:n.sim, warper, n = 500, m = m, rho=2, cl = cl),
            pblapply(1:n.sim, warper, n = 500, m = m, rho=4, cl = cl),
            pblapply(1:n.sim, warper, n = 500, m = m, rho=6, cl = cl))

## * Process results
#+BEGIN_SRC R :exports both :results output :session *R* :cache no
M <- do.call(rbind,lapply(ls.res, function(iList){ ## iList <- ls.res[[1]]
    dt1.full <- data.table(type = "full", endpoint = 1, rho = iList$rho, n = iList$n, 
                           covTime = iList$covTime, covPair = iList$covPair,
                           favorable = iList$full@count.favorable[1]/iList$full@n.pairs[1], 
                           unfavorable = iList$full@count.unfavorable[1]/iList$full@n.pairs[1],
                           delta = iList$full@delta[,1,"netBenefit"])
    dt2.full <- data.table(type = "full", endpoint = 2, rho = iList$rho, n = iList$n,
                           covTime = iList$covTime, covPair = iList$covPair,
                           favorable = iList$full@count.favorable[2]/iList$full@n.pairs[1], 
                           unfavorable = iList$full@count.unfavorable[2]/iList$full@n.pairs[1],
                           delta = iList$full@delta[,2,"netBenefit"])

    dt1.censored <- data.table(type = "censored", endpoint = 1, rho = iList$rho, n = iList$n,
                               covTime = iList$covTime, covPair = iList$covPair,
                               favorable = iList$censored@count.favorable[1]/iList$censored@n.pairs[1], 
                               unfavorable = iList$censored@count.unfavorable[1]/iList$censored@n.pairs[1],
                               delta = iList$censored@delta[,1,"netBenefit"])
    dt2.censored <- data.table(type = "censored", endpoint = 2, rho = iList$rho, n = iList$n,
                               covTime = iList$covTime, covPair = iList$covPair,
                               favorable = iList$censored@count.favorable[2]/iList$censored@n.pairs[1], 
                               unfavorable = iList$censored@count.unfavorable[2]/iList$censored@n.pairs[1],
                               delta = iList$censored@delta[,2,"netBenefit"])

    dt1.censoredCorr <- data.table(type = "censoredCorr", endpoint = 1, rho = iList$rho, n = iList$n,
                                   covTime = iList$covTime, covPair = iList$covPair,
                                   favorable = iList$censoredCorr@count.favorable[1]/iList$censoredCorr@n.pairs[1], 
                                   unfavorable = iList$censoredCorr@count.unfavorable[1]/iList$censoredCorr@n.pairs[1],
                                   delta = iList$censoredCorr@delta[,1,"netBenefit"])
    dt2.censoredCorr <- data.table(type = "censoredCorr", endpoint = 2, rho = iList$rho, n = iList$n,
                                   covTime = iList$covTime, covPair = iList$covPair,
                                   favorable = iList$censoredCorr@count.favorable[2]/iList$censoredCorr@n.pairs[1], 
                                   unfavorable = iList$censoredCorr@count.unfavorable[2]/iList$censoredCorr@n.pairs[1],
                                   delta = iList$censoredCorr@delta[,2,"netBenefit"])

    return(rbind(dt1.full,dt2.full,dt1.censored,dt2.censored,dt1.censoredCorr,dt2.censoredCorr))
}))


## * Display graph
tableS <- M[,.(rep = .N, covTime = mean(covTime), covPair = mean(covPair) ,favorable = mean(favorable), unfavorable = mean(unfavorable), delta = mean(delta)),by = c("endpoint","type","rho")]
vec.rho <- round(tableS[,.SD[1,covTime],by = "rho"][[2]],2)

dt.gg <- melt(M, measure = c("favorable","unfavorable","delta","covPair"), id.vars = c("endpoint","type","rho"))
dt.gg[(type != "full" | endpoint == 1) & variable == "covPair", value := NA]
dt.gg[, rho := factor(rho, levels = unique(rho), labels = paste0(unique(rho),"\n (",vec.rho,")"))]
dt.gg[, estimator := factor(type, c("full","censored","censoredCorr"), labels = c("complete data","Peron scoring rule","Peron scoring rule + correction at the pair level"))]
dt.gg[, endpoint := factor(endpoint, levels = 1:2, labels = c("1st endpoint","2nd endpoint"))]
## dt.gg[, variable := factor(variable, levels = c("favorable","unfavorable","delta"), labels = c(bquote(P[X >= Y+tau]),bquote(P[Y <= X+tau]),bquote(P[X >= Y+tau]-P[Y <= X+tau])))]
dt.gg[, variable := factor(variable,
                           levels = c("favorable","unfavorable","delta","covPair"),
                           labels = c(bquote(P[X >= Y+tau]),bquote(P[Y <= X+tau]),bquote(P[X >= Y+tau]-P[Y <= X+tau]),bquote(Cov(.,.))))]
gg <- ggplot(dt.gg, aes(x = rho, y = value, color = estimator))
gg <- gg + geom_hline(yintercept = 0)
gg <- gg + geom_boxplot()
gg <- gg + facet_grid(endpoint ~ variable, scales = "free_y")
gg <- gg + theme(legend.position="bottom")
gg <- gg + xlab("correlation coefficient (average empirical correlation between time 1 and time 2)") + ylab("estimate")
gg

## ggsave(gg, filename = file.path("figures-review","fig-SM-A.png"))
