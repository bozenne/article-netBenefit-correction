## * Analytical estimation of the net benefit
calcNB <- function(lambdaC,
                   lambda1, lambda2, lambda3, lambda4, lambda5,
                   t1, t2, t3, t4){
    delta1 <- t1
    delta2 <- t2-t1
    delta3 <- t3-t2
    delta4 <- t4-t3

    out <- setNames(rep(as.numeric(NA),7),
                    c(t1,t2,t3,t4,"infty","total","netBenefit"))
    out[1] <- 1-exp(-lambdaC*t1) + lambdaC/(lambdaC+lambda1)*(exp(-(lambdaC+lambda1)*t1)-1)

    out[2] <- exp(-lambdaC*t1)-exp(-lambdaC*t2) + lambdaC*exp(-lambda1*delta1)*exp(lambda2*t1)/(lambdaC+lambda2)*(exp(-(lambdaC+lambda2)*t2)-exp(-(lambdaC+lambda2)*t1))
    
    out[3] <- exp(-lambdaC*t2)-exp(-lambdaC*t3) + lambdaC*exp(-lambda1*delta1-lambda2*delta2)*exp(lambda3*t2)/(lambdaC+lambda3)*(exp(-(lambdaC+lambda3)*t3)-exp(-(lambdaC+lambda3)*t2))

    out[4] <- exp(-lambdaC*t3)-exp(-lambdaC*t4) + lambdaC*exp(-lambda1*delta1-lambda2*delta2-lambda3*delta3)*exp(lambda4*t3)/(lambdaC+lambda4)*(exp(-(lambdaC+lambda4)*t4)-exp(-(lambdaC+lambda4)*t3))

    out[5] <- exp(-lambdaC*t4) - lambdaC*exp(-lambda1*delta1-lambda2*delta2-lambda3*delta3-lambda4*delta4)*exp(lambda5*t4)/(lambdaC+lambda5)*exp(-(lambdaC+lambda5)*t4)

    out["total"] <- sum(out[1:5])
    out["netBenefit"] <- 1-2*out["total"]
    return(out)
}

HR <- 0.3
lambdaC <- 0.01

## ** scenario 3: early effect
lambda1E <- lambdaC * HR
lambda2E <- lambdaC * (0.25 + 0.75 * HR)
lambda3E <- lambdaC * (0.5 + 0.5 * HR)
lambda4E <- lambdaC * (0.75 + 0.25 * HR)
lambda5E <- lambdaC * 1

NBE <- calcNB(lambdaC = lambdaC,
              lambda1 = lambda1E,
              lambda2 = lambda2E,
              lambda3 = lambda3E,
              lambda4 = lambda4E,
              lambda5 = lambda5E, 
              t1 = 40,
              t2 = 60,
              t3 = 80,
              t4 = 100)

NBE
##         40         60         80        100      infty      total netBenefit 
## 0.01777268 0.01853809 0.02407939 0.02812682 0.25742445 0.34594144 0.30811713 


## FX <- function(t){
##     out <- rep(NA,length(t))
##     index1 <- which(t <= 40)
##     index2 <- intersect(which(t <= 60), which(t > 40))
##     index3 <- intersect(which(t <= 80), which(t > 60))
##     index4 <- intersect(which(t <= 100), which(t > 80))
##     index5 <- which(t > 100)

##     out[index1] <- 1-exp(-lambda1E*t[index1])
##     out[index2] <- 1-exp(-lambda1E*40-lambda2E*(t[index2]-40))
##     out[index3] <- 1-exp(-lambda1E*40-lambda2E*20-lambda3E*(t[index3]-60))
##     out[index4] <- 1-exp(-lambda1E*40-lambda2E*20-lambda3E*20-lambda4E*(t[index4]-80))
##     out[index5] <- 1-exp(-lambda1E*40-lambda2E*20-lambda3E*20-lambda4E*20-lambda5E*(t[index5]-100))
##     return(out)
## }
## fY <- function(t){
##     lambdaC*exp(-lambdaC*t)
## }

## integrate(f = function(t){fY(t)*FX(t)}, lower = 0, upper = 40, subdivisions = 1000)
## integrate(f = function(t){fY(t)*FX(t)}, lower = 40, upper = 60, subdivisions = 1000)
## integrate(f = function(t){fY(t)*FX(t)}, lower = 60, upper = 80, subdivisions = 1000)
## integrate(f = function(t){fY(t)*FX(t)}, lower = 80, upper = 100, subdivisions = 1000)
## integrate(f = function(t){fY(t)*FX(t)}, lower = 100, upper = 1000, subdivisions = 1000)
## integrate(f = function(t){fY(t)*FX(t)}, lower = 0, upper = 1000, subdivisions = 1000)

## d <- simData(n.C = 10000,
##              n.T = 10000,
##              HR = 0.3,
##              hazard.C = 0.01,
##              hazard.censoring = 0.000000000000000000000001,
##              type = "early")
## e <- BuyseTest(group ~ cont(timetoevent, threshold = 0),
##                method.inference = "u-statistic",
##                data=d)
## confint(e, statistic = "favorable")
## confint(e, statistic = "unfavorable")



## ** scenario 4: late effect
lambda1D <- lambdaC * 1
lambda2D <- lambdaC * (0.75 + 0.25 * HR)
lambda3D <- lambdaC * (0.5 + 0.5 * HR)
lambda4D <- lambdaC * (0.25 + 0.75 * HR)
lambda5D <- lambdaC * HR

NBD <- calcNB(lambdaC = lambdaC,
              lambda1 = lambda1D,
              lambda2 = lambda2D,
              lambda3 = lambda3D,
              lambda4 = lambda4D,
              lambda5 = lambda5D, 
              t1 = 40,
              t2 = 60,
              t3 = 80,
              t4 = 100)
print(NBD)
##         40         60         80        100      infty      total netBenefit 
## 0.05434444 0.04621729 0.04634688 0.04260991 0.23944854 0.42896705 0.14206590 

