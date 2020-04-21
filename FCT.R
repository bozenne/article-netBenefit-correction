### FCT.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: apr 20 2020 (11:10) 
## Version: 
## Last-Updated: apr 21 2020 (10:14) 
##           By: Brice Ozenne
##     Update #: 21
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * loadRes
loadRes <- function(path, tempo.file = FALSE,
                    export.attribute = NULL, trace = TRUE){
    all.files <- list.files(path)
    file.tempo <- grep("(tempo)",all.files,value = TRUE)
    file.final <- setdiff(all.files, file.tempo)


    if(tempo.file){
        file.read <- file.tempo
    }else{
        file.read <- file.final
    }
    n.file <- length(file.read)

    myApply <- switch(as.character(as.logical(trace)),
                      "TRUE" = pbapply::pblapply,
                      "FALSE" = lapply)

    ls.out <- do.call(myApply, args = list(X = 1:n.file, FUN = function(iFile){
        iRead <- readRDS(file = file.path(path,file.read[iFile]))
        iOut <- cbind(data.table::as.data.table(iRead),
                      file = file.read[iFile])
        if(!is.null(export.attribute)){
            attr(iOut,export.attribute) <- attr(iRead,export.attribute)
        }
        return(iOut)
    }))
    
    out <- do.call(rbind, ls.out)
    if(!is.null(export.attribute)){
        attr(out,export.attribute) <- attr(ls.out[[1]],export.attribute)
    }
    return(out)
}

## * GGarticle
GGarticle <- function(object, formula.grid = ~n.char){
    ggdata <- copy(object)
    Delta <- ggdata[n==max(n) & scoring.rule=="GS",mean(estimate.mean)]
    ggdata[, n.char := paste0("sample size: ",n," per group")]
    ggdata[, n.char := factor(n.char, levels = paste0("sample size: ",unique(sort(n))," per group"))]

    gg <- ggplot(ggdata[scoring.rule != "GS"],
                 aes(x = pc.censoring,
                     y = estimate.mean,
                     group = scoring.rule,
                     shape = scoring.rule,
                     linetype = scoring.rule,
                     color = scoring.rule))
    ## gg <- gg + geom_abline(intercept = Delta, slope = 0, col = "red")
    gg <- gg + geom_point() + geom_line()
    gg <- gg + facet_wrap(formula.grid) + geom_line()
    gg <- gg + scale_color_manual(name = "",
                                  values = c("Gehan" = "gray",
                                             "Gehan.C" = "black",
                                             "Peron" = "gray",
                                             "Peron.C" = "black"),
                                  labels = c("Gehan's scoring rule, uncorrected",
                                             "Gehan's scoring rule, corrected",
                                             "Peron's scoring rule, uncorrected",
                                             "Peron's scoring rule, corrected"))
    gg <- gg + scale_linetype_manual(name = "",
                                     values = c("Gehan" = 2,
                                                "Gehan.C" = 2,
                                                "Peron" = 1,
                                                "Peron.C" = 1),
                                     labels = c("Gehan's scoring rule, uncorrected",
                                                "Gehan's scoring rule, corrected",
                                                "Peron's scoring rule, uncorrected",
                                                "Peron's scoring rule, corrected"))
    gg <- gg + scale_shape_manual(name = "",
                                  values = c("Gehan" = 17,
                                             "Gehan.C" = 17,
                                             "Peron" = 19,
                                             "Peron.C" = 19),
                                  labels = c("Gehan's scoring rule, uncorrected",
                                             "Gehan's scoring rule, corrected",
                                             "Peron's scoring rule, uncorrected",
                                             "Peron's scoring rule, corrected"))
    gg <- gg + theme(legend.position = "bottom", legend.key.size = unit(3,"line"))
    gg <- gg + guides(color=guide_legend(nrow=2,byrow=TRUE),linetype=guide_legend(nrow=2,byrow=TRUE))
    gg <- gg + xlab("Proportion of censored observations")
    gg <- gg + ylab(expression("mean "*hat(Delta)))
    gg <- gg + scale_x_continuous(breaks=seq(0,1,0.2))
    return(gg)

}

## * GGcoverage
GGcoverage <- function(object){
    ggdata <- copy(object)
    ggdata[, n.char := paste0("sample size: ",n," per group")]
    ggdata[, n.char := factor(n.char, levels = paste0("sample size: ",unique(sort(n))," per group"))]

    gg <- ggplot(ggdata[scoring.rule != "GS"],
                 aes(x = pc.censoring,
                     y = coverage,
                     group = scoring.rule,
                     shape = scoring.rule,
                     linetype = scoring.rule,
                     color = scoring.rule))
    gg <- gg + geom_abline(intercept = 0.95, slope = 0, col = "red")
    gg <- gg + geom_point() + geom_line()
    gg <- gg + facet_wrap(~n.char) + geom_line()
    gg <- gg + scale_color_manual(name = "",
                                  values = c("Gehan" = "gray",
                                             "Gehan.C" = "black",
                                             "Peron" = "gray",
                                             "Peron.C" = "black"),
                                  labels = c("Gehan's scoring rule, uncorrected",
                                             "Gehan's scoring rule, corrected",
                                             "Peron's scoring rule, uncorrected",
                                             "Peron's scoring rule, corrected"))
    gg <- gg + scale_linetype_manual(name = "",
                                     values = c("Gehan" = 2,
                                                "Gehan.C" = 2,
                                                "Peron" = 1,
                                                "Peron.C" = 1),
                                     labels = c("Gehan's scoring rule, uncorrected",
                                                "Gehan's scoring rule, corrected",
                                                "Peron's scoring rule, uncorrected",
                                                "Peron's scoring rule, corrected"))
    gg <- gg + scale_shape_manual(name = "",
                                  values = c("Gehan" = 17,
                                             "Gehan.C" = 17,
                                             "Peron" = 19,
                                             "Peron.C" = 19),
                                  labels = c("Gehan's scoring rule, uncorrected",
                                             "Gehan's scoring rule, corrected",
                                             "Peron's scoring rule, uncorrected",
                                             "Peron's scoring rule, corrected"))
    gg <- gg + theme(legend.position = "bottom", legend.key.size = unit(2,"line"))
    gg <- gg + guides(color=guide_legend(nrow=2,byrow=TRUE),linetype=guide_legend(nrow=2,byrow=TRUE))
    gg <- gg + xlab("Proportion of censored observations")
    gg <- gg + ylab(expression("coverage of CI for "*hat(Delta)))
    gg <- gg + scale_x_continuous(breaks=seq(0,1,0.2))
    return(gg)

}

######################################################################
### FCT.R ends here
