## This file generates figure D of the supplementary materials
save <- FALSE

## * load R packages
library(ggplot2)

## * Functions computing the hazard ratio
HRearly <- function(x, HR = 0.3){
    if(x<=40){
        return(HR)
    }else if(40<x && x<=120){
        return(HR + (1-HR)*(x-40)/80)
    }else{
        return(1)
    }
}

HRlate <- function(x, HR = 0.3){
    if(x<=40){
        return(1)
    }else if(40<x && x<=120){
        return(1 - (1-HR)*(x-40)/80)
    }else{
        return(HR)
    }
}


ls.figureD <- lapply(seq(0,150,by=1),function(x){
    rbind(data.frame(type = "Scenario 4: delayed treatment effect",
                     time=x,
                     HR = HRlate(x=x, HR=0.3)),
          data.frame(type = "Scenario 3: early treatment effect",
                     time=x,
                     HR = HRearly(x=x, HR=0.3)))
})
df.figureD <- do.call(rbind,ls.figureD)

## * graphical display
figureD <- ggplot(df.figureD, aes(x=time,y=HR))
figureD <- figureD + geom_line(size = 1.5)
figureD <- figureD + facet_wrap(~type)
figureD <- figureD + xlab("Time (months)") + ylab("Hazard ratio") + coord_cartesian(ylim = c(0,1))
figureD <- figureD + theme(text = element_text(size=20))

print(figureD)
if(save){
    ggsave(figureD, filename = file.path("figures/figureD.png"), width = 10)
    ggsave(figureD, filename = file.path("figures/figureD.pdf"), width = 10)
}
