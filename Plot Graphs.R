plotMortTable <- function(probSurviveAll,plotGen) {

  if (!plotGen) return
library(reshape2)
plotdata <- reshape2::melt(probSurviveAll)
print(
  ggplot(plotdata, aes(x = Var2, y = value, fill = Var1)) +
    scale_fill_manual(values=c("blue", "red","green"),name="Surviving\nSpouses",labels=c("Both Survive", "Only Female Spouse Survives", "Only Male Spouse Survives")) +
    geom_bar(stat = "identity") + ggtitle("Mortality Table Generated") +
    xlab("Year of Retirement") + 
    theme(legend.position=c(.8,0.8)) +
    xlim(0,50) +
    ylab("Probabilty of Survival")) 

return

}

marketReturnsPlot <- function (mr.df, mrLabel, plotGen=TRUE) {
  
  if (!plotGen) return
  library(reshape2)
  
print(
  ggplot(data=mr.df, aes(mr.df$Returns)) +
    geom_histogram(binwidth=1,
                   col="blue",
                   fill="blue",
                   alpha = .2) +
    xlim(-40,60) +
    labs(x="Simulated Market Returns (%)", y="Count")  +
    ggtitle(paste("Histogram of \nSimulated Annual Real Market Returns")) +
    annotate("text", x = -25, y = n*1.8, label =mrLabel)

)

}