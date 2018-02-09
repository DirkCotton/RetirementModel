# convert a base hist() call to a ggplot histogram

histgg <- function(x,main,breaks,xlab,ylab="Count",col,labels) {
 
data.df <- data.frame(x)
colnames(data.df) <- "variable"
if (plotGen) {
  print(
    ggplot(data=data.df, aes(data.df$variable)) + 
      geom_histogram(binwidth=10, 
                     col="blue", 
                     fill="blue", 
                     alpha = .2) + 
      labs(title=main) +
      labs(x=xlab, y=ylab)  +
      ggtitle(main) +
      scale_x_discrete(limit = seq(0,.4,.1))
  )
}
}


###Main test
h2 <- histgg(equityAllocation[unmetSpendIndex]*100,col="lightblue",labels=TRUE,breaks = 14,main=paste("Histogram of Equity Allocations\nfor",length(unmetSpendIndex),"Underfunded Scenarios"),xlab="Equity Allocation (%)",ylab="Counts")
h3 <- histgg(annuityAllocation,main=paste("Histogram of Annuity Allocations\nfor",n,"Scenarios"),xlab="Allocation of Initial Portfolio Balance to Annuity Purchase")

# aA <- data.frame(annuityAllocation)
# if (plotGen) ggplot(data=aA, aes(aA$annuityAllocation)) +
#   geom_histogram(binwidth=.05,fill="red") +
#   labs(title=paste("Histogram of Annuity Allocations\nfor",n,"Scenarios"),x="Allocation of Initial Portfolio Balance to Annuity Purchase") +
#   scale_x_continuous(labels = percent)

