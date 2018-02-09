plotData.df <- data.frame(percentFunded[percentFunded < 1] * 100)

ggplot (data=plotData.df,aes(plotData.df$percentFunded.percentFunded...1.)) +
  geom_histogram(binwidth = 1,
                 col="blue", 
                 fill="blue", 
                 alpha = .2) +

  xlab("Percent of Years Funded in Underfunded Scenarios") + ylab("Number of Scenarios") +
  annotate("text", x = 60, y =20, label = "Rightmost columns are number of underfunded\nscenarios that were mostly funded.\nLeftmost columns were least-funded scenarios.") +
  ggtitle("Histogram of Percent of Years Funded\nfor Underfunded Scenarios")