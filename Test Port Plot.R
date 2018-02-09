library(reshape2)
library(ggplot2)

worst10scenarios <- order(unmetSpendIndex[annuityAllocation < 1], decreasing=TRUE)[1:10]
best10scenarios <- order(termPortValue, decreasing=TRUE)[1:10]
# find scenario with terminal portfolio value closest to median
medScenario <- which(abs(termPortValue-median(termPortValue))==min(abs(termPortValue-median(termPortValue))))[1]  
med <- data.frame(nw[medScenario,])
med$year <- seq(1,65)
nw <- cbind(saveInitPort,portfolioBalance)
nw[states == 0] <- 0
bw <- data.frame(rbind(nw[best10scenarios,],nw[worst10scenarios,],nw[medScenario,]))
# # df = data.frame(cat = LETTERS[1:6], VAR1 = runif(6), VAR2 = runif(6), VAR3 = runif(6), VAR4 = runif(6))
# # df_melted = melt(df, id.vars = 'cat')
bw$scenario <- 1:21
df <- data.frame(bw)

df_melted <- melt(df,id.vars = "scenario")
df_melted <- subset(df_melted, value !=0)
levels(df_melted$variable) <- seq(0,64)
colnames(med) <- c("Portfolio Value","Year of Retirement")

print(
  ggplot(data=df_melted, aes(x = variable, y = value,color=scenario,group=scenario)) + 
    geom_line() +
   
    scale_x_discrete(breaks = scales::pretty_breaks()(1:56)) +
    scale_y_continuous(labels= scales::comma, limits= c(0,4*saveInitPort)) +
    theme(legend.position="none") +
    labs(x="Year of Retirement", 
         y="Year of Retirement",
         title="Ten Best and Worst Terminal Portfolio Value Paths",
         subtitle="Exludes scenarios with 100% annuitization of portfolio.") +
    annotate("text", x = tail(which(nw[medScenario,]!=0),1), y = termPortValue[medScenario], label = "median TPV")
          )