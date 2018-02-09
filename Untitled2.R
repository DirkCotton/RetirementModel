# Plot a chart of mean safe floor income when both spouses are living 

ydata <- floor

is.na(ydata) <- states != 3 | ydata == 0   # set all floor data to NA when both spouse aren't alive of the min, max and mean calculations

y <- colMeans(ydata,na.rm = TRUE)[!is.nan(colMeans(ydata,na.rm = TRUE))]  
lb <- y[1:length(y)]
ub <- y[1:length(y)]

x <- 1:length(y)

exp1 <- rep(desiredSafetyNet,length(y))  # graph only non-zero expenses (zero expenses are for states with no survivors)

z <- exp1[1:length(y)]

plotdataFloor <- data.frame(x=x, y=y, lower = lb, upper = ub,z)  # create data frame for plot

print(
  p <- ggplot(plotdataFloor) + geom_line(aes(y=y, x=x),color="darkred") +   # mean floor income available
    geom_line(aes(y=z, x=x),color="blue") +      # mean total spending
    labs(title="Figure 13. Safe Floor Income vs. Desired Floor\nBoth Spouses Living",subtitle="Mean and Range: Pensions, Annuities, Social Security (red)\nMean Expenses (blue)",x="Year of Retirement",y="Mean Floor Income for all Scenarios ($)") +
    theme(legend.position="none") +  # remove legend
    scale_y_continuous(labels = scales::comma) 
)