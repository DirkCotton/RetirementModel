# Plot a chart of mean safe floor income with a ribbon showing min-max range

ydata <- floor

is.na(ydata) <- states == 0 | states == 4  # set all floor data to NA when no one is alive to leave out of the min, max and mean calculations
y <- colMeans(ydata,na.rm = TRUE)           # find mean floor value (only when at least one survivor)
ub <- colMaxs(ydata,na.rm = TRUE) # [is.na(y) == FALSE]  # find upper bound of graph ribbon (largest floor value)
lb <- colMins(ydata,na.rm = TRUE) # [is.na(y) == FALSE]  # find lower bound of graph ribbon (smallest floor value)

for(cc in length(y):1) {  # remove all columns from chart for which there were no survivors in any scenario
  if (is.na(y[cc])) {
    y <- y[1:(cc - 1 )]  
    lb <- lb[1:(cc - 1 )]
    ub <- ub[1:(cc - 1 )]
    }
}

x <- 1:length(y)

plotdataFloor <- data.frame(x=x, y=y, lower = lb, upper = ub)  # create data frame for plot

print(
  p <- ggplot(plotdataFloor) + geom_line(aes(y=y, x=x))+
    geom_ribbon(aes(ymin=lb, ymax=ub, x=x, fill = "band"), alpha = 0.3)+
    labs(title="Figure 13. Safe Floor Income by Year of Retirement",subtitle="Mean and Range: Pensions, Annuities, TIPS & Social Security Benefits",x="Year of Retirement",y="Mean Floor Income for all Scenarios ($)") +
    theme(legend.position="none") +  # remove legend
    scale_y_continuous(labels = scales::comma) 
)


