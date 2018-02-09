unmetEa <- equityAllocation[unSpendingYrs > 0] # vector if the equity allocation for every failed scenario
underfundedyears <- unSpendingYrs[unSpendingYrs > 0] # vector of number of years failed for each failed scenario
aad <- aggregate(underfundedyears~unmetEa,adata,mean)

print(
 plotmeanyears <- ggplot(data=aad, aes(x=aad$unmetEa * 100,y=aad$underfundedyears)) + 
      geom_bar(stat="identity", position="dodge",
                     col="blue", 
                     fill="blue", 
                     alpha = .2) + 
      labs(x="Equity Allocation (%)", y="Mean Failed Years")  +
      ggtitle(paste("Figure 11. Mean Years per Failed Scenario\nby Equity Allocation")) +
      scale_x_discrete(limit = seq(0,100,10)) 
 
)
