library (ggplot2)
scenarios1 <- length(market.rmsM[,1]) # number of retirements to simulate
years1 <- 30 # fixed retirement length in years
ip <- 1000000  # initial portfolio value
portBalance <- array(0,c(scenarios1,years1,length(seq(.02,.07,.01))))

wrIndex <- 0
spendAmount <-rep(0,length(seq(.02,.07,.01)))
fr <-rep(0,length(seq(.02,.07,.01))) # failure rate

for (wr in seq(.02,.07,.01)) {    # initial fixed withdrawals as % of initial portfolio value
  wrIndex <- wrIndex + 1
  spendAmount[wrIndex] <- wr * ip
  # cat("\nSpending amount for wr ",wr," is ",spendAmount[wrIndex])
  for (i in 1:scenarios1){
    portBalance[i,1,wrIndex] <- (ip - spendAmount[wrIndex]) * market.rmsM[1,1] # first year
    # cat("\nPortfolio Balance for wr% ",wr," Scenario ",i," Year 1 is ",portBalance[i,1,wrIndex])
    
    for (y in 2:years1){
      portBalance[i,y,wrIndex] <- max(0,(portBalance[i,y - 1,wrIndex] - spendAmount[wrIndex]) * market.rmsM[i,y])
   # cat("\nPortfolio Balance for wr% ",wr," Scenario ",i," Year ",y," is ",portBalance[i,y,wrIndex])
       } # end loop through years of each scenario
  
fr[wrIndex] <-  sum(portBalance[,years1,wrIndex] == 0) / scenarios1  # calculate failure rate
  } # end loop for all scenarios
  
  year30s <- portBalance[,30,]
  is.na(year30s) <- year30s == 0
  mTPV <- colMeans(year30s,na.rm=TRUE)

}

cat("\n\nFailure rate is ",fr)
cat("\nAnnual Spending ",spendAmount)
cat("\nMean Terminal Portfolio Value for successful scenarios is ",mTPV)

plotIt.df <- data.frame(spendAmount/ip,fr,mTPV)

pit <- ggplot(plotIt.df,aes(plotIt.df$spendAmount.ip,plotIt.df$fr))+
  geom_bar(stat="identity",col="blue", 
           fill="blue", 
           alpha = .2) + 
  ggtitle("Probability of Failure") +
  xlab("Fixed Spending % of Initial Portfolio Balance") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Probability of Portfolio Failure") +
  scale_x_continuous(labels = seq(2,7,1),breaks = seq(.02,.07,.01) )
print(pit)
ggsave("~/Desktop/Prob of Failure.png",dpi=600)

pit <- ggplot(plotIt.df,aes(plotIt.df$spendAmount.ip,plotIt.df$mTPV))+
  geom_bar(stat="identity",col="blue", 
           fill="blue", 
           alpha = .2) + 
  ggtitle("Mean Terminal Portfolio Value","Initial Portfolio Value = $1M") +
  xlab("Fixed Spending % of Initial Portfolio Balance") +
  scale_y_continuous(labels = scales::dollar) +
  ylab("Mean Terminal Portfolio Value") +
  scale_x_continuous(labels = seq(2,7,1),breaks = seq(.02,.07,.01) )
print(pit)
ggsave("~/Desktop/Mean TPV.png",dpi=600)