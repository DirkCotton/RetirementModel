summarize <- function (summaryScenario,nyrs,states,husbandAge,wifeAge,husbandClaimAge,wifeClaimAge,socSecM,market.rmsM,market.csM,initportfolio,spendM,swrM,client.incomesM,annuityIncomeM,tpvs,unmetSpend,rmMortBal,locHECM,initMort,homeEquity,hecmSpend,spendFromPortfolio,expenses,networth,portGrowth,equityAllocation) {

# Print out a summary of scenario number summaryScenario

cat("\n\nSummary Data for Scenario Number ",summaryScenario,sep=" ")
cat ("\nNumber of scenarios",prettyNum(length(states[,1]),scientific=FALSE,big.mark=","),sep=" ")
cat ("\nSpending needs were not met in ",prettyNum(length(unmetSpendIndex),scientific=FALSE,big.mark=",")," scenarios (",length(unmetSpendIndex)/n*100,"%).")

statesDef <- c("Neither spouse alive","Only husband survives","Only wife survives","Both spouses alive","Surviving spouse died last year.")
  

  # 
########################################################
# Plot expenses versus annual spending for this scenario
########################################################
 
library(reshape2)
plotData3 <- data.frame(pmax(0,spendFromPortfolio[summaryScenario,]),hecmSpend[summaryScenario,],socSecM[summaryScenario,],hPensionIncomeM[summaryScenario,],wPensionIncomeM[summaryScenario,],spiaIncomeM[summaryScenario,],diaIncomeM[summaryScenario,],tipsIncomeM[summaryScenario,],reserveSpend[summaryScenario,1:nyrs])

plotData3$row <- seq_len(nrow(plotData3))
plotYears <- survived[summaryScenario]  # number of years in this scenario with at least one survivor
plotData3 <- plotData3[1:plotYears,]

colnames(plotData3) <- c("Spend from Portfolio","Spend from HECM","Social Security","Pension (Husband)","Pension (Wife)","SPIA","DIA","TIPS Ladder","Reserves","rows")
plotData2 <- melt(plotData3,id.vars = "rows")

plotExpenses <- data.frame(expenses[summaryScenario,1:plotYears])

plotExpenses$rows <- seq(1,plotYears) # replaced seq_len(nrow(plotExpenses))
colnames(plotExpenses) <- c("Expenses","rows")
plotExpenses <- melt(plotExpenses,id.vars = "rows")
plotExpenses <- plotExpenses[1:plotYears,]

library(ggplot2)

print(ggplot(plotData2, aes(x=rows, y=value, fill=variable)) + 
        geom_bar(stat="identity") +
        geom_point(data=plotExpenses,aes(x=rows,y=value)) +
        # theme(legend.position = c(.2,.2)) +
        ggtitle (paste("Figure 17. Income Source vs. Expenses (black)\nScenario Number ",summaryScenario)) +
        xlab("\nYear of Retirement") +
        ylab("Annual $(Real) Income") +
        scale_y_continuous(labels = scales::comma) +
        labs(fill='Income Source') 
)

###################################################
# Plot portfolio balances for this scenario
###################################################

plotTpv <- data.frame(swrM[summaryScenario,])
plotTpv$rows <- seq_len(nrow(plotTpv))
colnames(plotTpv) <- c("Portfolio Balance","rows")
plotTpv2 <- melt(plotTpv,id.vars = "rows")

if (plotGen) print(ggplot(plotTpv2, aes(x=rows, y=value, fill=variable)) +
        geom_bar(stat="identity") +
        theme(legend.position = c(.8,.8)) +
        ggtitle ("Portfolio Balance\nScenario Number ",summaryScenario) +
        xlab("\nYear") +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(limits=c(0,40)) +
        theme(legend.position="none") +
        ylab("Real Terminal Portfolio Value") 
     )

###################################################
# Plot net worth balances for this scenario
###################################################

plotNetWorth <- data.frame(pmax(0,homeEquity[summaryScenario,]),swrM[summaryScenario,])
plotNetWorth$rows <- seq_len(nrow(plotNetWorth))
colnames(plotNetWorth) <- c("Home Equity","Portfolio Balance","rows")
plotNetWorth2 <- melt(plotNetWorth,id.vars = "rows")

if (plotGen) print(ggplot(plotNetWorth2, aes(x=rows, y=value, fill=variable)) +
        geom_bar(stat="identity") +
        theme(legend.position = c(.8,.8)) +
        ggtitle ("Net Worth\nScenario Number ",summaryScenario) +
        xlab("\nYear") +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(limits=c(0,40)) +
        ylab("Real Net Worth") 
)


###################################################
# Plot terminal portfolio values for failing years
###################################################

if (length(unmetSpendIndex) > 0) {
  if (plotGen) hist(apply(states,1,which.max) - 1,breaks=length(unmetSpendIndex),xlab="Number of Years Spending Was Met",ylab="Number of Scenarios",main="Histogram of Number of Years \nSpending Demand Was Met \nin Failed Scenarios")
  }



    return
}

