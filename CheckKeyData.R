# compare key data to make sure I go the same output as previously saved

if (sum(homeEquityA != homeEquity[,1:58]) > 0) cat("\n*** homeEquity matrix is different!")
if (sum(mortgageBalanceA != mortgageBalance[,1:58]) > 0) cat("\n*** mortgageBalance matrix is different!")
if (sum(creditLimitA != creditLimit) > 0) cat("\n*** creditLimit matrix is different!")
if (sum(unmetSpendingA != unmetSpending) > 0) cat("\n*** unmetSpending matrix is different!")
if (sum(hecmSpendA != hecmSpend[,1:58]) > 0) cat("\n*** hecmSpend matrix is different!")
if (sum(spendPortA != spendPort) > 0) cat("\n*** spendPort matrix is different!")
if (sum(portfolioBalanceA != portfolioBalance[,1:58]) > 0) cat("\n*** portfolioBalance matrix is different!")
if (sum(liborMA != liborM[,1:58]) > 0) cat("\n*** portfolioBalance matrix is different!")
if (sum(marketVectorA != marketVector[2:59]) > 0) cat("\n*** marketVector matrix is different!")
if (sum(market.rmsMA != market.rmsMA[,1:58]) > 0) cat("\n*** market.rms matrix is different!")




cat("\nFinished comparing output to previous results.")