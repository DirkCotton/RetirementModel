# Summarize a specific RetirementModel scenario

# cat("\014")  # clear the console
source('~/R Projects/RetirementModel/Summarize.R')

summarizeThis <- 216
f <- summarizeThis

# summarize this scenario
summaryScenario <- summarizeThis


s <- summarize (summarizeThis,nyrs,states,husbandAge,wifeAge,husbandClaimAge,wifeClaimAge,socSecM,market.rmsM,market.csM,initportfolio,spendM,swrM,client.incomesM,annuityIncomeM,tpvs,unmetSpend,mortgageBalance,locHECM,initMort,homeEquity,hecmSpend,spendFromPortfolio,expenses,networth,marketVector,equityAllocation)

#####################################################
# summarize example requested and write to Excel file
#####################################################

state4 <- which(states[summaryScenario,] == 4)
p1 <- NULL
p2 <- NULL
p3 <- NULL
p4 <- NULL
p5 <- NULL
p6 <- NULL
p7 <- NULL
p8 <- NULL
p9 <- NULL
p10 <- NULL
p11 <- NULL
p12 <- NULL
p13 <- NULL
p14 <- NULL
p15 <- NULL
p16 <- NULL
p17 <- NULL
p18 <- NULL
p19 <- NULL
p20 <- NULL
p20 <- NULL
p21 <- NULL
p22 <- NULL
p23 <- NULL
p24 <- NULL
p25 <- NULL
p26 <- NULL
p27 <- NULL
p28 <- NULL
p29 <- NULL
p30 <- NULL
p31 <- NULL
p32 <- NULL

rmSpend <- matrix(0,nscen,nyrs)
i <- 1
while (i < state4) {
  
  p14[i] <- husbandAge+i-1
  p15[i] <- wifeAge+i-1
  p16[i] <- husbandClaimAge
  p17[i] <- wifeClaimAge
  p18[i] <- min(which(states[summaryScenario,] < 3)) - 1  # first spouse dies
  p1[i] <- spendFromPortfolio[summaryScenario,i] #   annualPortSpend
  p3[i] <- socSecM[summaryScenario,i]
  p4[i] <- hecmSpend[summaryScenario,i]   # spend from HECM
  p2[i] <- annuityIncomeM[summaryScenario,i] + p3[i]  # safe spending total
  p3[i] <- socSecM[summaryScenario,i]
  p28[i] <- tipsIncomeM[summaryScenario,i]           # annual TIPS ladder spending
  p5[i] <- spendFromPortfolio[summaryScenario,i] + socSecM[summaryScenario,i] + annuityIncomeM[summaryScenario,i] + hecmSpend[summaryScenario,i] + tipsIncomeM[summaryScenario,i] + reserveSpend[summaryScenario,i] # total spending
  p6[i] <- expenses[summaryScenario,i]
  p12[i] <- swrM[summaryScenario,i]     # portfolio balances
  p8[i] <- round(mortgageBalance[summaryScenario,i])
  p9[i] <- homeEquity[summaryScenario,i]
  
  p11[i] <- locHECM[summaryScenario,i]    # HECM line of credit
  p7[i] <- market.rmsM[summaryScenario,i]      # Portfolio Balance
  p19[i] <- rateHECM[summaryScenario,i]     # HECM adjustable interest rate
  p20[i] <- marketVector[summaryScenario,i]  # weighted allocation portfolio returns
  p21[i] <- liborM[summaryScenario,i]  # weighted allocation portfolio returns
  p22[i] <- states[summaryScenario,i]  # weighted allocation portfolio returns
  p23[i] <- equityAllocation[i]  # equity allocation
  
  p24[i] <- spiaIncomeM[summaryScenario,i]     #income from SPIAs
  p25[i] <- diaIncomeM[summaryScenario,i]     #income from DIAs
  p26[i] <- hPensionIncomeM[summaryScenario,i]     #income from husband's pension
  p27[i] <- wPensionIncomeM[summaryScenario,i]     #income from wife's pension
  p30[i] <- reserveMarketVector[summaryScenario,i + 1]  # weighted reserve fund allocation portfolio returns
  p31[i] <- reserveFundM[summaryScenario,i + 1]  # reserve fund portfolio balance
  p32[i] <- reserveSpend[summaryScenario,i]     # amounts spent from reserve fund
  i <- i + 1
  
}
# residual market value of TIPS ladder at end of retirement

############################ ERROR
if (sum(ladderTV) == 0) ladderLength <- length(p1)
if (ladderLength >= length(p1)) p29 <- ladderTV[1:ladderLength] 
if (ladderLength < length(p1))  p29 <- c(ladderTV,rep(0,(length(p21)-ladderLength) ))
p10 <- p9 + p12  + p29 + p31 #net worth is home equity plus portfolio balance plus TIPS ladder remainder + reserve fund balance

pcDepletedPorts <- 100 * sum(depletedPort)/n    # percent of scenarios with depleted portfolios
spendLessExp <- round(totalSpending-expenses)   # calculate column vector of total spending less expenses
summarizeCsv <- data.frame(p14,p15,p22,p16,p17,p18,p24,p25,p28,p26,p27,p3,p2,p4,p1,p32,p5,p6,p7,p8,p9,p12,p29,p31,p10,p11,p19,p20,p21,p23,p30)

colnames(summarizeCsv) <- c("Husband Age","Wife Age","State","Husband Claims SS","Wife Claims SS","1st Spouse Dies","Spend from SPIAs","Spend from DIAs","spend from TIPS","Spend from Husband Pension","Spend from Wife Pension","Spend from SS","Total Safe Spending","Spend from HECM","Spend from Port","Spend Reserves","Total Spend","Expenses","Market Return","RM Bal","Home Equity","Port Bal","TIPS Balance","Reserve Balance","Net Worth","HECM LOC","HECM Rate","Portfolio Growth","Libor","Equity Alloc","Reserve Growth")
outFile <- paste("~/desktop/summarizecsv",toString(summaryScenario),".csv")
cat("\nSummary of Scenario ",summaryScenario," written to ",outFile)
write.csv(summarizeCsv,paste("~/desktop/summarizecsv",toString(summaryScenario),".csv"))

unSpendingYrs <- rowSums(round(totalSpending-expenses) < 0)  # column of unmet spending years per scenario
percentFunded <- 1 - (unSpendingYrs / lengthRetirement)   # calculate vector of percent years funded of years survived
scenUnmetSpend <- sum(rowSums(round(totalSpending-expenses)) != 0) #Number of scenarios with unmet spending
pcScenUnmet <- 100*sum(rowSums(round(totalSpending-expenses)) != 0)/n # percent of scenarios with at least one year unmet spending
first10unmet <- which(rowSums((round(totalSpending-expenses))) != 0)[1:10]  # first 10 senarios with unmet spending
yrsUnmetSpend <- sum(round(totalSpending-expenses) != 0)  # number of years of unmet spending in all scenarios
meanYrsUnmetSpend <- sum(round(totalSpending-expenses) != 0)/sum(rowSums(round(totalSpending-expenses)) != 0)  # mean years of unmet spending when scenario fails to meet spending at least one year
pcTappedLOC <- 100 * sum(tappedHECM)/n          #percent of scenarios that tapped HECM LOC
pcDepletedLOC <- 100 * sum(depletedHECM)/n      #percent of scenarios that depleted HECM LOC

lengthRetirement <- which(states == max(states), arr.ind = TRUE)   # create vector of length of each scenario's retirement in years
lengthRetirement <- lengthRetirement[ order(lengthRetirement[,1]), ]
lengthRetirement <- lengthRetirement[,2] - 1

cat("\nNumber of scenarios that failed to meet spending at least 95% of years ",sum(percentFunded < .95)," (",100*sum(percentFunded < .95)/n,"%)")

cat("\nFirst ten unmet spending scenarios: ",first10unmet)
cat("\nNumber of years with unmet spending ",prettyNum(yrsUnmetSpend,scientific=FALSE,big.mark=","))
cat("\nPercent of years with unmet spending and at least one spouse surviving",100*yrsUnmetSpend/sum(lengthRetirement),"%.")
cat("\nMean years with unmet spending when spending not met ",meanYrsUnmetSpend)
cat("\n\nDepleted portfolios = ",pcDepletedPorts,"%")
cat("\nTapped HECM L.O.C. = ",sum(tappedHECM)," ",pcTappedLOC,"%")
cat("\nDepleted HECM L.O.C. = ",sum(depletedHECM)," ",pcDepletedLOC,"%")

if (knitrSave == TRUE) {
cat("\n\nSET KNITR SAVE SummarizeScenarion line 136")
save.image("~/Desktop/RetirementModelData.RData")  # save environment for knitr
  cat("\n***Model Image saved for Knitr.")
}

# cat("\nMean Years of Unmet Spending in mean(unSpendingYrs[onlyFailed6670])
# mean(unSpendingYrs[onlyFailed6670])

if (plotGen) hist((marketVector-1)*100,col="lightblue",xlab="Simulated Annual Portfolio Returns (%)",xlim=c(-40,60),main=paste("Histogram of \nSimulated Annual Real Portfolio Returns"))

#############################################
# Plot histogram of Net Worth at death
#############################################

# ignore net worth greater than 4 times initial net worth (highly unlikely) and plot histogram.
if (plotGen) {
  tpv.df <- data.frame(termNetWorth/1000000)  # plot terminal net worth in $M
  colnames(tpv.df) <- "TNW"
  
  print(
    ggplot(data=tpv.df, aes(tpv.df$TNW))  +
      geom_histogram(
        col="blue",
        fill="blue",
        alpha = .2) +
      xlim(0,quantile(tpv.df$TNW,.95)) +
      labs(x="Terminal Net Worth ($M)", y="Number of Scenarios")  +
      ggtitle("Figure 8. Histogram of Terminal Networth to 95th %-ile") 
    
  ) 
}

if (printSummary == TRUE) {
  cat("\nMedian Net Worth= ",prettyNum(median(termNetWorth),scientific=FALSE,big.mark=","),".")
}

if (plotGen) {
  
  tpv2.df <- data.frame(termPortValue/1000000)  # plot terminal net worth in $M
  colnames(tpv2.df) <- "TPV"
  
  tpvMax <- quantile(tpv2.df$TPV,.95)
  
  print(
    plt <- ggplot(data=tpv2.df, aes(tpv2.df$TPV))  +
      geom_histogram(breaks=seq(0,8,0.1),
                     col="blue",
                     fill="blue",
                     alpha = .2) +
      xlim(0,tpvMax) +
      labs(x="Terminal Portfolio Value ($M)", y="Number of Scenarios")  +
      scale_y_continuous( limits = c(0,400), expand = c(0,0) ) +
      ggtitle("Figure 8a. Histogram of Terminal Portfolio Values to 95th %-ile") 
    
  ) 
}

if (plotGen) {
  tpvHe.df <- data.frame(termHomeEquity/1000000)  # plot terminal net worth in $M
  colnames(tpvHe.df) <- "THE"
  
  print(
    ggplot(data=tpvHe.df, aes(tpvHe.df$THE))  +
      geom_histogram(
        col="blue",
        fill="blue",
        alpha = .2) +
      labs(x="Terminal Home Equity ($M)", y="Number of Scenarios")  +
      ggtitle("Figure 8b. Histogram of Terminal Home Equity") 
    
  ) 
}

###################################################
#  Analysis of scenarios with unmet spending demand
###################################################

if (yrsUnmetSpend > 0) {      # skip all analysis if there are no scenarios in which spending demand was not met foe all years
  
  if (plotGen){ ea.df <- data.frame(equityAllocation[rowSums(totalSpending) - rowSums(expenses) < 0]*100)
  colnames(ea.df) <- "Equity"
    print(
      ggplot(data=ea.df, aes(ea.df$Equity)) + 
        geom_histogram(binwidth=5, 
                       col="blue", 
                       fill="blue", 
                       alpha = .2) + 
        labs(x="Equity Allocation (%)", y="Count")  +
        ggtitle(paste("Figure 7. Histogram of Equity Allocations for\n",sum(rowSums(totalSpending) - rowSums(expenses) < 0),"Underfunded Scenarios")) +
        scale_x_discrete(limit = seq(0,100,10))
    )
  } 
  #  Compare first year of retirement spending to depeleted portfolios
  
  spendLevels <- sort(unique(initspend))   # build vector of unique levels od portfolio spending

  # for (i in 1:length(spendLevels)) {fails[i] <- sum(initspend == spendLevels[i] & rowSums((round(totalSpending-expenses))) != 0)}
  # pcFails <- fails/sum(states[states >= 1 & states < 4])
  # 
  # pcFails <- pcFails[ 1:(length(spendLevels))]
  # plot(spendLevels,pcFails*100,main="Unmet Years of Spending vs. \nInitial Spending Level",xlab="Initial Annual Spending",ylab="Percent of All Survived Years",ylim=c(0,.05))
  # 
  
  if (plotGen) {
    # Plot spending levels for unmet spending and met-spending scenarios
    hist(initspend[rowSums(round(totalSpending-expenses)) != 0],plot=FALSE,breaks=unique(initspend)) -> h # do a histogram of y and assign its info to h
    h$counts <- cumsum(h$counts)/sum(h$counts) # replace the cell freq.s by cumulative freq.s
    # sum(h$counts)
    dd <- h$mids
    d2 <- h$counts
    spendData <- data.frame(dd,d2)
    ggplot(data=spendData, aes(dd,d2)) + 
      geom_bar(stat="identity", position="dodge",
               col="blue", 
               fill="blue", 
               alpha = .2) + 
      labs(x="Spending First Year of Retirement", y="Cumulative Percent of Underfunded Years")  +
      scale_y_continuous(labels=scales::percent) +
      ggtitle(paste("Cumulative Histogram of Underfunded Years\nby First-Year Spending"))  #+
    
    
  }
    
  survivorYears <- rowSums(states == 1 | states == 2) + 2 * rowSums(states == 3) # calculate total "survivor-years"
  
  if (plotGen) hist(survivorYears[rowSums((round(totalSpending-expenses)) != 0)],xlab = "Years Survived",main = paste("Histogram of Combined Years Survived\nfor ",scenUnmetSpend," Unmet Spending Scenarios (",round(pcScenUnmet,2),"%)"),breaks=101)
   
  totUnmetSpndYrs <- rowSums(round(totalSpending - expenses) != 0)
  unmetSpendIndex <- which(rowSums((round(totalSpending-expenses))) != 0)
  
  # cat("\n\nMean Unmet Spending Years by Combined Lifetimes")
  for (i in seq(80,30,-10)){
    
    a <- mean(totUnmetSpndYrs[survivorYears <= i & survivorYears > i - 10 ])
  #  cat("\nCombined Survived Years ",i - 10," to ",i," mean unmet spending years = ",a," years.")
  }
  
  a <- rep(0,11)
  b <- a
  c <- a
 # cat("\n\nMean Unmet Spending Years per Equity Allocation")
  j <- 1
  for (i in seq(0,1,0.1)){
    a[j] <- sum(totUnmetSpndYrs[round(equityAllocation,1) == round(i,1)]) # unmet years
    b[j] <- a[j] / sum(round(equityAllocation,1) == round(i,1) & rowSums(spendLessExp) < 0) # mean years per scenario
    c[j] <- sum(round(equityAllocation,1) == round(i,1) & rowSums(spendLessExp) < 0)   # unmet scenarios
   # cat("\nEquity Allocation ",i," unmet spending years = ",a[j],"years. Unmet spending scenarios= ",c[j]," mean years per scenario= ",b[j])
    j <- j + 1
  }
 # cat("\n\nTotal unmet spending scenarios ",sum(c)," (",100*sum(c)/n,"%). Total unmet years ",sum(a))
  d <- seq(0,1,.1)
  
  
  a2 <- rep(0,11)
  b2 <- a2
  c2 <- a2
  a3 <- a2
  c3 <- c2
  j <- 1
  for (i in seq(0,1,0.1)){
    a2[j] <- sum(totUnmetSpndYrs[round(annuityAllocation,1) == round(i,1)]) # unmet years
    # spendLessExp <- round(totalSpending-expenses)
    b2[j] <- a2[j] / sum(round(annuityAllocation,1) == round(i,1) & rowSums(spendLessExp) < 0) # mean years per scenario
    c2[j] <- sum(round(annuityAllocation,1) == round(i,1) & rowSums(spendLessExp) < 0)   # unmet scenarios
    # cat("\nEquity Allocation ",i," unmet spending years = ",a[j],"years. Unmet spending scenarios= ",c[j]," mean years per scenario= ",b[j])
    j <- j + 1
  }
  # cat("\n\nTotal unmet spending scenarios ",sum(c)," (",100*sum(c)/n,"%). Total unmet years ",sum(a))
  # d <- seq(0,1,.1)
  if (plotGen) plot(d,c2/n*100,main=paste("Percent of Scenarios with Unmet Spending\nby Annuity Allocation"),xlab="Annuity Allocation",ylab="% Scenarios with Unmet Spending")
  
  if (plotGen) plot(d,a,main=paste("Number of Unmet Spending Years\nby Equity Allocation"),xlab="Equity Allocation",ylab="Unmet Spending Years")
  if (plotGen) plot(d,a,main=paste("Number of Unmet Spending Years\nby Combined Lifetimes"),xlab="Combined Lifetimes",ylab="Unmet Spending Years")
  
  if (plotGen) { 
  unmetEa <- equityAllocation[unSpendingYrs > 0] # vector if the equity allocation for every failed scenario
  underfundedyears <- unSpendingYrs[unSpendingYrs > 0] # vector of number of years failed for each failed scenario
  adata <- data.frame(underfundedyears,unmetEa)
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
  }
  
  if (plotGen) plot(d,c/n*100,main=paste("Percent of Scenarios with Unmet Spending\nby Equity Allocation"),xlab="Equity Allocation",ylab="% Scenarios with Unmet Spending")
  
  j <- 1
  for (i in seq(0,1,0.1)){
    a3[j] <- sum(totUnmetSpndYrs[round(annuityAllocation,1) == round(i,1)]) # unmet years
    # spendLessExp <- round(totalSpending-expenses)
    c3[j] <- mean(termNetWorth[round(annuityAllocation,1) == round(i,1)])
    # cat("\nEquity Allocation ",i," unmet spending years = ",a[j],"years. Unmet spending scenarios= ",c[j]," mean years per scenario= ",b[j])
    j <- j + 1
  }
  if (plotGen) plot(d,c3,main=paste("Mean Terminal Net Worth\nby Annuity Allocation"),xlab="Annuity Allocation",ylab="Terminal Net Worth")
  

  #### combinedYears alread in survivorYears  
# combinedYears <- which(states == max(states), arr.ind = TRUE)   # create vector of combined years (husband and wife) survived for each scenario
# combinedYears <- combinedYears[ order(combinedYears[,1]), ]
# combinedYears <- combinedYears[,2] - 1
# hist(unmetByCombAge[,2],breaks = 50,main = "Histogram of Unmet Spending Scenarios\nby Years of Household Retirement",xlab = "Combined Years of Retirement")

yearStates <- states[unmetSpendIndex,]     # count the number of combined spouse-years in retirement
yearStates[yearStates == 4] <- 0
yearStates[yearStates == 2] <- 1
yearStates[yearStates == 3] <- 2
if (plotGen) hist(rowSums(yearStates),breaks = 50,main = "Histogram of Unmet Spending Scenarios\nby Combined Husband and Wife\nYears of Retirement",xlab = "Combined Years of Retirement")

####################


if (plotGen) ea.df <- data.frame(equityAllocation*100)
colnames(ea.df) <- "Equity"
if (plotGen) {
  print(
    ggplot(data=ea.df, aes(ea.df$Equity)) + 
      geom_histogram(binwidth=10, 
                     col="blue", 
                     fill="blue", 
                     alpha = .2) + 
      labs(title="Histogram for Age") +
      labs(x="Equity Allocation (%)", y="Count")  +
      ggtitle(paste("Histogram of Equity Allocations for\n",n,"Scenarios")) +
      scale_x_discrete(limit = seq(0,100,10))
  )
}

aA <- data.frame(annuityAllocation)
if (plotGen) ggplot(data=aA, aes(aA$annuityAllocation)) +
  geom_histogram(binwidth=.05,fill="red") +
  labs(title=paste("Histogram of Annuity Allocations\nfor",n,"Scenarios"),x="Allocation of Initial Portfolio Balance to Annuity Purchase") +
  scale_x_continuous(labels = scales::percent)

aAnot <- data.frame(annuityAllocation[unmetSpendIndex])
if (plotGen) ggplot(data=aAnot, aes(aAnot$annuityAllocation.unmetSpendIndex)) +
  geom_histogram(binwidth=.05,fill="red") +
  labs(title=paste("Histogram of Annuity Allocations\nfor",length(unmetSpendIndex),"Underfunded Scenarios"),x="Allocation of Initial Portfolio Balance to Annuity Purchase") +
  scale_x_continuous(labels = scales::percent)
  
if (plotGen) 
  plotData.df <- data.frame(percentFunded[percentFunded < 1] * 100)
 
pdata <- ggplot (data=plotData.df,aes(plotData.df$percentFunded.percentFunded...1.)) +
  geom_histogram(binwidth = 1,
                 col="blue", 
                 fill="blue", 
                 alpha = .2) +
  
  xlab("Percent of Years Funded in Underfunded Scenarios") + ylab("Number of Scenarios") +
  ggtitle("Figure 9. Histogram of Percent of Years Funded\nfor Underfunded Scenarios")  

print(pdata + annotate("text", x = 55, y =max(ggplot_build(pdata)$data[[1]]$count)*.85, label = "Rightmost columns are number of underfunded\nscenarios that were mostly funded.\nLeftmost columns were least-funded scenarios.",size=3) )

if (plotGen) plot(equityAllocation,unSpendingYrs,main="Underfunded Years by Equity Allocation",xlab="Equity Allocation",ylab="Number of Underfunded Years")
if (plotGen) plot(annuityAllocation,unSpendingYrs,main="Underfunded Years by Annuity Allocation",xlab="Annuity Allocation",ylab="Number of Underfunded Years")

if (plotGen) {
ea.df <- data.frame(equityAllocation[unSpendingYrs]*100)
colnames(ea.df) <- "Equity"

  print(
    ggplot(data=ea.df, aes(ea.df$Equity)) + 
      geom_histogram(binwidth=5, 
                     col="blue", 
                     fill="blue", 
                     alpha = .2) + 
      labs(title="Histogram for Age") +
      labs(x="Equity Allocation (%)", y="Count")  +
      ggtitle(paste("Histogram of Equity Allocations for\n",sum(unSpendingYrs),"Underfunded Years")) +
      scale_x_discrete(limit = seq(0,100,10))
  )
}

if (plotGen) ea.df <- data.frame(equityAllocation[rowSums(totalSpending) - rowSums(expenses) < 0]*100)
colnames(ea.df) <- "Equity"
if (plotGen) {
  print(
    ggplot(data=ea.df, aes(ea.df$Equity)) + 
      geom_histogram(binwidth=5, 
                     col="blue", 
                     fill="blue", 
                     alpha = .2) + 
      labs(x="Equity Allocation (%)", y="Count")  +
      ggtitle(paste("Histogram of Equity Allocations for\n",sum(rowSums(totalSpending) - rowSums(expenses) < 0),"Underfunded Scenarios")) +
      scale_x_discrete(limit = seq(0,100,10))
  )
}


} else {
  cat ("\nNo scenarios with unmet spending. Unmet spending analysis skipped.")
}

if (plotGen) {
ggplot(data=spendData, aes(dd,d2)) + 
  geom_bar(stat="identity",
                 col="blue", 
                 fill="blue", 
                 alpha = .2) + 
  labs(x="Spending First Year of Retirement", y="Cumulative Percent of Underfunded Years")  +
  scale_y_continuous(labels=scales::percent) +
  ggtitle(paste("Cumulative Histogram of Underfunded Years\nby First-Year Spending"))  #+
}

