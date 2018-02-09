# Main RISMAT Script

cat("\014")  # clear the console

# install required packages if not already installed
packages <- c("ggplot2", "scales", "reshape2")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

### not loading scales package

source('~/R Projects/RetirementModel/MortalityStatesTables.R')
source('~/R Projects/RetirementModel/returnsRISMAT.R')
source('~/R Projects/RetirementModel/iFixedAnnuity.R')
source('~/R Projects/RetirementModel/RetirementPkg.R')
source('~/R Projects/RetirementModel/Social Security.R')
source('~/R Projects/RetirementModel/BuildSpendM.R')
source('~/R Projects/RetirementModel/Summarize.R')

set.seed(27514)    # make random number generation repeatable every time script is run
cat("\014")   # clear console
dev.list()    #clear all plots

printSummary <- TRUE  # default is TRUE, turns on summary print to console
plotGen <- TRUE      # generate plots if TRUE

##########################################
#   Initial parameters
##########################################

n <- 10000            # number of scenarios to run
wifeAge <- 62 # 67                   # wife current age
husbandAge <- 62 # 65

# Set Social Security parameters

wifeClaimAge <- 62              # age wife will claim Social Security benefits
husbandClaimAge <-  66          # age husband will claim Social Security benefit
husbandHE <- TRUE               # TRUE if husband has the higher Social Security benefit
loEarnerOwnBenefit <- 12*1942   # expected SS benefit for lower earner
hiEarnerOwnBenefit <- 12*3037   # expected SS benefit for higher earner

# set portfolio spending parameters

initportfolio <- 3940000        # initial portfolio balance
saveInitPort <- initportfolio
equityAllocation <- rep(0,n)  # create a vector equity allocation for each scenario
equityAllocation <- sample(seq(0,1,.1),n,replace=TRUE)   # .3,.5,.05 randomize equity allocations from zer0 to 100%

# Set market and inflation parameters

inflation <- .02                # assign annual average rate of inflation
inflationSd <- .01              # set inflation rate annual std dev
rf <- .01                       # risk-free real return rate
rp <- .0425                     # set equity risk premium
mrSd <- .12                     # std dev of annual market returns

# Set annuity parameters

annuityStartAge <- 66           # age of annuitant when payments will begin
annPmt <- 800 * 12               # quote for annual payment before any deaths
inflationProt <- FALSE          # annuity is inflation protected
ownsAnnuity <- 1                # = 1 if husband is annuitant and wife is sur
annuityAllocation <- sample(seq(0,.4,.1),n,replace=TRUE)   # randomize annuity allocations from zero to 100%
annuityPayout <- .0536    #payout of fixed annuity, male age 65 joint female age 65
annuityPayoutS <- ((annuityAllocation * initportfolio) * annuityPayout)  + annPmt
initportfolio <- initportfolio * (1- annuityAllocation)
survBenefit <- 0.5     # percent annuity survivor benefit

# Set spending parameters

initialSpend <- 240000 # 250000 # 230000            # expected spending year one of retirement
survivorExpense <- .63        # % expense decline after death of first spouse. Kotlikoff: two can live as cheaply as 1.6
annualAdjust <- 0 # -0.015      # expenses typically decline 1.5% annually throughout retirement

# Set HECM line of credit and home equity parameters

homeMarketVal <- 1200000    # current market value of primary residence
housingAppreciation <- .01  # annual real rate of residential housing appreciation
liborMean <- .02            # mean long term return for 1-yr Libor
liborSigma <- .01    # standard deviation long term return for 1-yr Libor
initLoc <- 0 # 640000   # HECM line of credit's initial credit limit
initMort <- 0 #333      # HECM line of credit's initial mortgage balance at closing
maxRate <- .10336    # HECM line of credit's maximum lifetime interest rate
marginHECM <- .03    # HECM line of credit's maring added to Libor Index for variable rate loan
percentMIP <- .0125  # HECM line of credit's Monthly Insurance Premium percentage

#
################### End initial parameter settings ###################################################################

#############################################
# Randomize spending +/- 10% of initialSpend
#############################################

initspend <- rep (0,n)
initspend <- sample(seq(.9,1,0.01),n,replace=TRUE) * initialSpend    # create a random uniform spending vector around initialSpend
initspend <- round(initspend)

##########################################
# Build mortality and state tables
##########################################
# 0 = neither spouse alive, 1=husband alive, 2= only wife alive, 3 - both spouses live, 4= last surviving spoude died previous yeart.

states <- mortalityStates(n,husbandAge,wifeAge,plotMort=TRUE)

totalSurvivedYears <- sum(states[states >0 & states < 4])  # number of combined years of life for both spouses all scenarios
cat("\nNumber of combined years of life for both spouses in all scenarios",totalSurvivedYears)
survived <- which(states==4,arr.ind=TRUE)   # build column vector of number of years in each scenario with at least one survivor
survived <- survived[order(survived[,1]),]    # order by scenario number to match other data structures
survived <- survived[,2] 

# get number of scenarios and years
nscen <- dim(states)[1]
nyrs <- dim(states)[2]


##########################################
# Build a matrix of home equity
##########################################

##########################################
# Build a matrix of Libor 1-year rates
##########################################

liborM <- matrix(rnorm(nscen*nyrs,.02,.01),nscen,nyrs)

##########################################
# Build inflation and market return tables
##########################################

mkt <- returnsRISMAT(states,inflation, inflationSd,rf,rp,mrSd,plot = TRUE, printSummary) # build inflation and market returns matrices
market.csM <- t(matrix(unlist(mkt[1]), byrow=TRUE, nrow=nyrs ))
market.cumCsM <- t(matrix(unlist(mkt[2]), byrow=TRUE, nrow=nyrs) )
market.rmsM <- t(matrix(unlist(mkt[3]), byrow=TRUE, nrow=nyrs ))
market.cumRmsM <- t(matrix(unlist(mkt[
  4]), byrow=TRUE, nrow=nyrs ))
market.pvsM <- t(matrix(unlist(mkt[5]), byrow=TRUE, nrow=nyrs ))
market.ppcsM <- t(matrix(unlist(mkt[6]), byrow=TRUE, nrow=nyrs ))

# Calculate portfolio returns using stock market returns and asset allocation
portfolioReturns <- equityAllocation * (market.rmsM-1) + ( 1 - equityAllocation) * liborM

##########################################
# Build matrix of annuity incomes
##########################################

annuityIncomeM <- states 
cat("\nBuilding Annuity Income table")
annuityIncomeM <- states
annuityIncomeM[states == 4] <- 0  # no one alive, no annuity income
annuityIncomeM[states == 1 | states == 2] <- survBenefit  # one survivor, decrease payout by survBenefit %
annuityIncomeM[states == 3] <- 1  # both alive, 100% annuity income
annuityIncomeM <- annuityPayoutS * annuityIncomeM  # calculate annuity income by multiplying payout % times 100% payout amount

if (inflationProt == FALSE)
  annuityIncomeM <- annuityIncomeM/market.cumCsM # adjust future payments for inflation


##########################################
# Build a Social Security Income Matrix
##########################################

socSecM <- socialsecurity (wifeAge,husbandAge,wifeClaimAge,husbandClaimAge,husbandHE,states,loEarnerOwnBenefit,hiEarnerOwnBenefit,printSummary) 
write.csv(socSecM,"~/desktop/SocSecM.csv")

#############################################
# Build spending matrix
#############################################

cat("\nBuilding spending matrix.")

spendM <- buildSpendM (states, initspend, annualAdjust,survivorExpense)
expenses <- spendM  # spendM will be modified later. Expenses retains original spending demands
spendFromPortfolio <- spendM - socSecM - annuityIncomeM

##########################################
# Build a matrix of Libor 1-year rates
##########################################

liborM <- matrix(rnorm(nscen*nyrs,.02,.01),nscen,nyrs)

############################################################################
# Build Sustainable withdrawal rates matrix with HECM line of credit reserve
############################################################################

  # Bengen is a wrapper function for the C++ function loopYearsS. Bengen creates a 
  # matrix the same size as growthRates by determining the number of rows and columns
  # in growthRates. It gets around problems passing a 2D array to and from a C++ function
  # by converting the 2D function to a vector, allowing loopYears to translate the 
  # row and column index to a vector index, and then converting the returned vector of calculated 
  # annual balances back to a matrix the same size as growthRates. If sum=TRUE (default), Bengen 
  # will print a summary of the parameters passed to it.
  
  # ALL CALCULATED VALUES ARE END OF YEAR (EOY).
  #
  # Instead of calculating spending as a percentage the previous year's ending portfolio balance,
  # bengenspendFromPortfolio uses spending passed in the matrix spendFromPortfolio. spendFromPortfolio is identically sized
  # to the dimensions of growthRates (n scenarios (rows) n years per scenario (columns).
  # spendFromPortfolio spending should be net of Social Security, pension and annuity income.
  
  # initLoc = the initial HECM line of credit amount
  # loc = matrix of the annual maximum line of credit, which grows annually with a HECM
  # locBal = matrix of the balance of HECM line of credit borrowed to date. Available credit is max line of credit less outsanding balance of the mortgage
  # liborM = matrix of simulated Libir one-year interest rates (risk-free rate)
  # marginHECM = HECM loan margin percentage added to Libor index to determine variable interest rate
  # percentMIP = HECM mortgage percent charge for mortgage insurance (also added to interest rate)
  # initMort = HECM mortgage initial balance at loan closing
  
  years <- nyrs
  scenarios <- nscen
  
  
  # initialize matrices to same size as states matrix. create a new year zero (column 1) with initial state data
  portfolioBalance <- matrix(0,scenarios,years + 1)   # initialize investment portfolio balance matrix to zeros, add a year zero
  mortgageBalance <- matrix(0,scenarios,years + 1)             # initialize mortgage balance matrix to zeros, add a year zero
  creditLimit <- portfolioBalance                 # initialize HECM credit limit matrix to zeroes, add a year zero
  creditLimit[,1] <- rep(initLoc,scenarios)
  hecmSpend <- portfolioBalance                   # initialize HECM spend matrix to zeroes, add a year zero
  hecmSpend[,1] <- 0
  spendPort <- matrix(0,scenarios,years + 1)
  spendPort <- cbind(spendPort[,1],spendFromPortfolio)
  expenses <- cbind(c(0),expenses)
  socSecM <- cbind(c(0),socSecM)
  annuityIncomeM  <- cbind(c(0),annuityIncomeM)
  portfolioBalance[,1] <- initportfolio        # set year zero portfolio balance
  marketVector <- 1 + ((market.rmsM - 1) * equityAllocation + liborM * ( 1 - equityAllocation)) # calculate growth rate of weighted portfolio
  marketVector <- cbind(c(1),marketVector)  # set year zero (column 1) of market returns equal to one
  homeEquity <- matrix(0,scenarios,years) # initialize home equity matrix
  homeEquity <- cbind(rep(homeMarketVal,n),homeEquity)
  liborM <- cbind(c(0),liborM) # add year zero column to Libor matrix
 
  
  # Set HECM interest rates
  rateHECM <- liborM + marginHECM + percentMIP
  rateHECM[rateHECM > maxRate] <- maxRate   # make sure no HECM rates exceed lifetime max
  tappedHECM <- rep(FALSE,scenarios)  # Note when HECM LOC first tapped
  depletedHECM <- rep(FALSE,scenarios) # Note when HECM LOC depleted
  depletedPort <- rep(FALSE,scenarios) # Note when portfolio depleted
  termHomeEquity <- rep(0,scenarios) # save terminal Home Equity in this vector
  termPortValue <- rep(0,scenarios) # save terminal Portfolio Value (TPV) in this vector
  unmetSpending <- rep(FALSE,scenarios) # save unmet spending scenario first year
  
  if (printSummary == TRUE) {
    cat(paste("\nArithmetic Mean of Simulated Annual Portfolio Returns= ",sprintf("%.3f%%", 100*(mean(marketVector)-1)),sep=" "))
    cat(paste("\nStandard Deviation of Simulated Annual Portfolio Returns= ",sprintf("%.3f%%", 100*(sd(marketVector)),sep=" ")))
  }
  
  

  start.time <- Sys.time()  # start timing the looped SWR code

 
  
    for (yearOfRetirement in 2:(years + 1) ) {    # note: first column contains year zero (inital) data
    
      thisYear <- yearOfRetirement    # make indexes more readable
      lastYear <- thisYear - 1
        
        # adjust portfolio spending amount (might be modified by portfolioBalance calculation)
        spendPort[,thisYear] <- pmin(portfolioBalance[,lastYear] - annuityIncomeM[,thisYear] - expenses[,thisYear] - socSecM[,thisYear])
        # set portfolioDemand <- expenses - Social Sec Income - Annuity Income, spending matrix alread net of those two when passed
        portfolioBalance[,thisYear] <- pmax(0, portfolioBalance[,lastYear] - (pmin(portfolioBalance[,lastYear],spendPort[,thisYear])))
        # add market returns to portfolioBalance
        portfolioBalance[,thisYear] <- portfolioBalance[,thisYear] * marketVector[,thisYear]      
        #     set each element of hecmSpend vector for which expenses - SocSec - annuity Income > portfolioBalance
        thisSpend <- expenses[,thisYear] - socSecM[,thisYear] - annuityIncomeM[,thisYear]  # calculate this year's non-portfolio spending
        hecmSpend[thisSpend > portfolioBalance[,thisYear] ,thisYear] <- thisSpend[thisSpend > portfolioBalance[,thisYear]] - spendPort[thisSpend > portfolioBalance[,thisYear],thisYear]
        hecmSpend[,thisYear] <- pmin(hecmSpend[,thisYear], creditLimit[,lastYear])
        # set unmetSpending to TRUE if hecm spending non-zero this year
        unmetSpending[hecmSpend[,thisYear] > 0] <- TRUE
        
        creditLimit[,thisYear] <- pmax(0, creditLimit[,lastYear] - mortgageBalance[,thisYear] - hecmSpend[,thisYear])
        creditLimit[,thisYear] <-  creditLimit[,thisYear] * (1 + rateHECM[,thisYear])
        
        # increase mortgage balance by hecm spend
        mortgageBalance[,thisYear] <- mortgageBalance[,lastYear] + hecmSpend[,thisYear]
        # add HECM mortgage interest
        mortgageBalance[,thisYear] <- mortgageBalance[,thisYear] * (1 + rateHECM[,thisYear])
        
        # adjust home equity
        homeEquity[,thisYear] <- homeEquity[,lastYear] * (1 + housingAppreciation) - mortgageBalance[,thisYear]
  
  }    # end yearOfRetirement for-loop
      
      termPortValue <- portfolioBalance[,lengthRetirement]
      depletedPort[termPortValue == 0] <- TRUE
      termHomeEquity <- homeEquity[,lengthRetirement]
      depletedHECM[creditLimit[,lengthRetirement] == 0] <- TRUE
      
  # set all matrix values to zero if state means no one alive (0 or 4)
  clearMatrix = cbind(c(1),states)
  clearMatrix[clearMatrix==4] <- 0
  clearMatrix[clearMatrix >= 1 & clearMatrix <= 3] <- 1
  spendPort <- spendPort * clearMatrix
  marketVector <- marketVector * clearMatrix
  homeEquity <- homeEquity * clearMatrix
  creditLimit <- creditLimit * clearMatrix
 
  end.time <- Sys.time()
  time.taken <- end.time - start.time  # end timing the relevant SWR loop code
  cat("\n*** Time spend in SWR loop code= ",time.taken)
  
  hecmSpend <- round(hecmSpend)
  portfolioBalance <- round(portfolioBalance)
  termHomeEquity <- round(termHomeEquity)
  termNetWorth <- round(termNetWorth)
  termPortValue <- round(termPortValue)


# swrM <- portfolioBalance      # swr Portfolio results matrix
# locHECM <- creditLimit        # HECM line of credit matrix
# 
# portfolioBalance[states == 0 | states == 4] <- 0      # zero out all portfolio balances when no clients alive
# mortgageBalance[states == 0 | states == 4] <- 0      # zero out all mortgage balances when no clients alive
# 
# # remove year zero columns (matrix column 1)
# 
# spendFromPortfolio <- spendPort[,-1]  # Adjust spendFromPortfolio after bengenSpendHECM runs
# swrM <- swrM[,-1]     # eliminate year zero columns
# socSecM <- socSecM[,-1]
# annuityIncomeM <- annuityIncomeM[,-1]
# marketVector <- marketVector[,-1]
# expenses <- expenses[,-1]
# hecmSpend <- hecmSpend[,-1]
# locHECM <- locHECM[,-1]
# liborM <- liborM[,-1]
# rateHECM <- rateHECM[,-1]
# homeEquity <- homeEquity[,-1]
# 
# mortgageBalance <- mortgageBalance[,-1]   # HECM Reverse Mortgage balance matrix
# portfolioBalance <- portfolioBalance[,-1]   # HECM Reverse Mortgage balance matrix
# 
# totalSpending <- spendFromPortfolio + socSecM + annuityIncomeM + hecmSpend 
# 
# networth <- swrM + max(0,homeEquity)   # home equity can go negative with HECM LOC, but negative mortgage is non-recourse (0)
# termNetWorth <- pmax(0,termHomeEquity) + termPortValue # terminal net worth = terminal home equity plus total portfolio balance. Home Equity is non-recourse and can't go below zero.
# 
# cat ("\nUnmet spending scenarios = ",sum(rowSums(totalSpending-expenses) < 0))
# 
# # Adjust spendM when portfolio is depleted
# swrMb4 <- matrix(0,length(swrM[,1]),length(swrM[1,]))  # init swrMb4 to matrix of zeroes same dims as swrM
# swrMb4 <- cbind(initportfolio,swrM[,1:(nyrs-1)]) # build matrix like swrM but each row equals the previous year's balance
# 
# # calculate terminal portfolio values into vector "tpvs"
# 
# tpvStates <- states
# tpvStates[tpvStates == 3] <- 0
# tpvStates[tpvStates == 2] <- 0
# tpvStates[tpvStates == 1] <- 0
# tpvStates[tpvStates == 4] <- 1
# 
# tpvCalc <- tpvStates*(homeEquity + swrM)  # zero everything except the portfolio value for state=4
# tpvs <- rowSums(tpvCalc)
# 
# ### need to correct to choose TPV the year before state is 4. ###############
# 
# # set all portfolio balances to zero starting state-4, last survivor died last year
# 
# tpvStates <- states
# tpvStates[tpvStates == 3] <- 1
# tpvStates[tpvStates == 2] <- 1
# tpvStates[tpvStates == 1] <- 1
# tpvStates[tpvStates == 4] <- 0
# swrM <- tpvStates*swrM
# 
# 
# #############################################
# # Plot histogram of home equity at death
# #############################################
# 
# # ignore terminal portfolio values greater that 4 time initial portfolio value and highly unlikely and plot histogram
# # hist(tpvs[tpvs < 4*initportfolio],breaks=100,main="Histogram of Terminal Portfolio Values\nIgnores TPVs > 4 times Initial Portfolio Balance",xlab="Terminal Portfolio Values")
# # cat("\nMedian Terminal Portfolio Value= ",median(tpvs),".")
# 
# # calculate average annual withdrawal rate
# awrM <- spendFromPortfolio / swrMb4  # build withdrawal rate matrix
# awrM[is.nan(awrM)] <- 0  # change NaN to zero where swrMb4 caused divide by zero
# awrM[is.infinite(awrM)] <- 0  # change NaN to zero where swrMb4 near zero
# nzmean <- mean(awrM[which(awrM != 0)]) # get mean of all non-zero withdrawal rates
# if (printSummary == TRUE) {
#   cat("\nMean withdrawal rate for all scenarios =  ",round(100*nzmean,2),"% per year.",sep=" ")
# }
# 
# # cat("\nNumber of scenarios with unmet spending ",scenUnmetSpend)
# # cat("\nPercent of scenarios with unmet spending ",pcScenUnmet," %")
# # cat("\nFirst ten unmet spending scenarios: ",first10unmet)
# 
# parms <- data.frame(marginHECM,percentMIP,annualAdjust,initportfolio,initLoc,initMort,survivorExpense,survBenefit,wifeClaimAge,husbandClaimAge,wifeAge,husbandAge,loEarnerOwnBenefit,hiEarnerOwnBenefit,annuityStartAge,annualAdjust,maxRate)
# write.csv(parms,"~/desktop/Parms.csv")
# 
# totUnmetSpndYrs <- rowSums(round(totalSpending - expenses) != 0)
# unmetSpendIndex <- which(rowSums((round(totalSpending-expenses))) != 0)
# 
# source('Summarize Scenario.R')
# 
