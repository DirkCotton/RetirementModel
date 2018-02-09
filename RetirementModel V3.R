# Main RISMAT Script

cat("\014")  # clear the console
# rm(homeEquity,mortgageBalance,creditLimit,unmetSpending,hecmSpend,spendPort,portfolioBalance,liborM,marketVector,market.rmsM)

# install required packages if not already installed


### not loading scales package

source('~/R Projects/RetirementModel/MortalityStatesTables.R')
source('~/R Projects/RetirementModel/returnsRISMAT.R')
source('~/R Projects/RetirementModel/iFixedAnnuity.R')
source('~/R Projects/RetirementModel/RetirementPkg.R')
source('~/R Projects/RetirementModel/Social Security.R')
source('~/R Projects/RetirementModel/BuildSpendM.R')
source('~/R Projects/RetirementModel/Summarize.R')
source('~/R Projects/RetirementModel/Plot Graphs.R')
source('~/R Projects/RetirementModel/BuildDiaM.R')
source('~/R Projects/RetirementModel/BuildSpiaM.R')
source('~/R Projects/RetirementModel/Survivors V3.R')

source('~/R Projects/RetirementModel/PensionHusband.R')
source('~/R Projects/RetirementModel/PensionWife.R')

# ssBenefits <- matrix(0,3,2)
# ssBenefits[1,1] <- ssB1   # higher earner's earliest claiming age benefit per month
# ssBenefits[2,1] <- ssB2   # higher earner's FRA claiming age benefit per month
# ssBenefits[3,1] <- ssB3   # higher earner's age 70 claiming age benefit per month
# ssBenefits[1,2] <- ssB4   # lower earner's earliest claiming age benefit per month
# ssBenefits[2,2] <- ssB5   # lower earner's FRA claiming age benefit per month
# ssBenefits[3,2] <- ssB6   # lower earner's age 70 claiming age benefit per month

colnames(ssBenefits) <- c("higherEarner","lowerEarner")
rownames(ssBenefits) <- c("Age62","FRA","Age70")
fraTable <- read.csv("FRA by Birth Year.csv",header = FALSE) # read full retirement age by year of birth table

set.seed(27514)    # make random number generation repeatable every time script is run
# cat("\014")   # clear console
# dev.list()    #clear all plots

##########################################
#   Initial parameters
##########################################

# Always set SPIA purchase age and payment start date to first year of retirement.

wifeAge <-
annuityStartAge <- min(wifeAge,husbandAge)
purchaseAgeSPIA <- annuityStartAge

# set portfolio spending parameters

equityAllocation <- rep(0,n)  # create a vector equity allocation for each scenario
equityAllocation <- sample(seq(equityLB,equityUB,.1),n,replace=TRUE)   # .3,.5,.05 randomize equity allocations from zer0 to 100%

tipsSpend <-  0  # planned spending from TIPS bond ladder

diaAllocation <- rep(0,n)
spiaAllocation <- sample(seq(spiaLB, spiaUB, 0.1),n,replace=TRUE)   # create vector of n SPIA allocations

for (i in 1:n) {
  diaAllocation[i] <- sample(seq(diaLB, min(diaUB,spiaAllocation[i]), 0.1),1,replace=TRUE) 
}

# Immediate annuity parameters

# annuityStartAge is age of annuitant when payments will begin
# annPmt is quote for annual payment before any deaths
# inflationProtSPIA is annuity is inflation protected
# spiaOwnerisHusband is = 1 if husband is annuitant and wife is sur
# spiaPayout is payout of fixed annuity, male age 65 joint female age 65
# PercentSPIA is percent SPIA annuity survivor benefit

# Deferred income annuity parameters

# purchaseAgeDIA is age at which DIA will be purchased
# diaPayout is annual payout for this diaPayout (see quotes at abaris.com)
# startAgeDIA is annuity owners age when payouts will begin
# diaOwnerisHusband is TRUE or FALSE
# survivorPercentDIA is % of payout paid to a surviving spouse
# inflationProtDIA is payouts are inflation-protected, TRUE or FALSE

# Spending parameters

# initialSpend is expected spending year one of retirement
# survivorExpense is % expense decline after death of first spouse. Kotlikoff: two can live as cheaply as 1.6
# annualAdjust is expenses typically decline 1.5% annually throughout retirement

# HECM line of credit and home equity parameters

# homeMarketVal is current market value of primary residence
# housingAppreciation is annual real rate of residential housing appreciation
# liborMean is mean long term return for 1-yr Libor
# liborSigma is standard deviation long term return for 1-yr Libor
# initLoc is HECM line of credit's initial credit limit
# initMort is HECM line of credit's initial mortgage balance at closing
# maxRate is HECM line of credit's maximum lifetime interest rate
# marginHECM is HECM line of credit's maring added to Libor Index for variable rate loan
# percentMIP is HECM line of credit's Monthly Insurance Premium percentage

################### End initial parameter settings ###################################################################

#############################################
# Randomize spending +/- 10% of initialSpend
#############################################

initspend <- rep (0,n) 
initspend <- sample(seq(spendLB,spendUB,0.01),n,replace=TRUE) * initialSpend    # create a random uniform spending vector around initialSpend
initspend <- round(initspend)

##########################################
# Build mortality and state tables
##########################################
# 0 = neither spouse alive, 1=husband alive, 2= only wife alive, 3 - both spouses live, 4= last surviving spoude died previous yeart.

states <- mortalityStates(n,husbandAge,wifeAge,plotGen=TRUE)

totalSurvivedYears <- sum(states[states >0 & states < 4])  # number of combined years of life for both spouses all scenarios
cat("\nNumber of combined years of life for both spouses in all scenarios",totalSurvivedYears)
survived <- which(states==4,arr.ind=TRUE)   # build column vector of number of years in each scenario with at least one survivor
survived <- survived[order(survived[,1]),]    # order by scenario number to match other data structures
survived <- survived[,2] - 1
lengthRetirement <- survived

# get number of scenarios and years
nscen <- dim(states)[1]
nyrs <- dim(states)[2]

initportfolio <-  rep(saveInitPort,nscen)

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

###########################################
# Build matrix of immediate annuity incomes
###########################################

# has purchase age already passed? if so, input as a pension

spiaPurchase <- rep(0,n)

  purchaseAgeSPIA <- min(wifeAge, husbandAge)    #  SPIA is always purchased first year of retirement

  spiaPurchase <- spiaAllocation * initportfolio
  initportfolio <- initportfolio - spiaPurchase  # subtract purchase amount from savings
  

annuityIncomeM <- matrix (0,nscen,nyrs)

spiaIncomeM <- buildSpiaM (spiaAllocation,spiaPayout,states,purchaseAgeSPIA,spiaOwnerisHusband,survivorPercentSPIA,inflationProtSPIA,market.cumCsM,husbandAge,wifeAge,saveInitPort) 
annuityIncomeM <- annuityIncomeM + spiaIncomeM

#################################################
# Build matrix of deferred income annuity incomes
#################################################

# has purchase age already passed?

if (husbandAge > purchaseAgeDIA){ # yes   #### fix for wife is owner
  purchaseAgeDIA <- husbandAge
} else {
  diaPurchase <- diaAllocation * initportfolio
  initportfolio <- initportfolio - diaPurchase  # subtract purchase amount from savings
}

diaIncomeM <- buildDiaM (states,diaPurchase,purchaseAgeDia,diaPayout,startAgeDIA,diaOwnerisHusband=TRUE,survivorPercentDIA,inflationProtDIA,market.cumCsM,husbandAge,wifeAge) 
annuityIncomeM <- annuityIncomeM + diaIncomeM

###########################################
# Build a Pension Income Matrix for Husband
###########################################

hPensionIncomeM <- matrix(0,nscen,nyrs)
if (husbandPensionPayout > 0) {
  hPensionIncomeM <- buildPensionHusband (husbandPensionPayout,states,husbandPensionStartAge,survivorPercentHpension,inflationProthPension,market.cumCsM,husbandAge) 
    annuityIncomeM <- annuityIncomeM + hPensionIncomeM
}

###########################################
# Build a Pension Income Matrix for Wife
###########################################

wPensionIncomeM <- matrix(0,nscen,nyrs)
if (wifePensionPayout > 0) {
  wPensionIncomeM <- buildPensionWife (wifePensionPayout,states,wifePensionStartAge,survivorPercentWpension,inflationProthPension,market.cumCsM,wifeAge) 
     annuityIncomeM <- annuityIncomeM + wPensionIncomeM
}


##########################################
# Build a matrix of reserve funds
##########################################

reserve <- 0    # set amount of reserve
reserveEquityAllocation <- .8         # set equity allocation higher for reserve fund than for spending portfolio
initportfolio <- initportfolio - reserve  # subtract reserve fund amount from savings
reserveFundM <- matrix(0,nscen,nyrs + 1)     # initialize reserve fund matrix
reserveFundM[,1] <- reserve     # set initial state for each scenario = reserved amount

###########################################
# Build TIPS Ladder Income Matrix
###########################################

tipsIncomeM <- matrix(0,nscen,nyrs)
ladderTV <- rep(0,nyrs)
tipsIncomeM <- matrix(tipsSpend,nscen,nyrs)

if (tipsSpend > 0) {
tipsIncomeM <- matrix(tipsSpend,nscen,nyrs)
tipsIncomeM[states == 4 | states == 0] <- 0   # tips Income set to zero when neither spouse alive
# calculate TIPS ladder residual market value at death with simplifying assumption that interest rates remain constant
tipsCost <- read.csv("~/R Projects/RetirementModel/TIPS Ladder Cost.csv",header=FALSE)
ladderLength <- length(tipsCost$V1)
ladderTV <- rep(0,ladderLength)   # create a terminal value vector same length as laadder cost

# proportionalize spending and TIPS ladder cost read in from Pfau example to user request TIPS ladder spend amount
tipsFirstYearPayout <- 40000 / sum(tipsCost$V1)   # from Pfau example read from .cvs file
newTipsCost <- tipsSpend / tipsFirstYearPayout
ts <- 40000/sum(tipsCost$V1)*newTipsCost
tipsCost$V1 <- tipsCost$V1 * newTipsCost / sum(tipsCost$V1)

ladderTV[ladderLength] <- 0  # ladder will be depleted in last year
# assume market value will remain the same (no interest rate changes) so market value year n <- market value year n-1
for (termYear in 1:(ladderLength-1)) {
  ladderTV[termYear] <- sum(tipsCost$V1[1: (ladderLength - termYear)])
}

initportfolio <- initportfolio - sum(tipsCost$V1)  # subtract cost of TIPS ladder from savings
tipsIncomeM[,(ladderLength + 1):nyrs] <- 0      # set all years TIPS income to zero after length of ladder

}

##########################################
# Build a Social Security Income Matrix
##########################################
 
# Randomize Soc Sec claiming age for both spouse for 62,66 (FRA) or 70.
# wifeClaimAge <- sample(c(62,66,70), n, replace = TRUE)
# husbandClaimAge <- sample(c(62,66,70), n, replace = TRUE)

husbandHE <- TRUE  
# is husband higher earner for Soc Sec benefits
if (ssBenefits[2,1] < ssBenefits[2,2]) husbandHE <- FALSE

socSecM <- socialsecurity(household,wifeAge,husbandAge,wifeDOB,husbandDOB,wifeClaimAge,husbandClaimAge,husbandHE,states,ssBenefits,printSummary) 
socSecM <- socSecM * 12

# write.csv(socSecM,"~/desktop/SocSecM.csv")

#############################################
# Build spending matrix
#############################################

cat("\nBuilding spending matrix.")

spendM <- buildSpendM (states, initspend, annualAdjust,survivorExpense,household)
saveSpendM <- spendM
expenses <- spendM  # spendM will be modified later. Expenses retains original spending demands
spendFromPortfolio <- spendM - socSecM - annuityIncomeM - tipsIncomeM
floor <- socSecM + annuityIncomeM      # total lifetime (lonegvity-hedged) safe income
# floor[,1:length(ladderTV)] <- sweep(floor[,1:length(ladderTV)],2,ladderTV,FUN="+")     # add remaining TIPS bond ladder value to floor value
  
##########################################
# Build a matrix of Libor 1-year ratesm
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
  reserveSpend <- portfolioBalance                   # initialize reserve spend matrix to zeroes, add a year zero
  reserveSpend[,1] <- 0
  spendPort <- matrix(0,scenarios,years + 1)
  spendPort <- cbind(spendPort[,1],spendFromPortfolio)
  expenses <- cbind(c(0),expenses)
  socSecM <- cbind(c(0),socSecM)
  annuityIncomeM  <- cbind(c(0),annuityIncomeM)
  portfolioBalance[,1] <- initportfolio        # set year zero portfolio balance
  marketVector <- 1 + ((market.rmsM - 1) * equityAllocation + liborM * ( 1 - equityAllocation)) # calculate growth rate of weighted portfolio
  marketVector <- cbind(c(1),marketVector)  # set year zero (column 1) of market returns equal to one
  reserveMarketVector <- 1 + ((market.rmsM - 1) * reserveEquityAllocation + liborM * ( 1 -reserveEquityAllocation)) # calculate growth rate of weighted portfolio
  reserveMarketVector <- cbind(c(1),reserveMarketVector)  # set year zero (column 1) of market returns equal to one
  
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
  reserveTPV <- rep(0,scenarios) # save reserve fund terminal Portfolio Value (TPV) in this vector
  unmetSpending <- rep(FALSE,scenarios) # save unmet spending scenario first year
  
  if (printSummary == TRUE) {
    cat(paste("\nArithmetic Mean of Simulated Market Returns= ",sprintf("%.3f%%", 100*(mean(market.rmsM)-1)),sep=" "))
    cat(paste("\nStandard Deviation of Simulated Annual Market Returns Returns= ",sprintf("%.3f%%", 100*(sd(market.rmsM)),sep=" ")))
    cat(paste("\nArithmetic Mean of Simulated Annual Portfolio Returns= ",sprintf("%.3f%%", 100*(mean(marketVector)-1)),sep=" "))
    cat(paste("\nStandard Deviation of Simulated Annual Portfolio Returns= ",sprintf("%.3f%%", 100*(sd(marketVector)),sep=" ")))
  }
  
  for (yearOfRetirement in 2:(years + 1) ) {    # note: first column contains year zero (inital) data
    
    for (scenarioNumber in 1:scenarios) {
      
      if (states[scenarioNumber,yearOfRetirement-1] == 4) {   # surviving spouse died last year
        termPortValue[scenarioNumber] <- portfolioBalance[scenarioNumber,yearOfRetirement - 1]  # set TPV for this scenario = last years' portfolioBalance
        if (termPortValue[scenarioNumber] == 0) depletedPort[scenarioNumber] <- TRUE  # Note portfolio depleted in this scenario
        if (creditLimit[scenarioNumber,yearOfRetirement-1] == 0 & initLoc > 0) depletedHECM[scenarioNumber] <- TRUE
        termHomeEquity[scenarioNumber] <- homeEquity[scenarioNumber,yearOfRetirement - 1] - mortgageBalance[scenarioNumber,yearOfRetirement - 1]
        termNetWorth[scenarioNumber] <- termHomeEquity[scenarioNumber] + termPortValue[scenarioNumber] + ladderTV[scenarioNumber] + reserveTPV[scenarioNumber]
        reserveTPV[scenarioNumber] <- reserveFundM[scenarioNumber,yearOfRetirement - 1]  # set TPV of reserve fund for this scenario = last years' teserve fund Balance
        
      } else {
        
        # Pay this year's expenses from safe income (Social Security, pensions, etc.). If safe sources don't 
        # cover all expenses, then spend from the savings portfolio to cover the difference.
        
        # adjust portfolio spending amount (might be modified by portfolioBalance calculation)
        # spendPort[scenarioNumber,yearOfRetirement] <- min(portfolioBalance[scenarioNumber,(yearOfRetirement - 1 )],expenses[scenarioNumber,yearOfRetirement] - annuityIncomeM[scenarioNumber,yearOfRetirement] - socSecM[scenarioNumber,yearOfRetirement] )
        spendPort[scenarioNumber,yearOfRetirement] <- expenses[scenarioNumber,yearOfRetirement] - annuityIncomeM[scenarioNumber,yearOfRetirement] - socSecM[scenarioNumber,yearOfRetirement] 
        
        if (spendPort[scenarioNumber,yearOfRetirement] <= portfolioBalance[scenarioNumber,(yearOfRetirement - 1)]) {     # if there is enough left in the portfolio after last year, then spend from it
          portfolioBalance[scenarioNumber,(yearOfRetirement)] <- portfolioBalance[scenarioNumber,(yearOfRetirement - 1)] - spendPort[scenarioNumber,yearOfRetirement]
        } else {         # else last year's ending portfolio balance won't cover this year's spending
          portfolioBalance[scenarioNumber,(yearOfRetirement)] <- 0      # deplete portfolio balance and . . .
          spendPort[scenarioNumber,yearOfRetirement] <- portfolioBalance[scenarioNumber,(yearOfRetirement - 1)] # . . . spend portfolio balance
        }
        
        # add market returns to portfolioBalance
        portfolioBalance[scenarioNumber,yearOfRetirement] <- portfolioBalance[scenarioNumber,yearOfRetirement] * marketVector[scenarioNumber,yearOfRetirement]
        
        # If both safe income sources and the remaining investment portfolio are inadequate to cover this year's expenses, then
        # spend from a HECM line of credit if there is one and it is noy yet depleted.
        
        #     set hecmSpend <- portfolio demand  - remaining portfolio balance
        hecmSpend[scenarioNumber,yearOfRetirement] <- expenses[scenarioNumber,yearOfRetirement] - spendPort[scenarioNumber,yearOfRetirement] - socSecM[scenarioNumber,yearOfRetirement] - annuityIncomeM[scenarioNumber,yearOfRetirement]
        hecmSpend[scenarioNumber,yearOfRetirement] <- min(hecmSpend[scenarioNumber,yearOfRetirement],creditLimit[scenarioNumber,(yearOfRetirement - 1)])                                                                                                            
        #      if (hecmSpend[scenarioNumber,yearOfRetirement] > creditLimit[scenarioNumber,(yearOfRetirement - 1)]) unmetSpending[scenarioNumber] <- TRUE
        if (hecmSpend[scenarioNumber,yearOfRetirement] > 1) tappedHECM [scenarioNumber] <- TRUE
        else {
          hecmSpend[scenarioNumber,yearOfRetirement] <- 0 
        }
        
        creditLimit[scenarioNumber,yearOfRetirement] <- max(0,creditLimit[scenarioNumber,(yearOfRetirement-1)] - mortgageBalance[scenarioNumber,(yearOfRetirement)] - hecmSpend[scenarioNumber,yearOfRetirement])     
        # if (creditLimit[scenarioNumber,yearOfRetirement] == 0 & initLoc > 0) {
        #   depletedHECM[scenarioNumber] <- TRUE
        # }
        # increase credit limit per HECM contract                                                                              
        creditLimit[scenarioNumber,yearOfRetirement] <- creditLimit[scenarioNumber,yearOfRetirement] * (1 + rateHECM[scenarioNumber,(yearOfRetirement)])
        # increase reverse mortgage balance
        mortgageBalance[scenarioNumber,(yearOfRetirement)] <- mortgageBalance[scenarioNumber,(yearOfRetirement - 1)] +  hecmSpend[scenarioNumber,yearOfRetirement] 
        # add reverse mortgage interest charge  
        mortgageBalance[scenarioNumber,(yearOfRetirement)] <- mortgageBalance[scenarioNumber,(yearOfRetirement)] * (1 + rateHECM[scenarioNumber,(yearOfRetirement)] )
        # adjust home equity matrix
        homeEquity [scenarioNumber,(yearOfRetirement)] <- homeEquity[scenarioNumber,(yearOfRetirement - 1)] * ( 1 + housingAppreciation) - mortgageBalance[scenarioNumber,(yearOfRetirement)]
        
        # If this year's expenses are still not covered, spend from a reserve fund, if there is one and it is
        # not depleted.
        
        gap <- round(expenses[scenarioNumber,yearOfRetirement] - (spendPort[scenarioNumber,yearOfRetirement] + annuityIncomeM[scenarioNumber,yearOfRetirement] + socSecM[scenarioNumber,yearOfRetirement] + hecmSpend[scenarioNumber,yearOfRetirement]) )
        if(gap > 0) {
          # expenses are not covered
          # is there enough in the reserve fund to cover the remaining shortfall?
          if (reserveFundM[scenarioNumber,yearOfRetirement - 1] > gap) {    # yes
            reserveFundM[scenarioNumber,yearOfRetirement] <- (reserveFundM[scenarioNumber,yearOfRetirement - 1] - gap) * reserveMarketVector[scenarioNumber,yearOfRetirement] # yes, spend reserves
            reserveSpend[scenarioNumber,yearOfRetirement - 1] <- gap
            
          }  else { # don't have enough in reserve fund
            reserveSpend[scenarioNumber,yearOfRetirement - 1] <- reserveFundM[scenarioNumber,yearOfRetirement - 1]
            reserveFundM[scenarioNumber,yearOfRetirement - 1] <- 0   # deplete reserve fund
            
            unmetSpending[scenarioNumber] <- TRUE # no more income sources, mark as unfunded scenario
          }
        }
        
        else { # no gap or negative gap, just grow the reserves balance
          
          reserveFundM[scenarioNumber,yearOfRetirement] <- reserveFundM[scenarioNumber,yearOfRetirement - 1] * reserveMarketVector[scenarioNumber,yearOfRetirement]
        } # end no need to spend from reserves
        
      }   # end-else when state isn't = 4
    }  # end scenarios for-loop
  }    # end yearOfRetirement for-loop

  reserveSpend <- round(reserveSpend)
  reserveFundM <- round(reserveFundM)
  
  hecmSpend <- round(hecmSpend)
  portfolioBalance <- round(portfolioBalance)
  termHomeEquity <- pmax(0,round(termHomeEquity)) # home equity can become negative if a reverse mortgage is available but it will not lower terminal net worth
  termNetWorth <- round(termNetWorth)
  termPortValue <- round(termPortValue)


swrM <- portfolioBalance      # swr Portfolio results matrix
locHECM <- creditLimit        # HECM line of credit matrix

portfolioBalance[states == 0 | states == 4] <- 0      # zero out all portfolio balances when no clients alive
mortgageBalance[states == 0 | states == 4] <- 0      # zero out all mortgage balances when no clients alive

# remove year zero columns (matrix column 1)

spendFromPortfolio <- spendPort[,-1]  # Adjust spendFromPortfolio after bengenSpendHECM runs
swrM <- swrM[,-1]     # eliminate year zero columns
socSecM <- socSecM[,-1]
annuityIncomeM <- annuityIncomeM[,-1]
marketVector <- marketVector[,-1]
expenses <- expenses[,-1]
hecmSpend <- hecmSpend[,-1]
locHECM <- locHECM[,-1]
liborM <- liborM[,-1]
rateHECM <- rateHECM[,-1]
homeEquity <- homeEquity[,-1]

mortgageBalance <- mortgageBalance[,-1]   # HECM Reverse Mortgage balance matrix
portfolioBalance <- portfolioBalance[,-1]   # HECM Reverse Mortgage balance matrix

totalSpending <- spendFromPortfolio + socSecM + annuityIncomeM + hecmSpend 
totalSpending[totalSpending > expenses] <- expenses[totalSpending > expenses]   # if totalSpending exceeds expenses then the surplus is added back to the portfolioBalance

networth <- swrM + max(0,homeEquity) + reserveFundM[,nyrs]   # home equity can go negative with HECM LOC, but negative mortgage is non-recourse (0)
networth <- sweep(networth, 2, ladderTV, "+")
termNetWorth <- pmax(0,termHomeEquity) + termPortValue # terminal net worth = terminal home equity plus total portfolio balance. Home Equity is non-recourse and can't go below zero.

cat ("\nUnmet spending scenarios = ",sum(rowSums(totalSpending-expenses) < 0))

# Adjust spendM when portfolio is depleted
swrMb4 <- matrix(0,length(swrM[,1]),length(swrM[1,]))  # init swrMb4 to matrix of zeroes same dims as swrM
swrMb4 <- cbind(initportfolio,swrM[,1:(nyrs-1)]) # build matrix like swrM but each row equals the previous year's balance

# calculate terminal portfolio values into vector "tpvs"

tpvStates <- states
tpvStates[tpvStates == 3] <- 0
tpvStates[tpvStates == 2] <- 0
tpvStates[tpvStates == 1] <- 0
tpvStates[tpvStates == 4] <- 1

tpvCalc <- tpvStates*(homeEquity + swrM)  # zero everything except the portfolio value for state=4
tpvs <- rowSums(tpvCalc)

### need to correct to choose TPV the year before state is 4. ###############

# set all portfolio balances to zero starting state-4, last survivor died last year

tpvStates <- states
tpvStates[tpvStates == 3] <- 1
tpvStates[tpvStates == 2] <- 1
tpvStates[tpvStates == 1] <- 1
tpvStates[tpvStates == 4] <- 0
swrM <- tpvStates*swrM

# calculate average annual withdrawal rate
awrM <- spendFromPortfolio / swrMb4  # build withdrawal rate matrix
awrM[is.nan(awrM)] <- 0  # change NaN to zero where swrMb4 caused divide by zero
awrM[is.infinite(awrM)] <- 0  # change NaN to zero where swrMb4 near zero
nzmean <- mean(awrM[which(awrM != 0)]) # get mean of all non-zero withdrawal rates
if (printSummary == TRUE) {
  cat("\nMean withdrawal rate for all scenarios =  ",round(100*nzmean,2),"% per year.",sep=" ")
}

parms <- data.frame(marginHECM,percentMIP,annualAdjust,initportfolio,initLoc,initMort,survivorExpense,survBenefit,wifeClaimAge,husbandClaimAge,wifeAge,husbandAge,loEarnerOwnBenefit,hiEarnerOwnBenefit,annuityStartAge,annualAdjust,maxRate)
write.csv(parms,"~/desktop/Parms.csv")

totUnmetSpndYrs <- rowSums(round(totalSpending - expenses) != 0)
unmetSpendIndex <- which(rowSums((round(totalSpending-expenses))) != 0)

source('Summarize Scenario.R')

