# Main RISMAT Script

cat("\014")  # clear the console

source('~/R Projects/RetirementModel/MortalityStatesTables.R')
source('~/R Projects/RetirementModel/returnsRISMAT.R')
source('~/R Projects/RetirementModel/iFixedAnnuity.R')
source('~/R Projects/RetirementModel/RetirementPkg.R')
source('~/R Projects/RetirementModel/Social Security.R')
source('~/R Projects/RetirementModel/BuildSpendM.R')
source('~/R Projects/RetirementModel/Summarize.R')

set.seed(27514)
cat("\014")   # clear console
dev.list()    #clear all plots

printSummary <- TRUE  # default is TRUE, turns on summary print to console

##########################################
#   Initial parameters
##########################################

n <- 10000
# scenarios to run
wifeAge <- 67                   # wife current age

# Set Social Security parameters

wifeClaimAge <- 66              # age wife will claim Social Security benefits
husbandClaimAge <-  70           # age husband will claim Social Security benefit
husbandHE <- TRUE               # TRUE if husband has the higher Sociasl Security benefit
loEarnerOwnBenefit <- 12*1942   # expected SS benefit for lower earner
hiEarnerOwnBenefit <- 12*3037   # expected SS benefit for higher earner

# set portfolio spending parameters

initportfolio <- 3940000        # initial portfolio balance
equityAllocation <- rep(0,n)  # create a vector equity allocation \for each scenario
set.seed(27514)
equityAllocation <- sample(seq(0.4,0.6,.1),n,replace=TRUE)   # randomize equity allocations from zer0 to 100%

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
annuityAllocation <- sample(seq(0,.1,.01),n,replace=TRUE)   # randomize annuity allocations from zero to 100%
annuityPayout <- .0536    #payout of fixed annuity, male age 65 joint female age 65
annuityPayoutS <- ((annuityAllocation * initportfolio) * annuityPayout)  + annPmt
initportfolio <- initportfolio * (1- annuityAllocation)
survBenefit <- 0.5     # percent annuity survivor benefit

# Set spending parameters

initialSpend <- 250000 # 250000 # 230000            # expected spending year one of retirement
survivorExpense <- .63        # % expense decline after death of first spouse. Kotlikoff: two can live as cheaply as 1.6
annualAdjust <- 0 # -0.015      # expenses typically decline 1.5% annually throughout retirement

# Set HECM line of credit parameters

homeMarketVal <- 1200000    # current market value of primary residence
liborMean <- .02            # mean long term return for 1-yr Libor
liborSigma <- .01    # standard deviation long term return for 1-yr Libor
initLoc <- 0 #640000   # HECM line of credit's initial credit limit
initMort <- 0 #333      # HECM line of credit's initial mortgage balance at closing
maxRate <- .10336    # HECM line of credit's maximum lifetime interest rate
marginHECM <- .03    # HECM line of credit's maring added to Libor Index for variable rate loan
percentMIP <- .0125  # HECM line of credit's Monthly Insurance Premium percentage

#
################### End initial parameter settings ###################################################################

##########################################
# Print primary parameters to console
##########################################

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

survivedYrs <- sum(states[states >0 & states < 4])  # number of combined years of life for both spouses all scenarios
cat("\nNumber of combined years of life for both spouses in all scenarios",survivedYrs)

# get number of scenarios and years
nscen <- dim(states)[1]
nyrs <- dim(states)[2]

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

AnnuityIncomeM <- states 
cat("\nBuilding Annuity Income table")
annuityIncomeM <- states
annuityIncomeM[states == 4] <- 0  # no one alive, no annuity income
annuityIncomeM[states == 1 | states == 2] <- survBenefit  # one survivor, decrease payout by survBenefit %
annuityIncomeM[states == 3] <- 1  # both alive, 100% annuity income
annuityIncomeM <- annuityPayoutS * annuityIncomeM  # calculate annuity income by multiplying payout % times 100% payout amount

if (inflationProt == FALSE)
  annuityIncomeM <- annuityIncomeM/market.cumCsM # adjust future payments for inflation

cat("\nFinished Building Annuity Income table")
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
  # marginHECM = HECM lone margin percentage added to Libor index to determine variable interest rate
  # percentMIP = HECM mortgage percent charge for mortgage insurance (also added to interest rate)
  # initMort = HECM mortgage initial balance at loan closing
  
  years <- length(market.rmsM[1,])
  scenarios <- length(market.rmsM[,1])
  
  portfolioBalance <- matrix(0,scenarios,years + 1)   # initialize investment portfolio balance matrix to zeros, add a year zero
  mortgageBalance <- matrix(0,scenarios,years + 1)             # initialize mortgage balance matrix to zeros, add a year zero
  creditLimit <- portfolioBalance                 # initialize HECM credit limit matrix to zeroes, add a year zero
  hecmSpend <- portfolioBalance
  ################--------------- is growth rate correct for assumed percent of equities??????
  
  # create a year zero in all matrices for initial conditions
  spendPort <- matrix(0,scenarios,years + 1)
  spendPort <- cbind(spendPort[,1],spendFromPortfolio)
  expenses <- cbind(c(0),expenses)
  socSecM <- cbind(c(0),socSecM)
  portfolioBalance[,1] <- initportfolio        # set year zero portfolio balance
  marketVector <- 1 + ((market.rmsM - 1) * equityAllocation + liborM * ( 1 - equityAllocation)) # calculate growth rate of weighted portfolio
  
  if (printSummary == TRUE) {
  cat(paste("\nArithmetic Mean of Simulated Annual Portfolio Returns= ",sprintf("%.3f%%", 100*(mean(marketVector)-1)),sep=" "))
  cat(paste("\nStandard Deviation of Simulated Annual Portfolio Returns= ",sprintf("%.3f%%", 100*(sd(marketVector)),sep=" ")))
  }
  
  hecmSpend[,1] <- 0
  marketVector <- cbind(c(1),marketVector)  # set year zero (column 1) of market returns equal to one
  liborM <- cbind(liborM[,1],liborM) # add year zero column to Libor matrix
  liborM[,1] <- 0                     # set year zero Libor rate to zero
  creditLimit[,1] <- rep(initLoc,scenarios)
  
  # Set HECM interest rates
  rateHECM <- liborM + marginHECM + percentMIP
  rateHECM[rateHECM > maxRate] <- maxRate   # make sure no HECM rates exceed lifetime max
  tappedHECM <- rep(FALSE,scenarios)  # Note when HECM LOC first tapped
  depletedHECM <- rep(FALSE,scenarios) # Note when HECM LOC depleted
  depletedPort <- rep(FALSE,scenarios) # Note when portfolio depleted
  termHomeEquity <- rep(0,scenarios) # save terminal Home Equity in this vector
  termPortValue <- rep(0,scenarios) # save terminal Portfolio Value (TPV) in this vector
  unmetSpending <- rep(FALSE,scenarios) # save unmet spending scenario first year

    for (yearOfRetirement in 2:(years + 1) ) {    # note: first column contains year zero (inital) data
    
    for (scenarioNumber in 1:scenarios) {
      
      if (states[scenarioNumber,yearOfRetirement-1] == 4) {
        # surviving spouse died last year
        termPortValue[scenarioNumber] <- portfolioBalance[scenarioNumber,yearOfRetirement - 1]  # set TPV for this scenario = last years' portfolioBalance
        if (termPortValue[scenarioNumber] == 0) depletedPort[scenarioNumber] <- TRUE  # Note portfolio depleted in this scenario
        if (creditLimit[scenarioNumber,yearOfRetirement-1] == 0 & initLoc > 0) depletedHECM[scenarioNumber] <- TRUE
        termHomeEquity[scenarioNumber] <- homeEquity[scenarioNumber,yearOfRetirement - 1] - mortgageBalance[scenarioNumber,yearOfRetirement - 1]
        
        } else {
        
        # adjust portfolio spending amount (might be modified by portfolioBalance calculation)
        spendPort[scenarioNumber,yearOfRetirement] <- min(portfolioBalance[scenarioNumber,(yearOfRetirement - 1 )],expenses[scenarioNumber,yearOfRetirement] - annuityIncomeM[scenarioNumber,yearOfRetirement-1] - socSecM[scenarioNumber,yearOfRetirement] )
        
        # set portfolioDemand <- expenses - Social Sec Income - Annuity Income, spending matrix alread net of those two when passed
        portfolioBalance[scenarioNumber,yearOfRetirement] <- max(0,portfolioBalance[scenarioNumber,(yearOfRetirement - 1)] - ( min(portfolioBalance[scenarioNumber,(yearOfRetirement - 1)],spendPort[scenarioNumber,yearOfRetirement]) ))
       
        # add market returns to portfolioBalance
        
        portfolioBalance[scenarioNumber,yearOfRetirement] <- portfolioBalance[scenarioNumber,yearOfRetirement] * marketVector[scenarioNumber,yearOfRetirement]
      
        #     set hecmSpend <- portfolio demand  - remaining portfolio balance
        if (portfolioBalance[scenarioNumber,(yearOfRetirement)] < expenses[scenarioNumber,yearOfRetirement] - socSecM[scenarioNumber,yearOfRetirement] - annuityIncomeM[scenarioNumber,yearOfRetirement - 1] ) {
          
          hecmSpend[scenarioNumber,yearOfRetirement] <- expenses[scenarioNumber,yearOfRetirement] - spendPort[scenarioNumber,yearOfRetirement] - socSecM[scenarioNumber,yearOfRetirement] - annuityIncomeM[scenarioNumber,yearOfRetirement -1]
          hecmSpend[scenarioNumber,yearOfRetirement] <- min(hecmSpend[scenarioNumber,yearOfRetirement],creditLimit[scenarioNumber,(yearOfRetirement - 1)])                                                                                                            
          if (hecmSpend[scenarioNumber,yearOfRetirement] > creditLimit[scenarioNumber,(yearOfRetirement - 1)]) unmetSpending[scenarioNumber] <- TRUE
          if (hecmSpend[scenarioNumber,yearOfRetirement] > 1) tappedHECM [scenarioNumber] <- TRUE
        }
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
        
      }   # end-else when state isn't = 4
    }  # end scenarios for-loop
  }    # end yearOfRetirement for-loop
  
  hecmSpend <- round(hecmSpend)
  portfolioBalance <- round(portfolioBalance)
  termHomeEquity <- round(termHomeEquity)
  termNetWorth <- round(termNetWorth)
  termPortValue <- round(termPortValue)
  
  ###### end of old function

# assign results returned by function call
swrM <- portfolioBalance      # swr Portfolio results matrix
locHECM <- creditLimit        # HECM line of credit matrix
mortgageBalance <- mortgageBalance[,-1]   # HECM Reverse Mortgage balance matrix
portfolioBalance <- portfolioBalance[,-1]   # HECM Reverse Mortgage balance matrix

mortgageBalance[states == 0 | states == 4] <- 0      # zero out all mortgage balances when no clients alive
portfolioBalance[states == 0 | states == 4] <- 0      # zero out all portfolioe balances when no clients alive


spendFromPortfolio <- spendPort[,-1]  # Adjust spendFromPortfolio after bengenSpendHECM runs
swrM <- swrM[,-1]     # eliminate year zero columns
socSecM <- socSecM[,-1]
marketVector <- marketVector[,-1]
expenses <- expenses[,-1]
hecmSpend <- hecmSpend[,-1]
locHECM <- locHECM[,-1]
liborM <- liborM[,-1]
rateHECM <- rateHECM[,-1]
totalSpending <- spendFromPortfolio + socSecM + annuityIncomeM + hecmSpend 

networth <- swrM + pmax(0,homeEquity)   # home equity can go negative with HECM LOC, but negative mortgage is non-recourse (0)
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
homeEquity <- homeMarketVal - mortgageBalance  # build home equity matrix
homeEquity <- homeEquity[,-56] 
homeEquity[states == 0 | states == 4] <- 0      # zero out all home equity balances when no clients alive

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


#############################################
# Plot histogram of home equity at death
#############################################

# ignore terminal portfolio values greater that 4 time initial portfolio value and highly unlikely and plot histogram
# hist(tpvs[tpvs < 4*initportfolio],breaks=100,main="Histogram of Terminal Portfolio Values\nIgnores TPVs > 4 times Initial Portfolio Balance",xlab="Terminal Portfolio Values")
# cat("\nMedian Terminal Portfolio Value= ",median(tpvs),".")

# calculate average annual withdrawal rate
awrM <- spendFromPortfolio / swrMb4  # build withdrawal rate matrix
awrM[is.nan(awrM)] <- 0  # change NaN to zero where swrMb4 caused divide by zero
awrM[is.infinite(awrM)] <- 0  # change NaN to zero where swrMb4 near zero
nzmean <- mean(awrM[which(awrM != 0)]) # get mean of all non-zero withdrawal rates
if (printSummary == TRUE) {
  cat("\nMean withdrawal rate for all scenarios =  ",round(100*nzmean,2),"% per year.",sep=" ")
}

# cat("\nNumber of scenarios with unmet spending ",scenUnmetSpend)
# cat("\nPercent of scenarios with unmet spending ",pcScenUnmet," %")
# cat("\nFirst ten unmet spending scenarios: ",first10unmet)

parms <- data.frame(marginHECM,percentMIP,annualAdjust,initportfolio,initLoc,initMort,survivorExpense,survivorPercent,wifeClaimAge,husbandClaimAge,wifeAge,husbandAge,loEarnerOwnBenefit,hiEarnerOwnBenefit,annuityStartAge,annualAdjust,maxRate)
write.csv(parms,"~/desktop/Parms.csv")

totUnmetSpndYrs <- rowSums(round(totalSpending - expenses) != 0)
unmetSpendIndex <- which(rowSums((round(totalSpending-expenses))) != 0)

source('Summarize Scenario.R')

