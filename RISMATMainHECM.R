# Main RISMAT Script

cat("\014")  # clear the console

source('~/R Projects/RISMAT/MortalityStatesTables.R')
source('~/R Projects/RISMAT/returnsRISMAT.R')
source('~/R Projects/RISMAT/iFixedAnnuity.R')
source('~/R Projects/RISMAT/RetirementPkg.R')
source('~/R Projects/RISMAT/Social Security.R')
source('~/R Projects/RISMAT/BuildSpendM.R')
source('~/R Projects/RISMAT/BengenSpendHECM.R')
source('~/R Projects/RISMAT/Summarize.R')

set.seed(27514)
cat("\014")   # clear console
dev.list()    #clear all plots

##########################################
#   Initial parameters
##########################################

n <- 3                    # scenarios to run
husbandAge <- 65                # husband current age
wifeAge <- 67                   # wife current age
# Set Social Security parameters
wifeClaimAge <- 66              # age wife will claim Social Security benefits
husbandClaimAge <- 70           # age husband will claim Social Security benefit
husbandHE <- TRUE               # TRUE if husband has the higher Sociasl Security benefit
loEarnerOwnBenefit <- 12*1942   # expected SS benefit for lower earner
hiEarnerOwnBenefit <- 12*3037   # expected SS benefit for higher earner
# set portfolio spending parameters
initportfolio <- 5600000        # initial portfolio balance
spending <- 500000              # spend this percentage of last year's portfolio balance annually, or this amount if not a fraction
maxspend <- 350000              # max dollar amount to spend each year
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
ownsAnnuity <- 1                # = 1 if husband is annuitant and wife is survivior if TRUE, else 2
percentSurvivor <- 0.5          # percent of benefit that goes to survivor
# Set spending parameters
initspend <- 400000 # 230000            # expected spending year one of retirement
survivorPercent <- 0.375        # % expense decline after death of first spouse. Kotlikoff: two can live as cheaply as 1.6
annualAdjust <- 0 # -0.015      # expenses typically decline 1.5% annually throughout retirement
# Set HECM line of credit parameters
homeMarketVal <- 1200000    # current market value of primary residence
liborMean <- .02            # mean long term return for 1-yr Libor
liborSigma <- .01    # standard deviation long term return for 1-yr Libor
initLoc <- 640000    # HECM line of credit's initial credit limit
initMort <- 333      # HECM line of credit's initial mortgage balance at closing
maxRate <- .10336    # HECM line of credit's maximum lifetime interest rate
marginHECM <- .03    # HECM line of credit's maring added to Libor Index for variable rate loan
percentMIP <- .0125  # HECM line of credit's Monthly Insurance Premium percentage

#
#   End initial parameter settings
##########################################

##########################################
# Build mortality and state tables
##########################################
# 0 = neither spouse alive, 1=husband alive, 2= only wife alive, 3 - both spouses live, 4= last surviving spoude died previous yeart.

states <- mortalityStates(n,husbandAge,wifeAge,plotMort=TRUE)

# get number of scenarios and years
nscen <- dim(states)[1]
nyrs <- dim(states)[2]

##########################################
# Build inflation and market return tables
##########################################

mkt <- returnsRISMAT(states,inflation, inflationSd,rf,rp,mrSd,plot = TRUE) # build inflation and market returns matrices
market.csM <- t(matrix(unlist(mkt[1]), byrow=TRUE, nrow=nyrs ))
market.cumCsM <- t(matrix(unlist(mkt[2]), byrow=TRUE, nrow=nyrs) )
market.rmsM <- t(matrix(unlist(mkt[3]), byrow=TRUE, nrow=nyrs ))
market.cumRmsM <- t(matrix(unlist(mkt[4]), byrow=TRUE, nrow=nyrs ))
market.pvsM <- t(matrix(unlist(mkt[5]), byrow=TRUE, nrow=nyrs ))
market.ppcsM <- t(matrix(unlist(mkt[6]), byrow=TRUE, nrow=nyrs ))

# cat(paste("returnsRISMAT time = ",ptm <- proc.time(),sep=" "))

##########################################
# Build matrix of annuity incomes
##########################################
 
annuityIncomeM <- states
annuityIncomeM[annuityIncomeM == 3] <- annPmt  # 100% payment when both alive
if (ownsAnnuity == 1) {
  annuityIncomeM[annuityIncomeM == 2] <- annPmt/2  # 50% payment when only one alive
  annuityIncomeM[annuityIncomeM == 1] <- annPmt
}
if (ownsAnnuity == 2) {
  annuityIncomeM[annuityIncomeM == 1] <- annPmt/2  # 50% payment when only one alive
  annuityIncomeM[annuityIncomeM == 2] <- annPmt
}
  annuityIncomeM[annuityIncomeM == 4] <- 0  # otherwise zero
if (ownsAnnuity == 1) agestart <- husbandAge else agestart <- wifeAge
annuityIncomeM[,1:max(annuityStartAge - agestart,0)] <- 0  # zero income before annuity start age

if (inflationProt == FALSE) 
  annuityIncomeM <- annuityIncomeM/market.cumCsM # adjust future payments for inflation

##########################################
# Build a Social Security Income Matrix
##########################################

socSecM <- socialsecurity (wifeAge,husbandAge,wifeClaimAge,husbandClaimAge,husbandHE,states,loEarnerOwnBenefit,hiEarnerOwnBenefit) 

#############################################
# Build spending matrix
#############################################

cat("\nBuilding spending matrix.")

spendM <- buildSpendM (states, initSpend, annualAdjust,survivorPercent)
spendFromPortfolio <- spendM - socSecM - annuityIncomeM

############################################################################
# Build Sustainable withdrawal rates matrix with HECM line of credit reserve
############################################################################

# Build a matrix of Libor 1-year rates
liborM <- matrix(rnorm(nscen*nyrs,.02,.01),nscen,nyrs)

beng <- bengenSpendHECM (market.rmsM,initportfolio,spendFromPortfolio,initLoc,initMort,liborM,marginHECM,percentMIP,maxRate,sum=TRUE)
# assign results returned by function call
swrM <- beng$balances      # swr Portfolio results matrix
locHECM <- beng$loc        # HECM line of credit matrix
rmMortBal <- beng$locBal   # HECM Reverse Mortgage balance matrix
spendFromPortfolio <- beng$spf  # Adjust spendFromPortfolio after bengenSpendHECM runs
hecmDe <- beng$hecmD     #annual HECM demand

swrM <- swrM[,-1]     # eliminate year zero columns
spendFromPortfolio <- spendFromPortfolio[,-1]
locHECM <- locHECM[,-1]

# Adjust spendM when portfolio is depleted

swrMb4 <- cbind(rep(initportfolio,n),swrM[,1:(nyrs-1)]) # build matrix like swrM but each row equals the previous year's balance

if (sum(swrMb4 <= 0) == 0) {
  cat ("\nNo unmet spending scenarios.")
}

if (sum(swrMb4 <= 0) > 0) {
   spendNotMet <- which(spendM > swrMb4,arr.ind = TRUE)  # save indices of scenarios when spending wasn't met
   spendNotMet <- spendNotMet[!duplicated(spendNotMet[,1]),]   # remove duplicate rows of spendNotMet
   spendNotMet <- spendNotMet[order(spendNotMet[,1]),]  # sort to make easier to debug
   spendM[swrM == 0] <- 0 # if portfolio was depleted, zero spending
   b4spendNotMet <- cbind(spendNotMet[,1],spendNotMet[,2]-1)
   spendM[spendNotMet] <- swrM[b4spendNotMet]
}

# Adjust spendFromPortfolio after bengenSpendHECM runs

# calculate terminal portfolio values into vector "tpvs"

tpvStates <- states
tpvStates[tpvStates == 3] <- 0
tpvStates[tpvStates == 2] <- 0
tpvStates[tpvStates == 1] <- 0
tpvStates[tpvStates == 4] <- 1
homeEquity <- homeMarketVal - rmMortBal  # build home equity matrix
homeEquity[homeEquity < 0] <- 0       # no negative home equity
homeEquity <- homeEquity[,-56] 
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

cat("\nPercent of scenarios with depleted portfolios ignoring life state: ",100*(sum(tpvs == 0)/ nscen),"%")

#############################################
# Plot histogram of portfolio values at death
#############################################

# ignore terminal portfolio values greater that 4 time initial portfolio value and highly unlikely and plot histogram
hist(tpvs[tpvs < 4*initportfolio],breaks=100,main="Histogram of Terminal Portfolio Values\nIgnores TPVs > 4 times Initial Portfolio Balance",xlab="Terminal Portfolio Values")
cat("\nMedian Terminal Portfolio Value= ",median(tpvs),".")

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
cat("\nMean withdrawal rate for all scenarios =  ",round(100*nzmean,2),"% per year.",sep=" ")

summarizeThis <- 3   # summarize this scenario
summaryScenario <- summarizeThis

s <- summarize (summarizeThis,nyrs,states,husbandAge,wifeAge,husbandClaimAge,wifeClaimAge,socSecM,market.rmsM.market.csM,initportfolio,spendM,swrM,client.incomesM,annuityIncomeM,tpvs,unmetSpend,rmMortBal,locHECM,initMort,homeEquity)


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
rmSpend <- matrix(0,nscen,nyrs)
i <- 1
while (i < state4) {

  if (i == 1) {
      # annualPortSpend <- initportfolio - (swrM[summaryScenario,1]/market.rmsM[summaryScenario,i])
      annualPortSpend <- spendM[summaryScenario,i] - annuityIncomeM[summaryScenario,i] - socSecM[summaryScenario,i]
      rmMortBal[,i] <- initMort 
      rmSpend[,i] <- initMort
      
      } else {
     annualPortSpend <- spendM[summaryScenario,i] - annuityIncomeM[summaryScenario,i] - socSecM[summaryScenario,i]
   #   rmMortBal[,i] <- (rmMortBal[,(i-1)] - (max(0, spendM[,i] - swrM[,(i - 1)] ))) * (1 + min(rep(maxRate,nyrs), (liborM[,i] + rep(marginHECM,nyrs) + rep(percentMIP,nyrs))))
     # rmMortBal[,i] <- rmMortBal[,i] -  spendM[,i] - swrM[summaryScenario,i-1] * (1 + pmin(maxRate, (liborM[,i] + marginHECM + percentMIP)))    
     rmSpend[,i] <- rmMortBal[,i] - rmMortBal[,i - 1]
      }

p14[i] <- husbandAge+i-1
p15[i] <- wifeAge+i-1
p16[i] <- husbandClaimAge
p17[i] <- wifeClaimAge
p18[i] <- min(which(states[summaryScenario,] < 3)) - 1  # first spouse dies
p1[i] <- spendFromPortfolio[summaryScenario,i] #   annualPortSpend
p2[i] <- annuityIncomeM[summaryScenario,i]
p3[i] <- socSecM[summaryScenario,i]
p4[i] <- beng$hecmD[summaryScenario,i]   # spend from HECM
p5[i] <- spendFromPortfolio[summaryScenario,i] + annuityIncomeM[summaryScenario,i] + socSecM[summaryScenario,i] 
p6[i] <- spendM[summaryScenario,i]
p12[i] <- swrM[summaryScenario,i]
p8[i] <- rmMortBal[summaryScenario,i]
p9[i] <- homeEquity[summaryScenario,i]
p10[i] <- homeEquity[summaryScenario,i] + swrM[summaryScenario,i]
p11[i] <- locHECM[summaryScenario,i]
p7[i] <- market.rmsM[summaryScenario,i]
i <- i + 1
}

summarizeCsv <- data.frame(p14,p15,p16,p17,p18,p1,p2,p3,p4,p5,p6,p7,p8,p9,p12,p10,p11)
colnames(summarizeCsv) <- c("Husband Age","Wife Age","Husband Claims SS","Wife Claims SS","1st Spouse Dies","Spend from Port","Spend from Annuity","Spend from SS","Spend from HECM","Total Spend","Expenses","Market Return","RM Bal","Home Equity","Port Bal","Net Worth","HECM LOC")
write.csv(summarizeCsv,"~/desktop/summarizecsv.csv")

cat("\nPercent of scenarios with unmet spending ",100*sum(p10 < p11)/n," %")

