mortalityStates <- function (n,husbandAge,wifeAge,plotGen=TRUE) {

# Build mortality tables and mortality states table. Uses mortality tables from Excel Spreadsheet
# that uses Sharpe MATLAB algorithm.

library(ggplot2)
 
#  ------------ get vectors of mortality rates for person 1
#  make vectors for rows and columns of mortality tables
# ages = [50:1:120]' #  ages (rows in mortality tables)
# years = [2014:1:2113] #  years (columns in mortality tables)
#  compute mortalities for person 1
# if client.p1Sex == 'M'
#  get mortalities for 2014

# STATES
# state 0 - neither spouse alive
# state 1 - only spouse A is alive
# state 2 - only spouse B is alive
# state 3 - both spouses alive
# state 4 - last spouse dies in the previous year

cat(paste("Building mortality and state tables.",sep=" "))

w <- numeric(n)
wf <- numeric(n)


# build repeat columns function
rep.col<-function(x,n){ matrix(rep(x,each=n), ncol=n, byrow=TRUE) }

m1 <- read.csv("~/R Projects/RISMAT/GetRP2014Male.csv",header=FALSE) #  get mortality improvement rates 2015-2027
m2 <- read.csv("~/R Projects/RISMAT/GetMP2014Male.csv",header=FALSE) #  get mortality improvement rates 2015-2027
# else
m1f <- read.csv("~/R Projects/RISMAT/GetRP2014Female.csv",header=FALSE) #  get mortalities for 2014
m2f <- read.csv("~/R Projects/RISMAT/GetMP2014Female.csv",header=FALSE) #  get mortality improvement rates 2015-2027
# end
#  extend mortality improvement rates to 2113 using ultimate rates for 2027

m2027 <- m2[,13]
#  make mortality table from 2014 through 2113
m3 <- rep.col(m2027,57)
m4 <- cbind (1-m1, 1-m2 ,1-m3) #  join 2014 mortality and mortality factors (1 - improvement)
#m4 <- cumprod(m4) #  multiply 2014 mortality by cumulative factors
m4 <- t(apply(m4,1,cumprod))

m2027f <- m2f[,13]
m3f <- rep.col(m2027f,57)
#  make mortality table from 2014 through 2113
m4f <- cbind (1-m1f, 1-m2f ,1-m3f) #  join 2014 mortality and mortality factors (1 - improvement)
m4f <- t(apply(m4f,1,cumprod))

currentYear <- as.integer(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))
younger <- min(wifeAge, husbandAge) # extend the shorter length matrix to tl longer life to make equal

mortTableFemale.df <- m4f
mortTableMale.df <- m4             

wifeMort2.df <- mortTableFemale.df[(wifeAge-49):71,(currentYear - 2013):71]
husbandMort2.df <- mortTableMale.df[(husbandAge-49):71,(currentYear - 2013):71]

mortP12annual <-  read.csv("~/R Projects/RISMAT/SharpeBothDiags.csv")
lmortP1 <- length(mortP12annual[,1])  # number of elements read into vector
mortP1 <- numeric(lmortP1) # initialize vectors
mortP2 <- mortP1

mortP1[1] <- 1 - mortP12annual[husbandAge-49,1]
mortP2[1] <- 1 - mortP12annual[wifeAge-49,2]
youngest <- min(wifeAge,husbandAge)

for (ii in 2:(lmortP1 - youngest + 50)) {
  mortP1[ii] <- mortP1[ii-1] * (1 - mortP12annual[husbandAge-49 + ii - 1,1])
  mortP2[ii] <- mortP2[ii-1] * (1 - mortP12annual[wifeAge-49 + ii - 1,1])
}

mortP1 <- mortP1[1:(lmortP1 - youngest + 50)]
mortP2 <- mortP2[1:(lmortP1 - youngest + 50)]
mortP1[is.na(mortP1)] <- 0 # replace any NA's with zero
mortP1[is.na(mortP1)] <- 0

lMort <- length(mortP1)

probs <- matrix(mortP1,nrow=n,ncol=lMort,byrow=TRUE) # probability matrix husband's mortality
randnums <- matrix(runif(lMort*n),ncol=lMort,nrow=n) # same size table of random uniform numbers
randnums[,2:lMort] <- randnums[,1]
statesP1 <- (randnums >= probs) # matrix T/F client is alive

statesP1a <- statesP1

for (d in 1:n){
  w[d] <- min(which(randnums[d,1] >= probs[d,]))
  statesP1[d,] <- c(rep(1,w[d]),rep(0,lMort-w[d]))
}

# repeat for wife
probsW <- matrix(mortP2,nrow=n,ncol=lMort,byrow=TRUE) # probability matrix wife's mortality
randnumsW <- matrix(runif(lMort*n),ncol=lMort,nrow=n) # same size table of random uniform numbers
randnumsW[,2:lMort] <- randnumsW[,1]
statesP2 <- (randnumsW >= probsW) # matrix T/F client is alive
statesP2a <- statesP2

for (d in 1:n){
  wf[d] <- min(which(randnumsW[d,1] >= probsW[d,]))
  statesP2[d,] <- c(rep(1,wf[d]),rep(0,lMort-wf[d]))
}

statesP2 <- statesP2 * 2 # 1 means husband alive, 2 means wife alive

states <- statesP1 + statesP2

estates <- (states[,2:lMort] == 0) & (states[,1:lMort - 1] > 0)
estates <- c(rep(0,n),estates)
states <- states + 4*estates

#plot mortality table if requested by caller

if (plotGen) {

probSurvive1only <- colMeans(states == 1)
probSurvive2only <- colMeans(states == 2)
probSurviveBoth <- colMeans(states == 3)
probSurviveAll <- rbind(probSurvive1only,probSurvive2only,probSurviveBoth)
plotMortTable(probSurviveAll,plotGen)
}

return (states)
}



returnsRISMAT <- function (states,inflation,inflationSd,rf,rp,mrSd,plot = TRUE,printSummary=TRUE) {

# Create a matrix of Monte Carlo-simulated stock market returns with one row per scenario and one column
# per year of retirement. Output matrices should be same size as retiree life expectancy state diagram
# (nrows by ncols). Also, create a cost-of-living (inflation matrix of the sames size.)
# This code has been converted from MATLAB to R. The MATLAB code was produced by William F. Sharpe and complete documentation can be found
# at https://web.stanford.edu/~wfsharpe/RISMAT/

# INPUT PARAMETERS:

# states = matrix of life expectancy states created by "Build Mortality Tables" R Script
# inflation = expected average annual inflation rate (e.g., 0.02 for 2%)
# inflationSd <- 0.01   # standard deviation of annual inflation (e.g., 0.01 for 1%)
# rf = risk-free real return rate (e.g., 0.01)
# rp = expected risk premium, the market portfolio expected return over risk-free rate (e.g., 0.0425 for 4.25%)
# mrSd = market return annual standard deviation (e.g., 0.125 for 12.5%)
# plot = TRUE (default, plot histograms of market returns and inflation rates)

# OUTPUT GENERATED:

# returnsRISMAT$market.csM = matrix of inflation growth rates, i.e., (1 + annual inflation percentage).
# returnsRISMAT$market.cumCsM = matrix of cumulative inflation growth rates, i.e., (1 + cumulative inflation percentage).
# returnsRISMAT$market.rmsM = matrix of market returns as growth rates, i.e., (1 + annual market return percentage).
# returnsRISMAT$market.cumRmsM = matrix of cumulative market returns as growth rates, i.e., (1 + cumulative market return percentage).
# returnsRISMAT$market.pvsM = matrix of present value factors discounted at the risk-free rate
# NOTE: All matrices will be of size nrows by ncols.

# create a market data structure with default values
# cost of living 

# get market assumptions
  
  cat("\nCalculating returns.")

nrows <- length(states[,1]) #all matrices same size as life expectancy states matrix
ncols <- length(states[1,])

market.eC <- 1 + inflation   # expected cost of living ratio
market.sdC <- inflationSd   # standard deviation of cost of living ratios
# risk-free real investments   
market.rf <- 1 + rf   # risk-free real return rate
# market portfolio returns
market.exRm <- 1 + rp # market portfolio expected return over risk-free rate
market.sdRm  <- mrSd  # market portfolio standard deviation of return
     
  # compute cost of living (inflation) matrix
       u <- market.eC 
       v <- market.sdC^2
       b <- sqrt(log((v/(u^2)) + 1)) 
       a <- 0.5 *log((u^2)/exp(b^2))
       market.csM <- exp(a + b*matrix(rnorm(nrows*ncols),nrows,ncols))
  # compute cumulative cost of living (inflation) matrix
      
       m <- t(apply(market.csM,1,cumprod))
       market.cumCsM <- cbind(ones(nrows,1),m[,1:ncols-1])
       
  # compute risk-free real returns matrix
       market.rfsM <- market.rf*ones(nrows,ncols)
  # compute cumulative risk-free real returns matrix at ends of each year

       market.cumRfsM <- cbind(ones(nrows,1),m[,1:ncols-1])

  # compute market returns matrix
       u <- market.exRm + (market.rf - 1) # get total market expected return
       v <- market.sdRm^2
       b <- sqrt(log((v/(u^2)) + 1))
       a <- 0.5 *log((u^2)/exp(b^2))
      #  cat ("\nTerms for market ReturnsRISMAT = ",u,v,b,a,sep=" ")
       market.rmsM <- exp(a + b*matrix(rnorm(nrows*ncols),nrows,ncols))
       
  # compute market cumulative returns matrix
      
       market.cumRmsM <- cbind(ones(nrows,1),m[,1:ncols-1])
             
  # compute ppcs and present values matrix
       
       
       b <- log(u/market.rf)/log(1 + (market.sdRm^2)/(u^2))
       a <- sqrt(u*market.rf)^(b-1)
      
       # as <- (a ^ (seq(from=0,to=ncols-1))) #element-wise exponentiation
     
       market.ppcsM <- matrix(0,nrows,ncols)
       
       ############ following doesn't wotk. Present values wrong.
       
       # mb <- market.cumRmsM ^ (-b)
       # for (ii in 1:nrows){
       #   for (jj in 1:ncols){
       #     market.ppcsM[ii,jj] <- as[jj] * mb[ii,jj]
       #   }
       # }

       market.pvsM <- market.ppcsM/nrows 

   # temporary
       market.avec <- as
       market.b <- b
       
  # print a histogram of simulated market returns and inflation rates
        
         # if (plotGen) hist((market.rmsM-1)*100,col="lightblue",xlab="Simulated Annual Market Returns (%)",xlim=c(-40,60),main=paste("Histogram of \nSimulated Annual Real Market Returns"))
#          if (plotGen) {
       
       mr.df <- data.frame(as.vector((market.rmsM-1)*100))
       colnames(mr.df) <- "Returns"
       t1 <- "Arithmetic Mean of\nSimulated\nMarket Returns= "
       t2 <- sprintf("%.2f%%", 100*(mean(market.rmsM)-1))
       t3 <- "\nSD= "
       t4 <- sprintf("%.2f%%", 100*(sd(market.rmsM - 1)))
       
       marketReturnsPlot(mr.df, paste(t1,t2,t3,t4), plotGen=TRUE)
           
            # print(
            #   ggplot(data=mr.df, aes(mr.df$Returns)) + 
            #     geom_histogram(binwidth=1, 
            #                    col="blue", 
            #                    fill="blue", 
            #                    alpha = .2) + 
            #     xlim(-40,60) +
            #     labs(x="Simulated Market Returns (%)", y="Count")  +
            #     ggtitle(paste("Histogram of \nSimulated Annual Real Market Returns")) +
            #     annotate("text", x = -25, y = n*1.8, label =paste(t1,t2,t3,t4))
            #   
            # )
    #      }
          
          # if (plotGen) hist((market.csM-1)*100,col="lightblue",xlab="Simulated Annual Inflation Rates (%)",main=paste("Simulated Annual Inflation Rates (%)"))

            if (plotGen) {
              if.df <- data.frame(as.vector((market.csM-1)*100))
              colnames(if.df) <- "Inflation"
              t1 <- "Arithmetic Mean of\nSimulated\nInflation Rates= "
              t2 <- sprintf("%.3f%%", 100*(mean(market.csM)-1))
              t3 <- "\nSD= "
              t4 <- sprintf("%.3f%%", 100*(sd(market.csM - 1)))
                            
              print(
                ggplot(data=if.df, aes(if.df$Inflation)) + 
                  geom_histogram(binwidth=.1, 
                                 col="blue", 
                                 fill="blue", 
                                 alpha = .2) + 
                  xlim(-2,6) +
                  labs(x="Simulated Inflation Rates", y="Count")  +
                  ggtitle(paste("Histogram of \nSimulated Annual Inflation Rates")) +
                  annotate("text", x = -.75, y = n*2, label =paste(t1,t2,t3,t4))
              )
          }
       
       if (printSummary == TRUE) {
          
        cat(paste("\nArithmetic Mean of Simulated Annual Market Returns= ",sprintf("%.3f%%", 100*(mean(market.rmsM)-1)),sep=" "))
        cat(paste("\nStandard Deviation of Simulated Annual Market Returns= ",sprintf("%.3f%%", 100*(sd(market.rmsM)),sep=" ")))
        
        cat(paste("\nArithmetic Mean of Simulated Inflation Rates= ",sprintf("%.3f%%", 100*(mean(market.csM)-1)),sep=" "))
        cat(paste("\nStandard Deviation of Simulated Annual Inflation Rates= ",sprintf("%.3f%%", 100*(sd(market.csM)),sep=" ")))
        
        }
       
      cat("\nReturns calculated.")
return(list(market.csM,market.cumCsM,market.rmsM,market.cumRmsM,market.pvsM,market.ppcsM))
       
} # end function returnsRISMAT

#########################################################
# # function client <- iFixedAnnuity_process(iFixedAnnuity, client, market);
  # creates fixed annuity income matrix and fees matrix
  # then adds values to client incomes matrix and fees matrices.

# NOTE: Sharpe's RISMAT calculates annuity payments assuming insurer invests in risk-free
# bonds. This provides a payment significantly less than annuities on the market. This
# function accepts a market annuity payment estimate (annPmt), instead, and overrides the
# Sharpe calculation.
#########################################################

annuityMatrix <- function (annPmt,iFixedAnnuity.guaranteedIncomes,  iFixedAnnuity.pStateIncomes,  iFixedAnnuity.graduationRatio, iFixedAnnuity.realOrNominal,  iFixedAnnuity.valueOverCost,  iFixedAnnuity.cost, client.budget,client.feesM,nscen,nyrs,market.cumCsM,market.pvsM)
{
  
# INPUT PARAMETERS:
#   
# iFixedAnnuity.guaranteedIncomes = numeric ( )     # guaranteed relative or absolute incomes for years 1,... or null vector if none
# iFixedAnnuity.pStateIncomes = c(0, .5, .5, 1, 0)  # relative incomes in first post-guarantee year for personal states 0,1,2,3 and 4
# iFixedAnnuity.graduationRatio = 1.00              # graduation ratio (annual increase) of each post-guarantee income to prior post-guarantee income
# iFixedAnnuity.realOrNominal = 'r'                 # type of annuity incomes (real 'r' or nominal 'n')   
# iFixedAnnuity.valueOverCost = 0.90                # ratio of value to initial cost
# iFixedAnnuity.cost = 100000                       # purchase price of annuity
# client.budget = 1000000
# nscen = number of scenarios in states matrix (rows)
# nyrs = number of years in states matrix columns)
# market.pvsM = PV of market values from returnsRISMAT function
# market.cumCsM = cum inflation rates from returnsRISMAT function

# OUTPUT:
  
# client.incomesM = client annuity income matrix nscen x nrows
# client.feesM = client fees matrix nscen x nrows
# # subtract cost from client budget
# client.budget = client budget reduced by annuity cost

  client.incomesM <- matrix(0,nscen,nyrs)  # Initialize client income matrix to zero
  feesM <- matrix(0,nscen,nyrs)  # Initialize fees matrix to zero
  client.feesM <- feesM
  
  # make matrix of incomes for states 0,1,2,3 and 4
     psIncomesM <- numeric()
     for (pState in 0:4){
        # guaranteed incomes 
        
           if (pState == 0){ 
             if(length(iFixedAnnuity.guaranteedIncomes) == 0) guarIncomes <- numeric()
             else
             guarIncomes <-matrix(0,1,length(iFixedAnnuity.guaranteedIncomes))
           }
           if (pState > 0 & pState < 4) guarIncomes <- iFixedAnnuity.guaranteedIncomes
            
           if (pState == 4) {
             if(length(iFixedAnnuity.guaranteedIncomes) == 0) guarIncomes <- numeric()
             else {
             gIncome <- cumsum(iFixedAnnuity.guaranteedIncomes)
             guarIncomes <- rev(gIncome)
             }
           }                                       
           nAnnYrs <-  nyrs - length(iFixedAnnuity.guaranteedIncomes)
           gradRatios <- iFixedAnnuity.graduationRatio^(as.vector((seq(0,nAnnYrs - 1,by=1)))) 
           annIncomes <- iFixedAnnuity.pStateIncomes[pState+1]*gradRatios
           
  
        # guaranteed and annuity incomes
           psIncomes <- c(guarIncomes, annIncomes)
        # add to matrix
           psIncomesM <- rbind(psIncomesM, psIncomes)
     } # for pState <- 0:4
     
  # create matrix of relative incomes for all scenarios
     incomesM <- matrix(0,nscen,nyrs)
     for (pState in 0:4){
        # make matrix of incomes for personal state
          mat <- rep.row(psIncomesM[pState+1,],nscen) 
           
        # find cells in client personal state matrix for this state
          ii <- which(states == pState,arr.in=TRUE)
        # put selected incomes in incomes Matrix
          incomesM[ii] <- mat[ii]
     }

  # if values are nominal, change to real
     if (tolower(iFixedAnnuity.realOrNominal) == 'n') {
         incomesM <- incomesM / as.vector(market.cumCsM)
    } # if lower(iFixedAnnuity.realOrNominal) == 'n'

  # compute present value of all relative incomes
    pvIncomes <- sum(sum(incomesM * as.matrix(market.pvsM))) #______________________

  # create fee matrix
    feesM <- matrix(0,nscen,nyrs)
  # compute value of annuity purchased
    # Override factor if annual payout quoted
    annVal <- iFixedAnnuity.valueOverCost * iFixedAnnuity.cost

  # add fee to matrix in column 1
    feesM[,1] <- iFixedAnnuity.cost - annVal

  # scale incomes so pv <- amount invested - fee
    factor <- annVal/pvIncomes
 
   # factor <- annPmt #####################################################
    
    incomesM <- incomesM * factor

  # add incomes and fees to client matrices
    client.incomesM <- client.incomesM + incomesM
    # client.feesM <- client.feesM + feesM
    client.feesM <- client.feesM + feesM
  # subtract cost from client budget
    client.budget <- client.budget - iFixedAnnuity.cost
    cat(paste("\nClient budget: ",client.budget))
    
    return(list(client.incomesM, client.feesM, client.budget))
    
}     # end of annuityMatrix function
# end


     # Package of personal functions Dirk Cotton frequently used for retirment plan analysis

#################################
# Create a vector by duplicating (n times) row vector x.
#################################

rep.row <- function(x,n){matrix(rep(x,each=n),nrow=n)} # repeat row function
rep.col <- function(x,n){matrix(rep(x,each=n), ncol=n, byrow=TRUE)} # repeat col function

#################################
# cumprod function that works like MATLAB's cumprod
#################################

cumprod.M <- function(x) {  # convert to cumprod function in Matlab-style
  if (is.vector (x)) {
    return (cumprod(x))
  } else {
    return(apply(t(x),2,cumprod))
  }
}

#################################
# fast multlply of a vector times each row of a matrix
#################################

matTimesvec <- function (mat,vec) {
  return (t(t(mat) * vec))
}

#################################
# ones -- equivalent of MATLAB ones function
#################################

ones <- function (a,b){
  matrix(1,a,b)
}

#################################

# function tpv returns the integer year in which the spending model
# portfolio balance falls to zero. x is the vector containing market return growth
# factors (market return plus one). initportfolio is the initial portfolio balance.
# years is the max number of years to calculate (function returns 99 if balance never
# falls to zero.) spending contains the constant annual spending amount. 
# Start C++ vector index at zero (R starts at 1.)
#################################

library(Rcpp) 

cppFunction('int tpv (NumericVector growthfactors, int initportfolio, int years, double spending) {
            int balance = initportfolio;
            int k;
            
            for (k = 0; k <= years-1; ++k) {
            balance = (balance - spending); 
            if (balance >= 0) balance = balance * growthfactors[k];
            }
            return balance;
            }'
)


#################################
# function loopYears returns a numeric vector of the cumulative results of a)
# beginning with an initial portfolio value, subtracting a spending % of the initial 
# balance, then applying the corresponding market growth factor from a second vector and
# storing the result in resultVector[1]. The function then moves to the next "year",
# using the previous year's ending balance instead on the initial portfolio balance.
# The loop ends when the last year in the result vector has been computed or the balance becomes
# <= 0. All balances <= 0 are replaced with zero.
# growthfactors is the vector containing market return growth
# factors (annual market return plus one). initportfolio is the initial portfolio balance.
# years is the max number of years to calculate.
# Start C++ vector index at zero (R starts at 1.)
# If 0 < spending < 1, then spending is considered a percentage to spend of the previous years EOY portfolio balance.
#     Else spending is considered a constant-dollar amount to spend annually for positive vales of
#     spending, or to save each year of spending is negative.
# maxspend is the maximum amount to permitted spend each year from the portfolio.

#################################

library(Rcpp) 

# install.packages('Rcpp')
library('Rcpp')

cppFunction('NumericVector loopYears (NumericVector x, double initportfolio, int years, double spending, double maxspend) {
            
            int k;
            double prevYrBal;
            double spnd;
            NumericVector balance(years);
            
            prevYrBal = initportfolio;
            
            for (k = 0; k <= years-1; ++k) {
            
            // if spending between zero and one it represents the percentahe to spend from the previous
            // years portfolio balance. Otherwise, it represents the constat dollar amount to spend every year.
            // A positive amount outside the range 0<spending<1 is amount to spend. A negative amount is an amount to save annually.            
            if (spending > 0 && spending < 1) spnd = spending * prevYrBal;
            else spnd = spending;
            // Rcout << "spnd is " << spnd  ; 
            if (spnd > maxspend) spnd = maxspend;
            balance[k] = (prevYrBal - spnd) * x[k]; 
            prevYrBal = balance[k];
            if (balance[k] < 0) balance[k] = 0;
            }
            
            return balance;
            }'
 )
socialsecurity <- function (wifeAge,husbandAge,wifeClaimAge,husbandClaimAge,husbandHE,states,leBenefit,heBenefit,ssBenefits,switchBenefit,printSummary) {
   
# Build Social Security income matrix
  
  cat(paste("\nBuilding Social Security Expected Income matrix.\n",sep=" "))
  socSecM <- matrix(0,nscen,nyrs) # create output matrix of zeroes, defaulting to no SS benefit
  nyrs <- length(states[1,])
  nscen <- length(states[,1])
  
  
# STATES  ###################
 
  claimStatesH <- socSecM  # initial vector of claim states. 0 = neither has claimed, 2 = only low earner has claimed,
  #                              1 = only high earner has claimed, 3 = both have claimed
  claimStatesW <- socSecM 
  claimStatesB <- socSecM
  
  if (husbandHE == TRUE) {
    leclaim <- 1
    heclaim <- 2
  } else {
    leclaim <- 2
    heclaim <- 1
  }

  # can lower earner claim own benefits and switch to spousal when higher-earning spouse claims benefits?
  # there will be no benefit to switch in the future if both claim at the same age
  # if low-earner claims at 62 and higher earner at 70, le spouse's benefit will be larger of existing benefit plus 1/2 
  # the difference between their FRA benefits.
  # if low-earner claims at 67 and higher earner at 70, le spouse's benefit will be larger of existing benefit plus 1/2 
  # spouse's FRA benefit. switchBenefit is the ADDITIONAL benefit of switch to spousal benefit and may be zero.

    hiEarner <- 1 # higher earner benefit in column 1  # these assignments just make the following code easier to read
    loEarner <- 2 # lower earner benefit in column 2
    age62 <- 1 # age 62 beneftit in row 1
    FRA <- 2   # FRA benefit in row 2
    age70 <- 3 # age 70 benefit in row 3
      
    switchBenefit <- 0      # no benefit to wife switching to spousal benefit when husband claims
    if (wifeClaimAge == 62 & husbandClaimAge != 62) {
      switchBenefit <- max(ssBenefits[age62,loEarner],ssBenefits[age62,loEarner] + .5 * (ssBenefits[FRA,hiEarner] - ssBenefits[FRA,loEarner])) - ssBenefits[age62,loEarner]  # if wife claimed early her spousal benefit = existing claimed benefit + half difference in their FRA's
 
    } 
    if ((wifeClaimAge == 67 | wifeClaimAge == 66) & (husbandClaimAge != 67 & husbandClaimAge != 66)) {  # wife must have claimed at FRA and husband at 70
      switchBenefit <- max(0.5*ssBenefits[FRA,hiEarner],ssBenefits[FRA,loEarner]) - ssBenefits[FRA,loEarner]
      }
  
    if (wifeClaimAge == 62) leBenefit <- ssBenefits[age62,loEarner]
    if (wifeClaimAge == 67 | wifeClaimAge == 66) {
      leBenefit <- ssBenefits[FRA,loEarner]
      cat("\nClaim Age for wife = 67 and leBenefit=",leBenefit)}
    if (wifeClaimAge == 70) leBenefit <- ssBenefits[age70,loEarner]
    if (husbandClaimAge == 62) heBenefit <- ssBenefits[age62,hiEarner]
    if (husbandClaimAge == 67 | husbandClaimAge == 66) heBenefit <- ssBenefits[FRA,hiEarner]
    if (husbandClaimAge == 70) heBenefit <- ssBenefits[age70,hiEarner]
    
  if (husbandAge > husbandClaimAge) husbandClaimAge <- husbandAge   # husband has already claimed benefits before current age
  if (wifeAge > wifeClaimAge) wifeClaimAge <- wifeAge               # wife has already claimed benefits before current age
  
  claimStatesH[,(husbandClaimAge - husbandAge + 1):nyrs] <- leclaim # set husband has claimed
  claimStatesW[,(wifeClaimAge - wifeAge + 1):nyrs] <- heclaim # set wife has claimed
  claimStatesB <- claimStatesH + claimStatesW   # created state=3 when both have claimed
  
  leSpousal <- max(heBenefit/2, leBenefit)   # low earners spousal benefit = 1/2 high earner's FRA
                              
   # if wife (LE) claims before FRA and husband hasn't claimed, she can only claim her own benefit 
   ###### check following to make gender differences work---------------------NEED TO ADD FRA Calculation
  if ((husbandClaimAge + (husbandAge-wifeAge) > wifeClaimAge) & wifeClaimAge < 67) leSpousal <- 0
  cat("\n***husbandClaimAge",husbandClaimAge,"husband age",husbandAge," wife age=",wifeAge)
  leBenefit <- max(leBenefit, .5*ssBenefits[FRA,hiEarner]) # low earner gets larger of own and spousal benefits
  leSurvivors <- max(leBenefit,heBenefit)  # low earners Survivors benefit
 
  # build matrix of SS payments based on claiming status (rows) and mortality state (cols) 
  combinedStates <<- matrix (0,4,5)
  colnames(combinedStates) <- c("Both Dead","Only L.E. Alive","Only H.E. Alive","Both Alive","Both Dead")
  rownames(combinedStates) <- c("Neither Claimed","Only H.E. Claimed","Only L.E. Claimed","Both Claimed")
  combinedStates[,"Both Dead"] <- 0 # both dead, no benefits
  combinedStates["Only L.E. Claimed","Only H.E. Alive"] <- 0 # neither claimed, no benefits
  combinedStates["Only H.E. Claimed","Only L.E. Alive"] <- leSurvivors # Low earner should claim. no advantage to waiting
  combinedStates["Only H.E. Claimed","Both Alive"] <- heBenefit 
  combinedStates["Only H.E. Claimed","Only H.E. Alive"] <- heBenefit 
  combinedStates["Both Claimed","Only H.E. Alive"] <- heBenefit 
  combinedStates["Only L.E. Claimed","Both Alive"] <- leBenefit # L.E. can only claim when H.E. does # leBenefit  
  combinedStates["Only L.E. Claimed","Only L.E. Alive"] <-leSurvivors
  combinedStates["Both Claimed","Both Alive"] <-heBenefit + leBenefit + switchBenefit
  combinedStates["Both Claimed","Only L.E. Alive"] <- leSurvivors
  
# build a matrix, resultsM, and fill with the correct benefit from combinedStates matrix with
# the indices of the correct payment from combinedStates[states,claimStatesB]. For example,
# fill resultsM[a,b] from the payment at combinedStates[states,claimStatesB].
  
 resultsM <- matrix(as.vector(combinedStates)[(states)*4+(claimStatesB+1)],nscen,nyrs)
printSummary <- TRUE
 if (printSummary == TRUE)  {
 cat(paste("\nLow Earner's own annual benefit is =",leBenefit,sep=" "))
 cat(paste("\nHigh Earner's own annual benefit is =",heBenefit,sep=" "))
 cat(paste("\nLow Earner's survivors benefit is =",leSurvivors,sep=" "))
 cat(paste("\nHusband plans to claim at age ",husbandClaimAge,sep=" "))
 cat(paste("\nWife plans to claim at age ",wifeClaimAge,sep=" "))
 if (husbandHE)  cat(paste("\nHusband has higher benefit.",sep=" "))
      else cat(paste("\nWife has higher benefit.",sep=" "))
 }
 
  return(resultsM)
}
    buildSpendM <- function (states, initSpend, annualAdjust,survivorExpense) {
  
#build a matrix of estmated real-dollar (before inflation) spending.
  
# states = matrix of longevity states
# initSpend = expected spending for year one
# annualAdjust = percent to increase spending annually (positive) or decrease (negative)
# survivorPercent <- percent reduction of expenses after first spouse dies
  
  nscen <- length(states[,1])   # number of scenarios (rows)
  nyrs  <- length(states[1,])    # number of years (columns) per scenario
  spendM <- matrix(0,nscen,nyrs)
  spendM[,1] <- initspend      # first column is initial annual spending amount
  
# apply annualAdjustment to increase or decrease spending
  
  for (ii in 2:nyrs){
      spendM[,ii] <- spendM[,ii-1] * (1 + annualAdjust)
    }
  
# Reduce spending factors depending on who's still alive
  expenseFactor <- states
  expenseFactor[expenseFactor == 0 | expenseFactor == 4] <- 0      # no one alive, expense factor = 0
  expenseFactor[expenseFactor == 1 | expenseFactor == 2] <- survivorExpense  # one spouse alive, reduce expenses
  expenseFactor[expenseFactor == 3] <- 1              # factor =1, no expense reduction
  

  spendM <- spendM * expenseFactor
  
  return(spendM)
}


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

plotData <- data.frame(socSecM[summaryScenario,],annuityIncomeM[summaryScenario,],pmax(0,spendFromPortfolio[summaryScenario,]),hecmSpend[summaryScenario,])
cat("\nCalculated plotdata.")
plotData$row <- seq_len(nrow(plotData))
colnames(plotData) <- c("Social Security","Pensions/Annuities/TIPS","Spend from Portfolio","Spend from HECM","rows")
plotData2 <- melt(plotData,id.vars = "rows")

plotExpenses <- data.frame(expenses[summaryScenario,])
plotExpenses$rows <- seq_len(nrow(plotExpenses))
colnames(plotExpenses) <- c("Expenses","rows")
plotExpenses <- melt(plotExpenses,id.vars = "rows")

library(ggplot2)

  print(ggplot(plotData2, aes(x=rows, y=value, fill=variable)) + 
  geom_bar(stat="identity") +
  geom_point(data=plotExpenses,aes(x=rows,y=value)) +
  # theme(legend.position = c(.2,.2)) +
  ggtitle ("Figure 17. Annual Spending Source vs. Expenses (black)\nScenario Number ",summaryScenario) +
  xlab("\nYear of Retirement") +
  ylab("Annual $(Real) Spent") +
  scale_y_continuous(labels = scales::comma) +
  xlim(0,min(which(states[summaryScenario,] == 0) - 2))
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

###################################################
# Analyzed unfunded spending years
###################################################

# snm <- unmetSpendIndex  # create vector of row numbers for years spending not met
# snmYearofDeath <- apply(states[snm,],1,which.max) - 1  #  create vector of year of death for unfunded years (one year before state=4)
# ##### snmFailure <- spendNotMet[!duplicated(spendNotMet[,1]),2]  # create vector of years of death when spending not met
# unmetSpend <- data.frame(unmetSpendIndex,apply(states[snm,],1,which.max) - 1,snmFailure)
# unmetSpend$unfundedYears <- unmetSpend$snmYearofDeath - snmFailure
# unmetSpendnz <- unmetSpend[unmetSpend$unfundedYears != 0,] # remove any scenarios in which portfolio was depleted same year as death
# if (length(unmetSpendnz$snm) >0 ) {
# hist(unmetSpend$unfundedYears,breaks=length(unmetSpendnz$snm),main="Histogram of Number of Unfunded Years\nWhen Scenarios Don't Meet Spending Demand",xlab= "Number of Unfunded Years Before Death",ylab="Number of Scenarios")
# }

    return
}

plotMortTable <- function(probSurviveAll,plotGen) {

  if (!plotGen) return
library(reshape2)
plotdata <- reshape2::melt(probSurviveAll)
print(
  ggplot(plotdata, aes(x = Var2, y = value, fill = Var1)) +
    scale_fill_manual(values=c("blue", "red","green"),name="Surviving\nSpouses",labels=c("Both Survive", "Only Female Spouse Survives", "Only Male Spouse Survives")) +
    geom_bar(stat = "identity") + ggtitle("Mortality Table Generated") +
    xlab("Year of Retirement") + 
    theme(legend.position=c(.8,0.8)) +
    ylab("Probabilty of Survival")) 

return

}

marketReturnsPlot <- function (mr.df, mrLabel, plotGen=TRUE) {
  
  if (!plotGen) return
  library(reshape2)
  
print(
  ggplot(data=mr.df, aes(mr.df$Returns)) +
    geom_histogram(binwidth=1,
                   col="blue",
                   fill="blue",
                   alpha = .2) +
    xlim(-40,60) +
    labs(x="Simulated Market Returns (%)", y="Count")  +
    ggtitle(paste("Histogram of \nSimulated Annual Real Market Returns")) +
    annotate("text", x = -25, y = n*1.8, label =mrLabel)

)

}# Deferred Income Annuities
# 
# Strategy: Previous models built diaIncomeM matrix from an immediate income and/or a pension. 
# Separate annuity income into five matrices and combine them into diaIncomeM:
#   - Pension income
#   - an Immediate annuity
#   - a deferred income annuity
#   - one or two QLACS (per spouse)
#   - a HECM with tenure payments
# 
# The goal is to be able to inspect the matrices individually for correctness, to simplify the addition of
# future annuity products, tax analysis, or multiple annuities. Add the following functions:
# buildPensionM <- function(states,purchaseAmount,spiaPayout,startYear,owner,survivorPercent,inflationProt)
# buildDiaM <- function(states,purchaseAmount,purchaseAge,diaPayout,startYear,owner,household,survivorPercent,inflationProt)
# buildQlacMale <- function(states,purchaseAmount,diaPayout,startYear,owner,survivorPercent,inflationProt)
# buildQlacFemale <- function(states,purchaseAmount,diaPayout,startYear,owner,survivorPercent,inflationProt)
# buildHECMtenure <- function(states,purchaseAmount,hecmTenurePayout,startYear,owner,survivorPercent,inflationProt)


buildDiaM <- function(states,diaPurchase,purchaseAgeDia,diaPayout,startAgeDIA,diaOwnerisHusband,survivorPercentDIA,inflationProtDIA,market.cumCsM,husbandAge,wifeAge) {
  # Parameters: 
  # states -- states survivor matrix
  # diaAllocation -- percent of initial portfolio to spend on DIA
  # purchaseAgeDia -- age at which DIA will be purchased
  # diaPayout -- annual payout for this diaPayout (see quotes at abaris.com)
  # startAgeDia -- annuity owners age when payouts will begin
  # ownerisSpouseA -- TRUE or FALSE
  # survivorPercentDIA -- % of payout paid to a surviving spouse
  # inflationProt -- payouts are inclation-protected, TRUE or FALSE
  # market.cumCsM -- inflation-rate factor table
  # husbandAge -- husband's age at start of simulations (column 1 of states table)
  # wifeAge -- wife's age at start of simulations (column 1 of states table)
  
  if (diaOwnerisHusband) startAgeSimulation <- husbandAge else startAgeSimulation <- wifeAge

  diaIncomeM <- states 
  cat("\nBuilding Deferred Annuity Income table")
  cat("\nDual Payment ",diaPayout)
  cat("\nSurvivor Payment ",survivorPercentDIA * diaPayout)
  
  diaIncomeM[states == 4 | states == 0] <- 0  # no one alive, no annuity income
  if (diaOwnerisHusband){
    diaIncomeM[states == 2] <- survivorPercentDIA    # only non-owner spouse alive, decrease payout by survBenefit %
    diaIncomeM[states == 3 | states == 1] <- 1       # both alive or only SPIA owner is alive, 100% annuity income
  } else {                                           # wife owns Spia
    diaIncomeM[states == 1] <- survivorPercentDIA  # only non-owner spouse alive, decrease payout by survBenefit %
    diaIncomeM[states == 3 | states == 2] <- 1      # both alive or only SPIA owner is alive, 100% annuity income
  }
  
  diaIncomeM <- diaIncomeM * diaAllocation * saveInitPort * diaPayout
  
  if (startAgeDIA - startAgeSimulation >= 1 ) diaIncomeM[,1:(startAgeDIA - startAgeSimulation)] <- 0  # zero out all columns (ages) prior to start date for DIA payouts
   
  if (inflationProtDIA == FALSE)
    diaIncomeM <- diaIncomeM / market.cumCsM # adjust future payments for inflation
  
  # what if dates already passed?
  return(diaIncomeM)
  
} # end of function buildDiaM

# test function

# diaIncomeM <- buildDiaM (states,100000,purchaseAgeDia=65,diaPayout=21420/100000,startAgeDIA=80,diaOwnerisHusband=TRUE,survivorPercentDIA=.5,inflationProtDIA=TRUE,market.cumCsM,husbandAge,wifeAge) 
  
  
  
  # Problem -- spiaAllocation has to be considered and doesn't work with states matrix.
  #  Probably same problem with DIAs.
###########################################


# Immediate Income Annuities
# 
# Strategy: Previous models built diaIncomeM matrix from an immediate income and/or a pension. 
# Separate annuity income into five matrices and combine them into diaIncomeM:
#   - Pension income
#   - an Immediate annuity
#   - a deferred income annuity
#   - one or two QLACS (per spouse)
#   - a HECM with tenure payments
# 
# The goal is to be able to inspect the matrices individually for correctness, to simplify the addition of
# future annuity products, tax analysis, or multiple annuities. Add the following functions:
# buildPensionM <- function(states,purchaseAmount,spiaPayout,startYear,owner,survivorPercent,inflationProt)
# buildDiaM <- function(states,purchaseAmount,purchaseAge,diaPayout,startYear,owner,household,survivorPercent,inflationProt)
# buildQlacMale <- function(states,purchaseAmount,diaPayout,startYear,owner,survivorPercent,inflationProt)
# buildQlacFemale <- function(states,purchaseAmount,diaPayout,startYear,owner,survivorPercent,inflationProt)
# buildHECMtenure <- function(states,purchaseAmount,hecmTenurePayout,startYear,owner,survivorPercent,inflationProt)


buildSpiaM <- function(spiaAllocation,spiaPayout,states,purchaseAgeSpia,spiaOwnerisHusband,survivorPercentSPIA,inflationProtSPIA,market.cumCsM,husbandAge,wifeAge,saveInitPort) {
  # Parameters: 
  # states -- states survivor matrix
  # spiaAllocation -- percent of initial portfolio to spend on SPIA (vector)
  # purchaseAgeSpia -- age at which SPIA will be purchased
  # spiaPayout -- annual payout percentage (see quotes at abaris.com)
  # startAgeSpia -- annuity owners age when payouts will begin
  # ownerisSpouseA -- TRUE or FALSE
  # survivorPercentSPIA -- % of payout paid to a surviving spouse
  # inflationProtSPIA -- payouts are inclation-protected, TRUE or FALSE
  # market.cumCsM -- inflation-rate factor table
  # husbandAge -- husband's age at start of simulations (column 1 of states table)
  # wifeAge -- wife's age at start of simulations (column 1 of states table)
  
  if (spiaOwnerisHusband) startAgeSimulation <- husbandAge else startAgeSimulation <- wifeAge
  startAgeSpia <- purchaseAgeSpia  # annuty is immediate
  spiaIncomeM <- states 
  cat("\nBuilding Immediate Annuity Income table")
  # cat("\nSPIA Owner  ",spiaOwnerisHusband)
  # cat("\nSurvivor Payment ",survivorPercentSPIA * spiaPayout)
  
  spiaIncomeM[states == 4 | states == 0] <- 0  # no one alive, no annuity income
  if (spiaOwnerisHusband){
  spiaIncomeM[states == 2] <- survivorPercentSPIA    # only non-owner spouse alive, decrease payout by survBenefit %
  spiaIncomeM[states == 3 | states == 1] <- 1       # both alive or only SPIA owner is alive, 100% annuity income
  } else {                                           # wife owns Spia
    spiaIncomeM[states == 1] <- survivorPercentSPIA  # only non-owner spouse alive, decrease payout by survBenefit %
    spiaIncomeM[states == 3 | states == 2] <- 1      # both alive or only SPIA owner is alive, 100% annuity income
  }
  
  spiaIncomeM <- spiaIncomeM * spiaAllocation * saveInitPort * spiaPayout
  
  if (startAgeSpia - startAgeSimulation >= 1 ) spiaIncomeM[,1:(startAgeSpia - startAgeSimulation)] <- 0  # zero out all columns (ages) prior to start date for SPIA payouts
   
  if (inflationProtSPIA == FALSE)
    spiaIncomeM <- spiaIncomeM / market.cumCsM # adjust future payments for inflation
  
  # what if dates already passed?
  return(spiaIncomeM)
  
} # end of function buildSpiaM

# test function

# spiaIncomeM <- buildSpiaM (spiaAllocation,spiaPayout,states,purchaseAgeSpia=65,spiaOwnerisHusband=TRUE,survivorPercentSPIA=.5,inflationProtSPIA=TRUE,market.cumCsM,husbandAge,wifeAge,saveInitPort) 
  
  
  
  # Input RISMAT date from Excel CSV file

csvInput <- read.csv(userDataFile,header=TRUE,stringsAsFactors = FALSE)
wifeAge <- as.numeric(csvInput[1])
husbandAge <- as.numeric(csvInput[2])
wifeClaimAge <- as.numeric(csvInput[3])
husbandClaimAge <- as.numeric(csvInput[4])
husbandHE <- as.logical(csvInput[5])
loEarnerOwnBenefit <- as.numeric(csvInput[6])
hiEarnerOwnBenefit <- as.numeric(csvInput[7])
ssB1 <- as.numeric(csvInput[8])
ssB2 <- as.numeric(csvInput[9])
ssB3 <- as.numeric(csvInput[10])
ssB4 <- as.numeric(csvInput[11])
ssB5 <- as.numeric(csvInput[12])
ssB6 <- as.numeric(csvInput[13])
initportfolio <- as.numeric(csvInput[14])
inflation <- as.numeric(csvInput[15])
inflationSd <- as.numeric(csvInput[16])
rf <- as.numeric(csvInput[17])
rp <- as.numeric(csvInput[18])
mrSd <- as.numeric(csvInput[19])
purchaseAgeSPIA <- as.numeric(csvInput[20])
annuityStartAge <- as.numeric(csvInput[21])
annPmt <- as.numeric(csvInput[22])
inflationProtSPIA <- as.logical(csvInput[23])
spiaOwnerisHusband <- as.logical(csvInput[24])
spiaPayout <- as.numeric(csvInput[25])
survivorPercentSPIA <- as.numeric(csvInput[26])
purchaseAgeDIA <- as.numeric(csvInput[27])
diaPayout <- as.numeric(csvInput[28])
startAgeDIA <- as.numeric(csvInput[29])
diaOwnerisHusband <- as.logical(csvInput[30])
survivorPercentDIA <- as.numeric(csvInput[31])
inflationProtDIA <- as.logical(csvInput[32])
initialSpend <- as.numeric(csvInput[33])
survivorExpense <- as.numeric(csvInput[34])
annualAdjust <- as.numeric(csvInput[35])
homeMarketVal <- as.numeric(csvInput[36])
housingAppreciation <- as.numeric(csvInput[37])
liborMean <- as.numeric(csvInput[38])
liborSigma <- as.numeric(csvInput[39])
initLoc <- as.numeric(csvInput[40])
initMort <- as.numeric(csvInput[41])
maxRate <- as.numeric(csvInput[42])
marginHECM <- as.numeric(csvInput[43])
percentMIP <- as.numeric(csvInput[44])
husbandPensionPayout <- as.numeric(csvInput[45])
husbandPensionStartAge <- as.numeric(csvInput[46])
survivorPercentHpension <- as.numeric(csvInput[47])
inflationProthPension <- as.numeric(csvInput[48])
wifePensionPayout <- as.numeric(csvInput[49])
wifePensionStartAge <- as.numeric(csvInput[50])
survivorPercentWpension <- as.numeric(csvInput[51])
inflationProtwPension <- as.numeric(csvInput[52])# Build Pension Income Matrix for Husband
# 
# Strategy: Previous models built diaIncomeM matrix from an immediate income and/or a pension. 
# Separate annuity income into five matrices and combine them into diaIncomeM:
#   - Pension income
#   - an Immediate annuity
#   - a deferred income annuity
#   - one or two QLACS (per spouse)
#   - a HECM with tenure payments
# 
# The goal is to be able to inspect the matrices individually for correctness, to simplify the addition of
# future annuity products, tax analysis, or multiple annuities. Add the following functions:
# buildPensionM <- function(states,purchaseAmount,spiaPayout,startYear,owner,survivorPercent,inflationProt)
# buildDiaM <- function(states,purchaseAmount,purchaseAge,diaPayout,startYear,owner,household,survivorPercent,inflationProt)
# buildQlacMale <- function(states,purchaseAmount,diaPayout,startYear,owner,survivorPercent,inflationProt)
# buildQlacFemale <- function(states,purchaseAmount,diaPayout,startYear,owner,survivorPercent,inflationProt)
# buildHECMtenure <- function(states,purchaseAmount,hecmTenurePayout,startYear,owner,survivorPercent,inflationProt)


buildPensionHusband <- function(husbandPensionPayout,states,husbandPensionStartAge,survivorPercentHpension,inflationProthPension,market.cumCsM,husbandAge) {
  # Parameters: 
  # husbandPensionPayout -- annual pension payout to husband
  # states -- states survivor matrix
  # husbandPensionStartAge -- husband age when payments begin
  # survivorPercentHpension -- % of payout paid to a surviving spouse
  # inflationProthPension -- payouts are inclation-protected, TRUE or FALSE
  # market.cumCsM -- inflation-rate factor table
  # husbandAge -- husband's age at start of simulations (column 1 of states table)
  
  hPensionIncomeM <- states 
  cat("\nBuilding Husband Pension Income table")
  
  hPensionIncomeM[states == 4 | states == 0] <- 0      # no one alive, no pension income
  hPensionIncomeM[states == 3 | states == 1] <- 1      # both alive, full pension income
  hPensionIncomeM[states == 2] <- survivorPercentHpension  # wife only alive, reduced pension income
 
  hPensionIncomeM <- husbandPensionPayout * hPensionIncomeM
  
  if (husbandPensionStartAge - husbandAge >= 1 ) hPensionIncomeM[,1:(husbandPensionStartAge - husbandAge)] <- 0  # zero out all columns (ages) prior to start date for SPIA payouts
   
  if (inflationProthPension == FALSE)
    hPensionIncomeM <- hPensionIncomeM / market.cumCsM # adjust future payments for inflation
  
  # what if dates already passed?
  return(hPensionIncomeM)
  
} # end of function buildPensionHusband

# Build Pension Income Matrix for Wife
# 
# Strategy: Previous models built diaIncomeM matrix from an immediate income and/or a pension. 
# Separate annuity income into five matrices and combine them into diaIncomeM:
#   - Pension income
#   - an Immediate annuity
#   - a deferred income annuity
#   - one or two QLACS (per spouse)
#   - a HECM with tenure payments
# 
# The goal is to be able to inspect the matrices individually for correctness, to simplify the addition of
# future annuity products, tax analysis, or multiple annuities. Add the following functions:
# buildPensionM <- function(states,purchaseAmount,spiaPayout,startYear,owner,survivorPercent,inflationProt)
# buildDiaM <- function(states,purchaseAmount,purchaseAge,diaPayout,startYear,owner,household,survivorPercent,inflationProt)
# buildQlacMale <- function(states,purchaseAmount,diaPayout,startYear,owner,survivorPercent,inflationProt)
# buildQlacFemale <- function(states,purchaseAmount,diaPayout,startYear,owner,survivorPercent,inflationProt)
# buildHECMtenure <- function(states,purchaseAmount,hecmTenurePayout,startYear,owner,survivorPercent,inflationProt)


buildPensionWife <- function(wifePensionPayout,states,wifePensionStartAge,survivorPercentWpension,inflationProthPension,market.cumCsM,wifeAge) {
  # Parameters: 
  # wifePensionPayout -- annual pension payout to husband
  # states -- states survivor matrix
  # wifePensionStartAge -- husband age when payments begin
  # survivorPercentWpension -- % of payout paid to a surviving spouse
  # inflationProthPension -- payouts are inclation-protected, TRUE or FALSE
  # market.cumCsM -- inflation-rate factor table
  # wifeAge -- husband's age at start of simulations (column 1 of states table)
  
  wPensionIncomeM <- states 
  cat("\nBuilding Husband Pension Income table")
  
  wPensionIncomeM[states == 4 | states == 0] <- 0      # no one alive, no pension income
  wPensionIncomeM[states == 3 | states == 1] <- 1      # both alive, full pension income
  wPensionIncomeM[states == 2] <- survivorPercentWpension  # wife only alive, reduced pension income
 
  wPensionIncomeM <- wifePensionPayout * wPensionIncomeM
  
  if (wifePensionStartAge - wifeAge >= 1 ) wPensionIncomeM[,1:(wifePensionStartAge - wifeAge)] <- 0  # zero out all columns (ages) prior to start date for SPIA payouts
   
  if (inflationProthPension == FALSE)
    wPensionIncomeM <- wPensionIncomeM / market.cumCsM # adjust future payments for inflation
  
  # what if dates already passed?
  return(wPensionIncomeM)
  
} # end of function buildPensionWife

