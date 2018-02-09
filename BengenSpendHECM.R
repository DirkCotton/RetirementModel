bengenSpendHECM <- function (marketGrowth,initportfolio,spendFromPortfolio,initLoc,initMort,liborM,marginHECM,percentMIP,maxRate,socsec,annuityInc,sum=TRUE)
{

  cat("\n######made it to bengenSpendHecm.")
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
  
  years <- length(marketGrowth[1,])
  scenarios <- length(marketGrowth[,1])
  
  portfolioBalance <<- matrix(0,scenarios,years + 1)   # initialize investment portfolio balance matrix to zeros, add a year zero
  mortgageBalance <- portfolioBalance             # initialize mortgage balance matrix to zeros, add a year zero
  creditLimit <<- portfolioBalance                 # initialize HECM credit limit matrix to zeroes, add a year zero
  hecmSpend <<- portfolioBalance
  ################--------------- is growth rate correct for assumed percent of equities??????
  
  # create a year zero in all matrices for initial conditions
  spendPort <- matrix(0,scenarios,years + 1)
  spendPort <- cbind(spendPort[,1],spendFromPortfolio)
  portfolioBalance[,1] <- initportfolio        # set year zero portfolio balance
  mortgageBalance[,1] <- initMort     # set year zero reverse mortgage balance
        # set year zero reverse mortgage balance
  marketGrowth <- cbind(marketGrowth[,1],marketGrowth)  # add year zero column to maketGrowth matrix
  marketGrowth[,1] <- 1               # set year zero market growth rate to 1
  
  liborM <- cbind(liborM[,1],liborM) # add year zero column to Libor matrix
  liborM[,1] <- 0                     # set year zero Libor rate to zero
  portfolioDemand <- rep(0,years)     # initial  vector
  creditLimit[,1] <- rep(initLoc,scenarios)
  
  # Set HECM interest rates
  rateHECM <- liborM + marginHECM + percentMIP
  rateHECM[rateHECM > maxRate] <- maxRate   # make sure no HECM rates exceed lifetime max
  
  for (yearOfRetirement in 2:(years + 1)) {    # note: first column is now year zero
    
    
    # set portfolioDemand <- expenses - Social Sec Income - Annuity Income, spending matrix alread net of those two when passed
    portfolioDemand <- spendPort[,yearOfRetirement]
    # can the portfolio cover the expenses?
    # if portfolioDemand < last year's ending portfolio balance
    # then spend down portfolio
    #   else
    #     spend down remaining portfolio balance
    portfolioBalance[,yearOfRetirement] <- pmax(0,portfolioBalance[,(yearOfRetirement - 1)] - ( pmin(portfolioBalance[,(yearOfRetirement - 1)],portfolioDemand) * marketGrowth[,yearOfRetirement] ))
    spendPort[,yearOfRetirement] <- pmin(portfolioBalance[,(yearOfRetirement - 1)],portfolioDemand)
      
    #     set hecmSpend <- portfolio demand  - remaining portfolio balance
    hecmSpend[,yearOfRetirement] <- pmin(creditLimit[,(yearOfRetirement-1)],pmax(0, portfolioBalance[,(yearOfRetirement - 1)] - spendPort[,yearOfRetirement]))
  #  hecmSpend[,yearOfRetirement] <- 100000
    
     if (80313 %in% trunc(portfolioBalance[,(yearOfRetirement - 1)]) ){
      h <- pmax(0,portfolioBalance[,(yearOfRetirement - 1)] - spendPort[,yearOfRetirement])
      cat("\n h is ",h) 
      cat("\n\n hecmSpend:",hecmSpend[,yearOfRetirement])
      cat("\n\n last year's creditLimit ",creditLimit[,(yearOfRetirement-1)])
      cat("\n\n Spend from Portfolio ",spendPort[,(yearOfRetirement)])
      cat("\n\n last year's port bal ",portfolioBalance[,(yearOfRetirement - 1)])
      cat("\n\n pmax(0,spendPort[,yearOfRetirement]) ",pmax(0,spendPort[,yearOfRetirement]))
      
      cat("\n\n pmin(creditLimit[,(yearOfRetirement-1)],pmax(0,spendPort[,yearOfRetirement] ", pmin(creditLimit[,(yearOfRetirement-1)],pmax(0,spendPort[,yearOfRetirement])))                                             
      
      }
    
    #     if HECM demand < HECM line of credit
    #     then spend down HECM loc
    #       else
    #         spend down remaining HECM loc
    
    creditLimit[,yearOfRetirement] <- max(0,creditLimit[,(yearOfRetirement-1)] - pmin(creditLimit[,(yearOfRetirement-1)],pmax(0,spendPort[,yearOfRetirement] - portfolioBalance[,(yearOfRetirement - 1)])))
    # increase credit limit per HECM contract                                                                              
    creditLimit[,yearOfRetirement] <- creditLimit[,yearOfRetirement] * (1 + rateHECM[,yearOfRetirement])
    # increase reverse mortgage balance
    mortgageBalance[,(yearOfRetirement)] <- min(creditLimit[,(yearOfRetirement-1)],mortgageBalance[,(yearOfRetirement -1)] + pmin(creditLimit[,(yearOfRetirement-1)],pmax(0,spendPort[,yearOfRetirement] - portfolioBalance[,(yearOfRetirement - 1)])) )
    # add reverse mortgage interest charge  
    mortgageBalance[,(yearOfRetirement)] <- min(creditLimit[,(yearOfRetirement-1)],mortgageBalance[,(yearOfRetirement)] * (1 + rateHECM[,yearOfRetirement]) )
    
  }    # end for-loop
cat("\nHECM Spend ")
print(hecmSpend)
  return (list("balances" = portfolioBalance,"loc" = creditLimit,"locBal"= mortgageBalance,"spf"=spendPort,"hecmD"=hecmSpend))
  
  }
  