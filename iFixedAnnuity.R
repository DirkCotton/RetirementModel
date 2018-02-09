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


     