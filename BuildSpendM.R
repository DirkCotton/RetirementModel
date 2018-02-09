buildSpendM <- function (states, initSpend, annualAdjust,survivorExpense,numberRetiring) {
  
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
  if (numberRetiring == 2) {
    expenseFactor[expenseFactor == 1 | expenseFactor == 2] <- survivorExpense  # one spouse alive, reduce expenses
  } else {
    expenseFactor[expenseFactor == 1 | expenseFactor == 2] <- 1  # one person retiring, no expense reduction
  }
  
  expenseFactor[expenseFactor == 3] <- 1              # factor = 1, no expense reduction
  

  spendM <- spendM * expenseFactor
  
  return(spendM)
}


