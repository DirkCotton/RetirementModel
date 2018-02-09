# Find the optimal asset allocation for all scenarios



for (s in 1:20) {          # test all scenarios
  
  portSpend <- spendFromPortfolio[s,]
  ea <- equityAllocation[s]  # equity allocation for this scenario simulation
  mktret <- market.rmsM[s,] # market growth factor
  libor <- liborM[s,]
  
  
  yrRetire <- which(states[1,] == max(states[1,])) - 1  #years in retirement for this scenario
  for (assAll in seq(0,1,.05)) {   # for all asset allocations
    ip <- initportfolio[1]  # inital portfolio value for this scenario
    for (yrR in (1:yrRetire)) {   # for all years of retirement
      #   cat("\nip= ",ip)
      
      ip <- (ip - portSpend[yrR]) * ( (mktret[yrR] * assAll) + ((1 +libor[yrR]) * (1 - assAll)) )
      
    }
    cat("\nFor ",assAll * 100,"% equity allocation TPV = ",ip)
    # pb[] <- ip - portSpend
    
  }   # end of years in this scenario
  
}    # end of scenarios

