
# Build Social Security income matrix
  
  cat(paste("\nBuilding Social Security Expected Income matrix.\n",sep=" "))
  
 #  socSec <- socialsecurity(wifeAge,husbandAge,husbandIsHE,wifeClaimAge,husbandClaimAge,states,loEarnerOwnBenefit,hiEarnerOwnBenefit,nscen,nyrs)
  
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

  claimStatesH[,(husbandClaimAge - husbandAge + 1):nyrs] <- leclaim # set husband has claimed
  claimStatesW[,(wifeClaimAge - wifeAge + 1):nyrs] <- heclaim # set wife has claimed
  claimStatesB <- claimStatesH + claimStatesW   # created state=3 when both have claimed
  
  cat("\nClaim States Both")
  print(claimStatesB)

  leSpousal <- hiEarnerOwnBenefit/2  # low earners spousal benefit
  loEarnerOwnBenefit <- max(loEarnerOwnBenefit,leSpousal) # low earner gets larger of own and spousal benefits
  leSurvivors <- max(loEarnerOwnBenefit,hiEarnerOwnBenefit)  # low earners Survivors benefit
 
  # build matrix of SS payments based on claiming status (rows) and mortality state (cols) 
  combinedStates <<- matrix (0,4,5)
  colnames(combinedStates) <- c("Both Dead","Only L.E. Alive","Only H.E. Alive","Both Alive","Both Dead")
  rownames(combinedStates) <- c("Neither Claimed","Only H.E. Claimed","Only L.E. Claimed","Both Claimed")
  combinedStates[,"Both Dead"] <- 0 # both dead, no benefits
  combinedStates["Only L.E. Claimed","Only H.E. Alive"] <- 0 # neither claimed, no benefits
  combinedStates["Only H.E. Claimed","Only L.E. Alive"] <- leSurvivors # Low earner should claim. no advantage to waiting
  combinedStates["Only H.E. Claimed","Both Alive"] <- hiEarnerOwnBenefit 
  combinedStates["Only H.E. Claimed","Only H.E. Alive"] <- hiEarnerOwnBenefit 
  combinedStates["Both Claimed","Only H.E. Alive"] <- hiEarnerOwnBenefit 
  combinedStates["Only L.E. Claimed","Both Alive"] <-loEarnerOwnBenefit  
  combinedStates["Only L.E. Claimed","Only L.E. Alive"] <-leSurvivors
  combinedStates["Both Claimed","Both Alive"] <-loEarnerOwnBenefit + hiEarnerOwnBenefit
  combinedStates["Both Claimed","Only L.E. Alive"] <- leSurvivors
  
  cat("\nCombined States")
  print(combinedStates)
  
  
# build a matrix, resultsM, and fill with the correct benefit from combinedStates matrix with
# the indices of the correct payment from combinedStates[states,claimStatesB]. For example,
# fill resultsM[a,b] from the payment at combinedStates[states,claimStatesB].
  
resultsM <- matrix(as.vector(combinedStates)[(states)*4+(claimStatesB+1)],nscen,nyrs)


 if (printSummary == TRUE)  {
 cat(paste("\nLow Earner's own annual benefit is =",loEarnerOwnBenefit,sep=" "))
 cat(paste("\nHigh Earner's own annual benefit is =",hiEarnerOwnBenefit,sep=" "))
 cat(paste("\nLow Earner's survivors benefit is =",leSurvivors,sep=" "))
 cat(paste("\nHusband plans to claim at age ",husbandClaimAge,sep=" "))
 cat(paste("\nWife plans to claim at age ",wifeClaimAge,sep=" "))
 if (husbandHE)  cat(paste("\nHusband has higher benefit.",sep=" "))
      else cat(paste("\nWife has higher benefit.",sep=" "))
 }


    