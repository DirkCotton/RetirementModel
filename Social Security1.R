socialsecurity <- function (wifeAge,husbandAge,wifeClaimAge,husbandClaimAge,husbandHE,states,leBenefit,heBenefit,ssBenefits,switchBenefit,printSummary) {
   
  ssBenefitsA <- 12 * ssBenefits    # annualize benefits
  cat("\n\nssBenefitsA")
  print(ssBenefitsA)
# Build Social Security income matrix
  
  cat(paste("\nBuilding Social Security Expected Income matrix.\n",sep=" "))
  socSecM <- matrix(0,nscen,nyrs) # create output matrix of zeroes, defaulting to no SS benefit
  nyrs <- length(states[1,])
  nscen <- length(states[,1])
  if (household == 1 & leBenefit == 0) {  # set benefits the same for one-person household
    leBenefit <- heBenefit
  } else {
    if (household == 1 & heBenefit == 0)  heBenefit <- leBenefit
  }
  
# STATES  ###################
 
  claimStatesH <- socSecM  # initial vector of claim states. 0 = neither has claimed, 2 = only low earner has claimed,
  #                              1 = only high earner has claimed, 3 = both have claimed
  claimStatesW <- socSecM 
  claimStatesB <- socSecM
  
  if (ssBenefitsA[2,1] > ssBenefitsA[2,2]) {  # husband is higher earner
    leclaim <- 1
    heclaim <- 2
  } else {
    leclaim <- 2
    heclaim <- 1
  }
  
  cat("\n\nheClaim is ",heclaim,"leClaim is ",leclaim )

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
      switchBenefit <- max(ssBenefitsA[age62,loEarner],ssBenefitsA[age62,loEarner] + .5 * (ssBenefitsA[FRA,hiEarner] - ssBenefitsA[FRA,loEarner])) - ssBenefitsA[age62,loEarner]  # if wife claimed early her spousal benefit = existing claimed benefit + half difference in their FRA's
 
    } 
    if ((wifeClaimAge == 67 | wifeClaimAge == 66) & (husbandClaimAge != 67 & husbandClaimAge != 66)) {  # wife must have claimed at FRA and husband at 70
      switchBenefit <- max(0.5*ssBenefitsA[FRA,hiEarner],ssBenefitsA[FRA,loEarner]) - ssBenefitsA[FRA,loEarner]
      }
  
    if (wifeClaimAge == 62) leBenefit <- ssBenefitsA[age62,loEarner]
    if (wifeClaimAge == 67 | wifeClaimAge == 66) {
      leBenefit <- ssBenefitsA[FRA,loEarner]
      cat("\nClaim Age for wife = 67 or 67 (FRA) and leBenefit=",leBenefit)
      }
    if (wifeClaimAge == 70) leBenefit <- ssBenefitsA[age70,loEarner]
    if (husbandClaimAge == 62) heBenefit <- ssBenefitsA[age62,hiEarner]
    if (husbandClaimAge == 67 | husbandClaimAge == 66) heBenefit <- ssBenefitsA[FRA,hiEarner]
    if (husbandClaimAge == 70) heBenefit <- ssBenefitsA[age70,hiEarner]
    
    
    
    
    if (husbandAge > husbandClaimAge) husbandClaimAge <- husbandAge   # husband has already claimed benefits before current age
    if (wifeAge > wifeClaimAge) wifeClaimAge <- wifeAge               # wife has already claimed benefits before current age
    
    if (husbandAge != 0) {
      claimStatesH[,(husbandClaimAge - husbandAge + 1):nyrs] <- leclaim # set husband has claimed
    } else {
      claimStatesH <- 0
    }
    if (wifeAge != 0) {
      claimStatesW[,(wifeClaimAge - wifeAge + 1):nyrs] <- heclaim # set wife has claimed
    } else {
      claimStatesW <- 0
    }
    
  leSpousal <- max(heBenefit/2, leBenefit)   # low earners spousal benefit = 1/2 high earner's FRA
                              
   # if wife (LE) claims before FRA and husband hasn't claimed, she can only claim her own benefit 
   ###### check following to make gender differences work---------------------NEED TO ADD FRA Calculation
  if ((husbandClaimAge + (husbandAge-wifeAge) > wifeClaimAge) & (wifeClaimAge == 67 | wifeClaimAge == 67)) leSpousal <- 0
  cat("\n***husbandClaimAge",husbandClaimAge,"husband age",husbandAge," wife age=",wifeAge)
  leBenefit <- max(leBenefit, .5*ssBenefitsA[FRA,hiEarner]) # low earner gets larger of own and spousal benefits
  leSurvivors <- max(leBenefit,heBenefit)  # low earners Survivors benefit
 
  
  cat("\n\nleBenefit is ",leBenefit," heBenefit is ",heBenefit, "leSurvivors is ",leSurvivors)
  # build matrix of SS payments based on claiming status (rows) and mortality state (cols) 
  combinedStates <- matrix (0,4,5)
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

  write.csv(combinedStates,"~/desktop/combinedStates")
  write.csv(claimStatesH,"~/desktop/claimStatesH")
  write.csv(claimStatesH,"~/desktop/claimStatesW")
  write.csv(claimStatesH,"~/desktop/claimStatesB")
  
 resultsM <- matrix(as.vector(combinedStates)[(states)*4+(claimStatesB+1)],nscen,nyrs)
 
 # if (printSummary == TRUE)  {
 cat(paste("\nLow Earner's own annual benefit is =",leBenefit,sep=" "))
 cat(paste("\nHigh Earner's own annual benefit is =",heBenefit,sep=" "))
 cat(paste("\nLow Earner's survivors benefit is =",leSurvivors,sep=" "))
 cat(paste("\nHusband plans to claim at age ",husbandClaimAge,sep=" "))
 cat(paste("\nWife plans to claim at age ",wifeClaimAge,sep=" "))
 # if (husbandHE)  cat(paste("\nHusband has higher benefit.",sep=" "))
 #      else cat(paste("\nWife has higher benefit.",sep=" "))
 # }
 
  return(resultsM)
}
    