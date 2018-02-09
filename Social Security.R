socialsecurity <- function (household,wifeAge,husbandAge,wifeDOB,husbandDOB,wifeClaimAge,husbandClaimAge,husbandHE,states,ssBenefits,printSummary) {
   
  cat("\nBuilding Social Security Benefits Matrix.")
  
# Build a table of benefits for each spouse at each claiming age based on the FRA benefit amount for both
  
  wifeFRAamount <- ssBenefits[2,1]
  husbandFRAamount <- ssBenefits[2,2]
  
  fraTable <- read.csv("FRA by Birth Year.csv",header=F)
  
  fraAges2 <- findFRA(fraTable,wifeDOB,husbandDOB)  # find FRA Ages
  wifeFRAage <- fraAges$wifeFRAage
  husbandFRAage <- fraAges$husbandFRAage
  
  ssClaimTable <- buildSocSec(wifeFRAamount,husbandFRAamount,wifeFRAage,husbandFRAage) # ssClaimTable is table of claimg by year
    
# Change all "4" states to zero, since no one is alive in 4   
  ssClaimStates <- states
  ssClaimStates[ssClaimStates == 4] <- 0
  ssBenefitsM <- states
  ssBenefitsM <- matrix(0,length(states[,1]),length(states[1,]))     # create matrix nscen by nyrs and fill with zeroes
  
  # if Household size = 1 Replace all "3" states (both alive) with combined claimed benefits
    # Return resultsM
  if (household == 2) {
    # Calculate survivor benefits, annualize them, and replace all "1" and "2" states with survivor benefits
    survivorBenefits <- survivorClaims(states,ssBenM,nscen=length(states[,1]),nyrs=length(states[1,]),wifeAge,husbandAge,wifeFRAage,husbandFRAage,wifeClaimAge,husbandClaimAge) 
    ssBenefitsM[ssClaimStates == 3] <- ssClaimTable[wifeClaimAge - 61,2] + ssClaimTable[husbandClaimAge - 61,3]
    ssBenefitsM <- ssBenefitsM + survivorBenefits
      
    } else {   # household size = 1 or is incorrect for states table
      if (max(ssClaimStates == 1)) {  # household is a single male
        ssBenefitsM[ssBenefitsM == 1] <- ssClaimTable[husbandClaimAge - 61,3] 
      } else {
        if (max(ssClaimStates == 2)) {  # household is a single female
          ssBenefitsM[ssBenefitsM == 2] <- ssClaimTable[wifeClaimAge - 61,2] 
        } else {
          cat("\n#### ERROR States = 3 exist in a one-person household states table.")
        }
      } 
    }
  
  return(ssBenefitsM)  
} 

  
  

 
    
    