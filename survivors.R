# Function to apply age adjustments table to maximum survivors benefit

ageAdjusted <- function (benefit,ageSurvivorBenefitClaimed) {
  adjustments <- rep(0,21)
  adjustments[0:11] <- .715 # ages 50-60
  adjustments[12:17] <- c(.7558, .7965, .8372, .8779, .9186, .9593)
  adjustments[18:21] <- 1
  if (ageSurvivorBenefitClaimed < 50) {
    adjBenefit <- 0 
  } else {
    if (ageSurvivorBenefitClaimed > 70) {
      adjBenefit <- benefit 
    } else {
      adjBenefit <- benefit * adjustments[ageSurvivorBenefitClaimed]
    }
    return(adjBenefit)
  }
} # end of function ########################################

# Function to apply survivor claims to socSecM matrix
survivorClaims <- function (states,socSecM) {

wifeHasClaimed <- FALSE
If (wifeClaimAge >= wifeCurrentAge) wifeHasClaimed <- TRUE 

husbandHasClaimed <- FALSE
If (husbandClaimAge >= husbandCurrentAge) husbandHasClaimed <- TRUE

# if (wifeDeceased) {
#   if  (!wifeHasClaimed & wifeDiedBeforeFRA) {
#     husbandSurvivorBenefit <- ageAdjusted(wifeFRA,husbandAgeatWifesDeath)
#   } else {
#     if  (!wifeHasClaimed & !wifeDiedBeforeFRA) {
#       husbandSurvivorBenefit <- ageAdjusted(wifesClaimedBenefit,husbandsAgeatWifesDeath)
#     } 
#   } else  {
#     if (wifeHasClaimed) {
#       # Step #1: Determine MAXIMUM Survivor Benefit
#       maxHusbandSurvivorBenefit <- max (wifeClaimedBenefit,0.825 * wifeClaimedBenefit)
#       Step #2: Age-Based Reduction to Maximum Benefit
#       husbandSurvivorBenefit <- ageAdjusted(maxHusbandSurvivorBenefit,husbandAgeatWifesDeath)
#     }
#   }   
#   
#   
# } else {    # wife not deceased
#   if (husbandDeceased) {
#     if  (!husbandHasClaimed & husbandDiedBeforeFRA) {
#       wifeSurvivorBenefit <- ageAdjusted(husbandFRA,wifeAgeatHusbandsDeath)
#     } else {
#       if  (!husbandHasClaimed & !husbandDiedBeforeFRA) {
#         wifeSurvivorBenefit <- ageAdjusted(husbandsClaimedBenefit,wifeAgeatHusbandsDeath)
#       } else  {
#         if (husbandHasClaimed) }
#       # Step #1: Determine MAXIMUM Survivor Benefit
#       maxWifeSurvivorBenefit <- max (husbandClaimedBenefit,0.825 * husbandFRA)
#       Step #2: Age-Based Reduction to Maximum Benefit
#       wifeSurvivorBenefit <- ageAdjusted(maxWifeSurvivorBenefit,wifeAgeatHusbandsDeath)
#     }
#   }                                                                                     
# }

return (socSecM)


# Earnings Limit
# If you are under full retirement age you are limited to $16,920 in wages or net earnings from self employment. If you exceed that limit, your benefit will be reduced by $1 for every $2 you go over. The one exception is the calendar year you turn full retirement age. For that period, your limit is a much higher $44,880. The amount they’ll reduce your benefit by is more generous as well.
# Once you are full retirement age, there is no limit to the amount you can earn while drawing Social Security.
####################################

# Function to find first year of survivors for each scenario

findSurvivor <- function (states) {
  
  # the first year of a scenario for survivor 1 in each scenario is the first year state = 3 is followed by state = 1 
  # the first year of a scenario for survivor 2 in each scenario is the first year state = 3 is followed by state = 2
  # return a matrix the same size as states filled with zeroes except for 1 and 2 signififying survivor start state
  
  survivorStates <- states
  first1 <- apply(df,1,match,x=1)  # find first 1 (vector)
  first2 <- apply(df,1,match,x=2)   # find first 2 (vector)
   <- min(first)
  
}

# Combined States

a <- matrix(0,3,10)
a[1,] <- c(3,3,3,1,1,1,4,0,0,0)
a[2,] <- c(3,3,2,2,2,2,4,0,0,0)
a[3,] <- c(3,3,3,3,3,4,0,0,0,0)
wifeDeceased <- rep(T,3)
husbandDeceased <- rep(T,3)

survivorStates <- a # states



first1 <- which(a[1,] == 1) [1]
first2 <- which(a[1,] == 2) [1]

#-------------------
# determine if this is a one person household or couple
singleHousehold <- F   
if (is.na(which(states == 1) [1]) | is.na(which(states == 2) [1])) singleHousehold <- T

survivorStates <- matrix(0,nscen,nyrs)
survivorBenefits <- matrix(0,nscen,nyrs)

# get rid of all 3 and 4 states in states matrix
survivorStates[states == 3 | states == 4] <- 0  # no survivors in these states, only in states 1 or 2

wifeFRA <- 66    
husbandFRA <- 66

# build vector of wife and husband age for each year of retirement (column)
wifeFutureAge <- seq(wifeAge,(wifeAge + nyrs - 1))
husbandFutureAge <- seq(husbandAge,(husbandAge + nyrs - 1))

# build vectors of husband and wife year of death from states table
wifeYearofDeath <- rep(0,nscen)
husbandYearofDeath <- rep(0,nscen)

for (i in 1:nscen) {
wifeYearofDeath[i] <- which(states[i,] == 1) [1] - 1
wifeYearofDeath[is.na(wifeYearofDeath)] <- which(states[i,] == 4) [1]
husbandYearofDeath[i] <- which(states[i,] == 1) [1] - 1
husbandYearofDeath[is.na(husbandYearofDeath)] <- which(states[i,] == 4) [1]
}

# wifeDeceased & !wifeHasClaimed & wifeDiedBeforeFRA survivor state = 5
survivorStates[(survivorStates == 1) & (wifeYearofDeath < wifeClaimAge) & (wifeYearofDeath < wifeFRA )]  <- 5

# wifeDeceased & !wifeHasClaimed & !wifeDiedBeforeFRA survivor state = 6
survivorStates[(survivorStates == 1) & (wifeYearofDeath < wifeClaimAge) & (wifeYearofDeath >= wifeFRA )]  <- 6

# wifeDeceased & wifeHasClaimed state = 7
survivorStates[(survivorStates == 1) & (wifeYearofDeath < wifeClaimAge) ]  <- 7

# husbandDeceased & !husbandHasClaimed & husbandDiedBeforeFRA survivor state = 8
survivorStates[(survivorStates == 2) & (husbandYearofDeath < husbandClaimAge) & (husbandYearofDeath < husbandFRA )]  <- 8

# husbandDeceased & !husbandHasClaimed & !husbandDiedBeforeFRA survivor state = 9
survivorStates[(survivorStates == 3) & (husbandYearofDeath < husbandClaimAge) & (husbandYearofDeath >= husbandFRA )]  <- 9

# husbandDeceased & husbandHasClaimed state = 10
survivorStates[(survivorStates == 4) & (husbandYearofDeath < husbandClaimAge) & (husbandYearofDeath >= husbandFRA )]  <- 10

# set husband survivor benefit for state 5 wifeDeceased & !wifeHasClaimed & wifeDiedBeforeFRA 
survivorBenefits[survivorStates == 5]  <- ageAdjusted(wifeFRA,husbandAgeatWifesDeath)

# set husband survivor benefit for state 6 wifeDeceased & !wifeHasClaimed & !wifeDiedBeforeFRA
survivorBenefits[survivorStates == 6] <- ageAdjusted(wifesClaimedBenefit,husbandsAgeatWifesDeath)

# set husband survivor benefit for state 7 wifeDeceased & wifeHasClaimed state = 7
# Step #1: Determine MAXIMUM Survivor Benefit
maxHusbandSurvivorBenefit <- max (wifeClaimedBenefit,0.825 * wifeClaimedBenefit)
# Step #2: Age-Based Reduction to Maximum Benefit
survivorBenefits[survivorStates == 7]  <- ageAdjusted(maxHusbandSurvivorBenefit,husbandAgeatWifesDeath)

# set wife survivor benefit for state 8 husbandDeceased & !husbandHasClaimed & husbandDiedBeforeFRA
survivorBenefits[survivorStates == 8] <- ageAdjusted(husbandFRA,wifeAgeatHusbandsDeath)

# set wife survivor benefit for state 9 husbandDeceased & !husbandHasClaimed & !husbandDiedBeforeFRA
survivorBenefits[survivorStates == 9] <- ageAdjusted(husbandsClaimedBenefit,wifeAgeatHusbandsDeath)

# set wife survivor benefit for state 10 husbandDeceased & husbandHasClaimed 
# Step #1: Determine MAXIMUM Survivor Benefit
maxWifeSurvivorBenefit <- max (husbandClaimedBenefit,0.825 * husbandFRA)
# Step #2: Age-Based Reduction to Maximum Benefit
survivorBenefits[survivorStates == 10] <- ageAdjusted(maxWifeSurvivorBenefit,wifeAgeatHusbandsDeath)

return (socSecM)
}

# Earnings Limit
# If you are under full retirement age you are limited to $16,920 in wages or net earnings from self employment. If you exceed that limit, your benefit will be reduced by $1 for every $2 you go over. The one exception is the calendar year you turn full retirement age. For that period, your limit is a much higher $44,880. The amount they’ll reduce your benefit by is more generous as well.
# Once you are full retirement age, there is no limit to the amount you can earn while drawing Social Security.
####################################

  