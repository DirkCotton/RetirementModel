# function to build table of survivor benefits NOTE: don't call if household size = 1

survivorClaims <- function (states,ssBenM,nscen,nyrs,wifeAge,husbandAge,wifeFRAage,husbandFRAage,wifeClaimAge,husbandClaimAge) {

  # adjustments to claim
  if (husbandClaimAge < 60 | husbandClaimAge >70 | wifeClaimAge < 60 | wifeClaimAge > 70) cat ("\n***SOCIAL SECURITY CLAIMING AGE IS OUT OF RANGE FROM 60-70.")
  adjustments <- rep(0,11)
  adjustments[1] <- .715 # ages 60 - 120
  adjustments[2:7] <- c(.7558, .7965, .8372, .8779, .9186, .9593)
  adjustments[8:61] <- 1
  
wifeFRAamount <- ssBenM[wifeFRAage - 62 + 1,2]
husbandFRAamount <- ssBenM[husbandFRAage - 62 + 1,3]

wifeClaimedBenefit <- ssBen[wifeClaimAge - 61,2]
husbandClaimedBenefit <- ssBen[husbandClaimAge - 61,3]

survivorStatesM <- states
# get rid of all 3 and 4 states in survivor states matrix
survivorStatesM[states == 3 | states == 4] <- 0  # no survivors in these states, only in states 1 or 2
survivorBenefitsM <- matrix(0,nscen,nyrs)

survivorStatesM[states == 3 | states == 4] <- 0  # no survivors in these states, only states 0, 1 or 2 remain

# build vector of wife and husband age for each year of retirement (column)
wifeFutureAge <- seq(wifeAge,(wifeAge + nyrs - 1))
husbandFutureAge <- seq(husbandAge,(husbandAge + nyrs - 1))
wifeAgeatHusbandDeath <- rep(0,nscen)
husbandAgeatWifeDeath <- rep(0,nscen)

# build vectors of husband and wife age at death from states table
wifeAgeatDeath <- rep(0,nscen)
husbandAgeatDeath <- rep(0,nscen)
for (i in 1:nscen) {
  wifeAgeatDeath[i] <- which(states[i,] == 1) [1] - 1
  wifeAgeatDeath[is.na(wifeAgeatDeath)] <- which(states[i,] == 4) [1] - 1
  wifeAgeatDeath[i] <- wifeAgeatDeath[i] + wifeAge - 1
  husbandAgeatWifeDeath[i] <- wifeAgeatDeath[i] + (husbandAge - wifeAge)
  husbandAgeatDeath[i] <- which(states[i,] == 2) [1] - 1
  husbandAgeatDeath[is.na(husbandAgeatDeath)] <- which(states[i,] == 4) [1] - 1
  husbandAgeatDeath[i] <- husbandAgeatDeath[i] + husbandAge - 1
  wifeAgeatHusbandDeath[i] <- husbandAgeatDeath[i] + (husbandAge - wifeAge)
}

##############
# create a state matrix, survivorStatesM, based on which spouse died, whether they had claimed, and whether they died before their FRA
##############

# wifeDeceased & !wifeHasClaimed & wifeDiedBeforeFRA survivor state = 5
survivorStatesM[(survivorStatesM == 1) & (wifeAgeatDeath <= wifeClaimAge) & (wifeAgeatDeath < wifeFRAage )]  <- 5

# wifeDeceased & !wifeHasClaimed & !wifeDiedBeforeFRA survivor state = 6
survivorStatesM[(survivorStatesM == 1) & (wifeAgeatDeath <= wifeClaimAge) & (wifeAgeatDeath >= wifeFRAage )]  <- 6

# wifeDeceased & wifeHasClaimed state = 7
survivorStatesM[(survivorStatesM == 1) & (wifeAgeatDeath > wifeClaimAge) ]  <- 7

# husbandDeceased & !husbandHasClaimed & husbandDiedBeforeFRA survivor state = 8
survivorStatesM[(survivorStatesM == 2) & (husbandAgeatDeath <= husbandClaimAge) & (husbandAgeatDeath < husbandFRAage )]  <- 8

# husbandDeceased & !husbandHasClaimed & !husbandDiedBeforeFRA survivor state = 9
survivorStatesM[(survivorStatesM == 2) & (husbandAgeatDeath <= husbandClaimAge) & (husbandAgeatDeath >= husbandFRAage )]  <- 9

# husbandDeceased & husbandHasClaimed state = 10
survivorStatesM[(survivorStatesM == 2) & (husbandAgeatDeath > husbandClaimAge)]  <- 10

##############
# set survivorBenefitsM to the correct surviors benefits based on the claim states for that scenario and year
##############

# set husband survivor benefit for state 5 wifeDeceased & !wifeHasClaimed & wifeDiedBeforeFRA 
survivorBenefitsM[survivorStatesM == 5]  <- wifeFRAamount * adjustments[husbandAgeatWifeDeath - 59]


# set husband survivor benefit for state 6 wifeDeceased & !wifeHasClaimed & !wifeDiedBeforeFRA
survivorBenefitsM[survivorStatesM == 6] <- wifeClaimedBenefit * adjustments[husbandAgeatWifeDeath - 59]

# set husband survivor benefit for state 7 wifeDeceased & wifeHasClaimed state = 7
# Step #1: Determine MAXIMUM Survivor Benefit
maxHusbandSurvivorBenefit <- max (wifeClaimedBenefit,0.825 * wifeClaimedBenefit)
# Step #2: Age-Based Reduction to Maximum Benefit
survivorBenefitsM[survivorStatesM == 7]  <- maxHusbandSurvivorBenefit * adjustments[husbandAgeatWifeDeath - 59]

# set wife survivor benefit for state 8 husbandDeceased & !husbandHasClaimed & husbandDiedBeforeFRA
survivorBenefitsM[survivorStatesM == 8] <- husbandFRAamount * adjustments[wifeAgeatHusbandDeath - 59]

# set wife survivor benefit for state 9 husbandDeceased & !husbandHasClaimed & !husbandDiedBeforeFRA
survivorBenefitsM[survivorStatesM == 9] <- husbandClaimedBenefit * adjustments[wifeAgeatHusbandDeath - 59]

# set wife survivor benefit for state 10 husbandDeceased & husbandHasClaimed
# Step #1: Determine MAXIMUM Survivor Benefit
maxWifeSurvivorBenefit <- max (husbandClaimedBenefit,0.825 * husbandFRAamount)
# Step #2: Age-Based Reduction to Maximum Benefit
survivorBenefitsM[survivorStatesM == 10] <- maxWifeSurvivorBenefit * adjustments[wifeAgeatHusbandDeath - 59]

parms <- list("survivorBenefitsM" = survivorBenefitsM,"survivorStatesM"=survivorStatesM)
return (parms)
}

# Earnings Limit
# If you are under full retirement age you are limited to $16,920 in wages or net earnings from self employment. If you exceed that limit, your benefit will be reduced by $1 for every $2 you go over. The one exception is the calendar year you turn full retirement age. For that period, your limit is a much higher $44,880. The amount they’ll reduce your benefit by is more generous as well.
# Once you are full retirement age, there is no limit to the amount you can earn while drawing Social Security.
####################################


# Test
wifeFRAage <- 66
husbandFRAage <- 67
ssTestM <- survivorClaims(states,ssBenM,nscen,nyrs,wifeAge,husbandAge,wifeFRAage,husbandFRAage,wifeClaimAge,husbandClaimAge) 
  

