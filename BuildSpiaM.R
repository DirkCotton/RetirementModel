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


  
  
  