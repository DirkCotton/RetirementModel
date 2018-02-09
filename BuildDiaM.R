# Deferred Income Annuities
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


buildDiaM <- function(states,diaPurchase,purchaseAgeDia,diaPayout,startAgeDIA,diaOwnerisHusband,survivorPercentDIA,inflationProtDIA,market.cumCsM,husbandAge,wifeAge) {
  # Parameters: 
  # states -- states survivor matrix
  # diaAllocation -- percent of initial portfolio to spend on DIA
  # purchaseAgeDia -- age at which DIA will be purchased
  # diaPayout -- annual payout for this diaPayout (see quotes at abaris.com)
  # startAgeDia -- annuity owners age when payouts will begin
  # ownerisSpouseA -- TRUE or FALSE
  # survivorPercentDIA -- % of payout paid to a surviving spouse
  # inflationProt -- payouts are inclation-protected, TRUE or FALSE
  # market.cumCsM -- inflation-rate factor table
  # husbandAge -- husband's age at start of simulations (column 1 of states table)
  # wifeAge -- wife's age at start of simulations (column 1 of states table)
  
  if (diaOwnerisHusband) startAgeSimulation <- husbandAge else startAgeSimulation <- wifeAge

  diaIncomeM <- states 
  cat("\nBuilding Deferred Annuity Income table")
  # cat("\nDIA Payout ",diaPayout)
  # cat("\nSurvivor Payment ",survivorPercentDIA * diaPayout)
  
  diaIncomeM[states == 4 | states == 0] <- 0  # no one alive, no annuity income
  if (diaOwnerisHusband){
    diaIncomeM[states == 2] <- survivorPercentDIA    # only non-owner spouse alive, decrease payout by survBenefit %
    diaIncomeM[states == 3 | states == 1] <- 1       # both alive or only SPIA owner is alive, 100% annuity income
  } else {                                           # wife owns Spia
    diaIncomeM[states == 1] <- survivorPercentDIA  # only non-owner spouse alive, decrease payout by survBenefit %
    diaIncomeM[states == 3 | states == 2] <- 1      # both alive or only SPIA owner is alive, 100% annuity income
  }
  
  diaIncomeM <- diaIncomeM * diaAllocation * saveInitPort * diaPayout
  
  if (startAgeDIA - startAgeSimulation >= 1 ) diaIncomeM[,1:(startAgeDIA - startAgeSimulation)] <- 0  # zero out all columns (ages) prior to start date for DIA payouts
   
  if (inflationProtDIA == FALSE)
    diaIncomeM <- diaIncomeM / market.cumCsM # adjust future payments for inflation
  
  # what if dates already passed?
  return(diaIncomeM)
  
} # end of function buildDiaM

  
  