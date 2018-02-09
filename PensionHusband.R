# Build Pension Income Matrix for Husband
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


buildPensionHusband <- function(husbandPensionPayout,states,husbandPensionStartAge,survivorPercentHpension,inflationProthPension,market.cumCsM,husbandAge) {
  # Parameters: 
  # husbandPensionPayout -- annual pension payout to husband
  # states -- states survivor matrix
  # husbandPensionStartAge -- husband age when payments begin
  # survivorPercentHpension -- % of payout paid to a surviving spouse
  # inflationProthPension -- payouts are inclation-protected, TRUE or FALSE
  # market.cumCsM -- inflation-rate factor table
  # husbandAge -- husband's age at start of simulations (column 1 of states table)
  
  hPensionIncomeM <- states 
  cat("\nBuilding Husband Pension Income table")
  
  hPensionIncomeM[states == 4 | states == 0] <- 0      # no one alive, no pension income
  hPensionIncomeM[states == 3 | states == 1] <- 1      # husband pension owner alive, full pension income
  hPensionIncomeM[states == 2] <- survivorPercentHpension  # wife only alive, reduce pension income
 
  hPensionIncomeM <- husbandPensionPayout * hPensionIncomeM
  
  if (husbandPensionStartAge - husbandAge >= 1 ) hPensionIncomeM[,1:(husbandPensionStartAge - husbandAge)] <- 0  # zero out all columns (ages) prior to start date for SPIA payouts
   
  if (inflationProthPension == FALSE)
    hPensionIncomeM <- hPensionIncomeM / market.cumCsM # adjust future payments for inflation
  
  # what if dates already passed?
  return(hPensionIncomeM)
  
} # end of function buildPensionHusband

