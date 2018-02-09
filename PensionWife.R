# Build Pension Income Matrix for Wife
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


buildPensionWife <- function(wifePensionPayout,states,wifePensionStartAge,survivorPercentWpension,inflationProthPension,market.cumCsM,wifeAge) {
  # Parameters: 
  # wifePensionPayout -- annual pension payout to husband
  # states -- states survivor matrix
  # wifePensionStartAge -- husband age when payments begin
  # survivorPercentWpension -- % of payout paid to a surviving spouse
  # inflationProthPension -- payouts are inclation-protected, TRUE or FALSE
  # market.cumCsM -- inflation-rate factor table
  # wifeAge -- husband's age at start of simulations (column 1 of states table)
  
  wPensionIncomeM <- states 
  cat("\nBuilding Wife Pension Income table")
  
  wPensionIncomeM[states == 4 | states == 0] <- 0      # no one alive, no pension income
  wPensionIncomeM[states == 3 | states == 2] <- 1      # wife pension owner alive, full pension income
  wPensionIncomeM[states == 1] <- survivorPercentWpension  # husband only alive, reduced pension income
 
  wPensionIncomeM <- wifePensionPayout * wPensionIncomeM
  
  if (wifePensionStartAge - wifeAge >= 1 ) wPensionIncomeM[,1:(wifePensionStartAge - wifeAge)] <- 0  # zero out all columns (ages) prior to start date for SPIA payouts
   
  if (inflationProthPension == FALSE)
    wPensionIncomeM <- wPensionIncomeM / market.cumCsM # adjust future payments for inflation
  
  # what if dates already passed?
  return(wPensionIncomeM)
  
} # end of function buildPensionWife

