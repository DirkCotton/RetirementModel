floorSummary <- matrix(0,8,3)
colnames(floorSummary) <- c("Both Survive","Husband Survives","Wife Survives")
rownames(floorSummary)<- c("Social Security Benefits","Wife's Pension","Wife's Pension (survivor)","Husband's Pension","Husband's Pension (survivor)","SPIA Income","DIA Income","TOTAL")

# both survive
floorSummary[1,1] <- ssBenefits[3,1] + ssBenefits[3,2]
floorSummary[2,1] <- wifePensionPayout
floorSummary[3,1] <- 0
floorSummary[4,1] <- husbandPensionPayout
floorSummary[5,1] <- 0
floorSummary[6,1] <- spiaAllocation[1] * saveInitPort * spiaPayout
floorSummary[7,1] <- diaAllocation[1] * saveInitPort * diaPayout

# husband survives
floorSummary[1,2] <- ssBenefits[3,1] 
floorSummary[2,2] <- 0
floorSummary[3,2] <- wifePensionPayout * survivorPercentWpension
floorSummary[4,2] <- husbandPensionPayout
floorSummary[5,2] <- 0
if (spiaOwnerisHusband){
  floorSummary[6,2] <- spiaAllocation[1] * saveInitPort * spiaPayout 
} else {
  floorSummary[6,2] <- spiaAllocation[1] * saveInitPort * spiaPayout * survivorPercentSPIA
}

if (!spiaOwnerisHusband){
  floorSummary[7,2] <- diaAllocation[1] * saveInitPort * diaPayout 
} else {
  floorSummary[7,2] <- diaAllocation[1] * saveInitPort * diaPayout * survivorPercentDIA
}


# wife survives
floorSummary[1,3] <- ssBenefits[3,1] 
floorSummary[2,3] <- wifePensionPayout
floorSummary[3,3] <- 0
floorSummary[4,3] <- 0
floorSummary[5,3] <- husbandPensionPayout * survivorPercentHpension
if (!spiaOwnerisHusband){
  floorSummary[6,3] <- spiaAllocation[1] * saveInitPort * spiaPayout 
} else {
  floorSummary[6,3] <- spiaAllocation[1] * saveInitPort * spiaPayout * survivorPercentSPIA
}

if (spiaOwnerisHusband){
  floorSummary[7,3] <- diaAllocation[1] * saveInitPort * diaPayout 
} else {
  floorSummary[7,3] <- diaAllocation[1] * saveInitPort * diaPayout * survivorPercentDIA
}

floorSummary[8,1] <- sum(floorSummary[,1])
floorSummary[8,2] <- sum(floorSummary[,2])
floorSummary[8,3] <- sum(floorSummary[,3])

print(knitr::kable(floorSummary,format="markdown"))