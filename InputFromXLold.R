# Input RISMAT date from Excel CSV file

# IMPORTANT. Annuities that have already begun paying out should be entered as pensions so
# their purchase prices isn't removed from savings



csvInput <- read.csv(userDataFile,header=TRUE,stringsAsFactors = FALSE)
csvInput[is.na(csvInput)] <- 0

wifeAge <- as.numeric(csvInput[1])
husbandAge <- as.numeric(csvInput[2])
wifeClaimAge <- as.numeric(csvInput[3])
husbandClaimAge <- as.numeric(csvInput[4])
husbandHE <- as.logical(csvInput[5])
desiredSafetyNet <- as.numeric(csvInput[6])
# hiEarnerOwnBenefit <- as.numeric(csvInput[7])

ssBenefits <- matrix(0,3,2)
ssBenefits[1,1] <- as.numeric(csvInput[11])   # higher earner's earliest claiming age benefit per month
ssBenefits[2,1] <- as.numeric(csvInput[12])   # higher earner's FRA claiming age benefit per month
ssBenefits[3,1] <- as.numeric(csvInput[13])   # higher earner's age 70 claiming age benefit per month
ssBenefits[1,2] <- as.numeric(csvInput[8])   # lower earner's earliest claiming age benefit per month
ssBenefits[2,2] <- as.numeric(csvInput[9])   # lower earner's FRA claiming age benefit per month
ssBenefits[3,2] <- as.numeric(csvInput[10])   # lower earner's age 70 claiming age benefit per month


initportfolio <- as.numeric(csvInput[14])
# inflation <- as.numeric(csvInput[15])
# inflationSd <- as.numeric(csvInput[16])
# rf <- as.numeric(csvInput[17])
# rp <- as.numeric(csvInput[18])
# mrSd <- as.numeric(csvInput[19])
purchaseAgeSPIA <- as.numeric(csvInput[20])
# annuityStartAge <- as.numeric(csvInput[21])
# annPmt <- as.numeric(csvInput[22])
inflationProtSPIA <- as.logical(csvInput[23])
spiaOwnerisHusband <- as.logical(csvInput[24])
spiaPayout <- as.numeric(csvInput[25])
survivorPercentSPIA <- as.numeric(csvInput[26])
purchaseAgeDIA <- as.numeric(csvInput[27])
diaPayout <- as.numeric(csvInput[28])
startAgeDIA <- as.numeric(csvInput[29])
diaOwnerisHusband <- as.logical(csvInput[30])
survivorPercentDIA <- as.numeric(csvInput[31])
inflationProtDIA <- as.logical(csvInput[32])
initialSpend <- as.numeric(csvInput[33])
# survivorExpense <- as.numeric(csvInput[34])
# annualAdjust <- as.numeric(csvInput[35])
homeMarketVal <- as.numeric(csvInput[36])
# housingAppreciation <- as.numeric(csvInput[37])
# liborMean <- as.numeric(csvInput[38])
# liborSigma <- as.numeric(csvInput[39])
initLoc <- as.numeric(csvInput[40])
initMort <- as.numeric(csvInput[41])
maxRate <- as.numeric(csvInput[42])
marginHECM <- as.numeric(csvInput[43])
percentMIP <- as.numeric(csvInput[44])
husbandPensionPayout <- as.numeric(csvInput[45])
husbandPensionStartAge <- as.numeric(csvInput[46])
survivorPercentHpension <- as.numeric(csvInput[47])
inflationProthPension <- as.logical(csvInput[48])
wifePensionPayout <- as.numeric(csvInput[49])
wifePensionStartAge <- as.numeric(csvInput[50])
survivorPercentWpension <- as.numeric(csvInput[51])
inflationProtwPension <- as.logical(csvInput[52])