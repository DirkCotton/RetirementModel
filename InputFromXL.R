# Input RISMAT date from Excel CSV file

# IMPORTANT. Annuities that have already begun paying out should be entered as pensions so
# their purchase prices isn't removed from savings

ssBenefits <- matrix (0,3,2)
csvInput <- read.csv(userDataFile,header=TRUE,stringsAsFactors = FALSE)
csvInput[is.na(csvInput)] <- 0

household <- as.numeric(csvInput[1])
wifeDOB <- as.numeric(csvInput[2])
husbandDOB <- as.numeric(csvInput[3])
wifeClaimAge <- as.numeric(csvInput[4])
husbandClaimAge <- as.numeric(csvInput[5])
desiredSafetyNet <- as.numeric(csvInput[6])
initportfolio <- as.numeric(csvInput[7])
initialSpend <- as.numeric(csvInput[8])
purchaseAgeSPIA <- as.numeric(csvInput[9])
annuityStartAge <- as.numeric(csvInput[10])
inflationProtSPIA <- as.logical(csvInput[11])
spiaOwnerisHusband <- as.logical(csvInput[12])
spiaPayout <- as.numeric(csvInput[13])
survivorPercentSPIA <- as.numeric(csvInput[14])
purchaseAgeDIA <- as.numeric(csvInput[15])
diaPayout <- as.numeric(csvInput[16])
startAgeDIA <- as.numeric(csvInput[17])
diaOwnerisHusband <- as.logical(csvInput[18])
survivorPercentDIA <- as.numeric(csvInput[19])
inflationProtDIA <- as.logical(csvInput[20])
homeMarketVal <- as.numeric(csvInput[21])
initLoc <- as.numeric(csvInput[22])
initMort <- as.numeric(csvInput[23])
maxRate <- as.numeric(csvInput[24])
marginHECM <- as.numeric(csvInput[25])
percentMIP <- as.numeric(csvInput[26])
husbandPensionPayout <- as.numeric(csvInput[27])
husbandPensionStartAge <- as.numeric(csvInput[28])
survivorPercentHpension <- as.numeric(csvInput[29])
inflationProthPension <- as.logical(csvInput[30])
wifePensionPayout <- as.numeric(csvInput[31])
wifePensionStartAge <- as.numeric(csvInput[32])
survivorPercentWpension <- as.numeric(csvInput[33])
inflationProtwPension <- as.logical(csvInput[34])
ssBenefits[1,1] <- as.numeric(csvInput[35])
ssBenefits[2,1] <- as.numeric(csvInput[36])
ssBenefits[3,1] <- as.numeric(csvInput[37])
ssBenefits[1,2] <- as.numeric(csvInput[38])
ssBenefits[2,2] <- as.numeric(csvInput[39])
ssBenefits[3,2] <- as.numeric(csvInput[40])

wifeAge <- 66    # wife's age at start of simulation
husbandAge <- 66 # husband's age at start of simulation
