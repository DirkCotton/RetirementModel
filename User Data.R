# Set up User Data for Simulation

cat("\n**********************")
cat("\nUser Data")
cat("\n**********************")

userDataFile <- "~/R Projects/RetirementModel/RISMAT Input Butts.csv"

source('~/R Projects/RetirementModel/InputFromXL.R')  # get data from Excel CSV file

household <- 2

cat("\nReading user data from file= ",userDataFile,"\n")

#################
# overrides
#################
# wifeClaimAge <- 62
# cat("\n\nWife SS Claiming Age override ",wifeClaimAge)

cat("\nWife's Year of Birth: ", wifeDOB, "Husband' Age's Year of Birth ",husbandDOB)
cat("\nWife's SS Claiming Age: ", wifeClaimAge, "Husband's SS claiming Age ",husbandClaimAge)

colnames(ssBenefits) <- c("Higher-Earner Benefits","Lower-Earner Benefits")
rownames(ssBenefits) <- c("Age 62","FRA","Age 70")

cat("\n  ")
print (ssBenefits)

cat("\n\nInitial Portfolio Balance ",initportfolio)
saveInitPort <- initportfolio
cat("\n\nFirst year spending is ",initialSpend)

cat("\nDesired Floor Spending",desiredSafetyNet)

cat("\n\nSPIA purchase age: ",purchaseAgeSPIA)
cat("\nSPIA is inflation-protected is ",inflationProtSPIA)
cat("\nSPIA owner is husband is ",spiaOwnerisHusband)
cat("\nSPIA Payout is ",spiaPayout)
cat("\nSPIA survivor percentage is ",survivorPercentSPIA)

cat("\n\nDIA purchase age: ",purchaseAgeDIA)
cat("\nDIA is inflation-protected is ",inflationProtDIA)
cat("\nDIA owner is husband is ",diaOwnerisHusband)
cat("\nDIA Payout is ",diaPayout)
cat("\nDIA survivor percentage is ",survivorPercentDIA)

cat("\n\nSurvivor expense ratio is ",survivorExpense)

cat("\n\nHome Market Value is ",homeMarketVal)

cat("\n\nHECM initial line of credit ",initLoc)
cat("\nHECM initial mortgage balance ",initLoc)
cat("\nHECM max interest rate ",maxRate)
cat("\nHECM margin rate ",marginHECM)
cat("\nHECM MIP ",percentMIP)

cat("\n\nHusband Pension Payout ",husbandPensionPayout)
cat("\nHusband Pension Start Age ",husbandPensionStartAge)
cat("\nHusband Pension Survivor percent ",survivorPercentHpension)
cat("\nHusband Pension is inflation-protected is ",inflationProthPension)

cat("\n\nWife Pension Payout ",wifePensionPayout)
cat("\nWife Pension Start Age ",wifePensionStartAge)
cat("\nWife Pension Survivor percent ",survivorPercentWpension)
cat("\nWife Pension is inflation-protected is ",inflationProtwPension)

cat("\n\nOveride variables here before running Retirement Model is desired.")

