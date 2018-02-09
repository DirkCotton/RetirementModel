# function to find full retirement ages for both spouses give birth years. Returns fraAges$wifeFRAage, fraAges$husbandFRAage

findFRA <- function (fraTable,wifeDOB,husbandDOB)
  if (wifeDOB < 1937) {
    wifeFRAage <- 65
  } else {
    wifeFRAage <- fraTable[wifeDOB - 1937 + 1,2] 
  }

if (husbandDOB < 1937) {
  husbandFRAage <- 65
} else {
  husbandFRAage <- fraTable[husbandDOB - 1937 + 1,2]
}

fraAges <- list("wifeFRAage" = wifeFRAage,"husbandFRAage" = husbandFRAage)
 
return(fraAges)






