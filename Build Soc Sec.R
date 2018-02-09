# function to build Social Security benefits per claiming age given husband and wife monthly fra benefit

buildSocSec <- function (wifeFRAamount,husbandFRAamount,wifeFRAage,husbandFRAage) {
  ssBen <- matrix(0,9,3)
  colnames(ssBen) <- c("Age","Wife Benefit","Husband Benefit")
  ssBen[,1] <- seq(62,70)
  
  fra <- wifeFRAage - 62
  
  ssBen[fra + 1,2] <- wifeFRAamount
 
  for (i in fra:(fra + 1 - 3)) {  # for three years prior to fra reduce benefit 6.67%
  ssBen[i,2] <- ( 1 - .0667) * ssBen[i + 1,2]
  }

  if (i > 1) {
    for (j in (i-1):1) {  # for  second three years prior to fra reduce benefit 5%
      ssBen[j,2] <- ( 1 - .05) * ssBen[j + 1,2]
    }
  }

  fra2 <- husbandFRAage - 62
  ssBen[fra2 + 1,3] <- husbandFRAamount
  for (i in fra2:(fra2 + 1 - 3)) {  # for three years prior to fra reduce benefit 6.67%
    ssBen[i,3] <- ( 1 - .0667) * ssBen[i + 1,3]
  }
  
  if (i > 1) {
    for (j in (i-1):1) {  # for  second three years prior to fra reduce benefit 5%
      ssBen[j,3] <- ( 1 - .05) * ssBen[j + 1,3]
    }
  }
  
  # build delay credits for both spouse for ages FRA + 1 to 70.
  for (i in (fra + 2):9) {
    ssBen[i,2] <- ssBen[i - 1,2] * 1.08
  }

  
  for (i in (fra2 + 2):9) {
    ssBen[i,3] <- ssBen[i - 1,3] * 1.08
  }
  
return(round(ssBen))
  
}


