histReturns <- read.csv("~/Downloads/histretSP.csv",stringsAsFactors = FALSE)

colnames(histReturns) <- c("Year","SP500 Return","T-Bill Return","T-Bond Return","Stocks Growth of Dollar","T-Bills Growth of Dollar","T-Bonds Growth of Dollar")

# download Inflation rates from http://www.usinflationcalculator.com/inflation/historical-inflation-rates/

histInflation <- read.csv("~/Downloads/Historical Inflation Rates US.csv",stringsAsFactors = FALSE)

swr <- matrix(0,86,4)
swrNoSpend <- swr
annualSpend <- 30000
initPortfolio <- 1000000

for (i in 1:86) {
  swr[i,1] <- (initPortfolio - annualSpend) * (1 + histReturns$`SP500 Return`[i])
  swr[i,2] <- (swr[i,1]  - annualSpend) * (1 + histReturns$`SP500 Return`[i + 1])
  swr[i,3] <- (swr[i,2]  - annualSpend) * (1 + histReturns$`SP500 Return`[i + 2])
  swr[i,4] <- (swr[i,3]  - annualSpend) * (1 + histReturns$`SP500 Return`[i + 3])

  swrNoSpend[i,1] <- (initPortfolio) * (1 + histReturns$`SP500 Return`[i])
  swrNoSpend[i,2] <- (swrNoSpend[i,1]) * (1 + histReturns$`SP500 Return`[i + 1])
  swrNoSpend[i,3] <- (swrNoSpend[i,2]) * (1 + histReturns$`SP500 Return`[i + 2])
  swrNoSpend[i,4] <- (swrNoSpend[i,3]) * (1 + histReturns$`SP500 Return`[i + 3])
  if (i==1) cat("\nFirst swr row",swr[1,])
  }

swr <- round(swr)
swrNoSpend <- round(swrNoSpend)

costPortSpending <- swrNoSpend[,4] - swr[,4]

