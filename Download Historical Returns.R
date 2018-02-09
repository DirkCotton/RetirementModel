
histReturns <- read.csv("~/Downloads/histretSP.csv",stringsAsFactors = FALSE)

colnames(histReturns) <- c("Year","SP500 Return","T-Bill Return","T-Bond Return","Stocks Growth of Dollar","T-Bills Growth of Dollar","T-Bonds Growth of Dollar")

# download Inflation rates from http://www.usinflationcalculator.com/inflation/historical-inflation-rates/

histInflation <- read.csv("~/Downloads/Historical Inflation Rates US.csv",stringsAsFactors = FALSE)
str(histReturns)
