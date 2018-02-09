returnsRISMAT <- function (states,inflation,inflationSd,rf,rp,mrSd,plot = TRUE,printSummary=TRUE) {
  
  # Create a matrix of Monte Carlo-simulated stock market returns with one row per scenario and one column
  # per year of retirement. Output matrices should be same size as retiree life expectancy state diagram
  # (nrows by ncols). Also, create a cost-of-living (inflation matrix of the sames size.)
  # This code has been converted from MATLAB to R. The MATLAB code was produced by William F. Sharpe and complete documentation can be found
  # at https://web.stanford.edu/~wfsharpe/RISMAT/
  
  # INPUT PARAMETERS:
  
  # states = matrix of life expectancy states created by "Build Mortality Tables" R Script
  # inflation = expected average annual inflation rate (e.g., 0.02 for 2%)
  # inflationSd <- 0.01   # standard deviation of annual inflation (e.g., 0.01 for 1%)
  # rf = risk-free real return rate (e.g., 0.01)
  # rp = expected risk premium, the market portfolio expected return over risk-free rate (e.g., 0.0425 for 4.25%)
  # mrSd = market return annual standard deviation (e.g., 0.125 for 12.5%)
  # plot = TRUE (default, plot histograms of market returns and inflation rates)
  
  # OUTPUT GENERATED:
  
  # returnsRISMAT$market.csM = matrix of inflation growth rates, i.e., (1 + annual inflation percentage).
  # returnsRISMAT$market.cumCsM = matrix of cumulative inflation growth rates, i.e., (1 + cumulative inflation percentage).
  # returnsRISMAT$market.rmsM = matrix of market returns as growth rates, i.e., (1 + annual market return percentage).
  # returnsRISMAT$market.cumRmsM = matrix of cumulative market returns as growth rates, i.e., (1 + cumulative market return percentage).
  # returnsRISMAT$market.pvsM = matrix of present value factors discounted at the risk-free rate
  # NOTE: All matrices will be of size nrows by ncols.
  
  # create a market data structure with default values
  # cost of living 
  
  # get market assumptions
  
  cat("\nBuilding market returns table.")
  
  nrows <- length(states[,1]) #all matrices same size as life expectancy states matrix
  ncols <- length(states[1,])
  
  market.eC <- 1 + inflation   # expected cost of living ratio
  market.sdC <- inflationSd   # standard deviation of cost of living ratios
  # risk-free real investments   
  market.rf <- 1 + rf   # risk-free real return rate
  # market portfolio returns
  market.exRm <- 1 + rp # market portfolio expected return over risk-free rate
  market.sdRm  <- mrSd  # market portfolio standard deviation of return
  
  # compute cost of living (inflation) matrix
  u <- market.eC 
  v <- market.sdC^2
  b <- sqrt(log((v/(u^2)) + 1)) 
  a <- 0.5 *log((u^2)/exp(b^2))
  market.csM <- exp(a + b*matrix(rnorm(nrows*ncols),nrows,ncols))
  # compute cumulative cost of living (inflation) matrix
  
  m <- t(apply(market.csM,1,cumprod))
  market.cumCsM <- cbind(ones(nrows,1),m[,1:ncols-1])
  
  # compute risk-free real returns matrix
  market.rfsM <- market.rf*ones(nrows,ncols)
  # compute cumulative risk-free real returns matrix at ends of each year
  
  market.cumRfsM <- cbind(ones(nrows,1),m[,1:ncols-1])
  
  # compute market returns matrix
  u <- market.exRm + (market.rf - 1) # get total market expected return
  v <- market.sdRm^2
  b <- sqrt(log((v/(u^2)) + 1))
  a <- 0.5 *log((u^2)/exp(b^2))
  #  cat ("\nTerms for market ReturnsRISMAT = ",u,v,b,a,sep=" ")
  market.rmsM <- exp(a + b*matrix(rnorm(nrows*ncols),nrows,ncols))
  
  # compute market cumulative returns matrix
  
  market.cumRmsM <- cbind(ones(nrows,1),m[,1:ncols-1])
  
  # compute ppcs and present values matrix
  
  
  b <- log(u/market.rf)/log(1 + (market.sdRm^2)/(u^2))
  a <- sqrt(u*market.rf)^(b-1)
  
  # as <- (a ^ (seq(from=0,to=ncols-1))) #element-wise exponentiation
  
  market.ppcsM <- matrix(0,nrows,ncols)
  
  ############ following doesn't wotk. Present values wrong.
  
  # mb <- market.cumRmsM ^ (-b)
  # for (ii in 1:nrows){
  #   for (jj in 1:ncols){
  #     market.ppcsM[ii,jj] <- as[jj] * mb[ii,jj]
  #   }
  # }
  
  market.pvsM <- market.ppcsM/nrows 
  
  # temporary
  market.avec <- as
  market.b <- b
  
  # print a histogram of simulated market returns and inflation rates
  
  # if (plotGen) hist((market.rmsM-1)*100,col="lightblue",xlab="Simulated Annual Market Returns (%)",xlim=c(-40,60),main=paste("Histogram of \nSimulated Annual Real Market Returns"))
  #          if (plotGen) {
  
  mr.df <- data.frame(as.vector((market.rmsM-1)*100))
  colnames(mr.df) <- "Returns"
  t1 <- "Arithmetic Mean of\nSimulated\nMarket Returns= "
  t2 <- sprintf("%.2f%%", 100*(mean(market.rmsM)-1))
  t3 <- "\nSD= "
  t4 <- sprintf("%.2f%%", 100*(sd(market.rmsM - 1)))
  
  marketReturnsPlot(mr.df, paste(t1,t2,t3,t4), plotGen=TRUE)
  
  if (plotGen) {
    if.df <- data.frame(as.vector((market.csM-1)*100))
    colnames(if.df) <- "Inflation"
    t1 <- "Arithmetic Mean of\nSimulated\nInflation Rates= "
    t2 <- sprintf("%.3f%%", 100*(mean(market.csM)-1))
    t3 <- "\nSD= "
    t4 <- sprintf("%.3f%%", 100*(sd(market.csM - 1)))
    
    print(
      ggplot(data=if.df, aes(if.df$Inflation)) + 
        geom_histogram(binwidth=.1, 
                       col="blue", 
                       fill="blue", 
                       alpha = .2) + 
        xlim(-2,6) +
        labs(x="Simulated Inflation Rates", y="Count")  +
        ggtitle(paste("Histogram of \nSimulated Annual Inflation Rates")) +
        annotate("text", x = -.75, y = n*2, label =paste(t1,t2,t3,t4))
    )
  }
  
  # if (printSummary == TRUE) {
  #   
  #   cat(paste("\nArithmetic Mean of Simulated Annual Market Returns= ",sprintf("%.3f%%", 100*(mean(market.rmsM)-1)),sep=" "))
  #   cat(paste("\nStandard Deviation of Simulated Annual Market Returns= ",sprintf("%.3f%%", 100*(sd(market.rmsM)),sep=" ")))
  #   
  #   cat(paste("\nArithmetic Mean of Simulated Inflation Rates= ",sprintf("%.3f%%", 100*(mean(market.csM)-1)),sep=" "))
  #   cat(paste("\nStandard Deviation of Simulated Annual Inflation Rates= ",sprintf("%.3f%%", 100*(sd(market.csM)),sep=" ")))
  #   
  # }
  
  return(list(market.csM,market.cumCsM,market.rmsM,market.cumRmsM,market.pvsM,market.ppcsM))
  
} # end function returnsRISMAT

