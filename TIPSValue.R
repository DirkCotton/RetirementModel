tipsCost <- read.csv("~/R Projects/RetirementModel/TIPS Ladder Cost.csv",header=FALSE)
ladderLength <- length(tipsCost$V1)
ladderTV <- rep(0,ladderLength)   #create a terminal value vector same length as laadder cost

ladderTV[ladderLength] <- 0  # ladder will be depleted in last year
# assume market value will remain the same (no interest rate changes) so market value year n <- market value year n-1
for (termYear in 1:(ladderLength-1)) {
  ladderTV[termYear] <- sum(tipsCost$V1[1: (ladderLength - termYear)])
}
print(ladderTV)