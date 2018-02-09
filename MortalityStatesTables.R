mortalityStates <- function (n,husbandAge,wifeAge,plotGen=TRUE) {

# Build mortality tables and mortality states table. Uses mortality tables from Excel Spreadsheet
# that uses Sharpe MATLAB algorithm.
 
#  ------------ get vectors of mortality rates for person 1
#  make vectors for rows and columns of mortality tables
# ages = [50:1:120]' #  ages (rows in mortality tables)
# years = [2014:1:2113] #  years (columns in mortality tables)
#  compute mortalities for person 1
# if client.p1Sex == 'M'
#  get mortalities for 2014

# STATES
# state 0 - neither spouse alive
# state 1 - only spouse A is alive
# state 2 - only spouse B is alive
# state 3 - both spouses alive
# state 4 - last spouse dies in the previous year

cat(paste("Building mortality and state tables.",sep=" "))

w <- numeric(n)
wf <- numeric(n)


# build repeat columns function
rep.col<-function(x,n){ matrix(rep(x,each=n), ncol=n, byrow=TRUE) }

m1 <- read.csv("~/R Projects/RISMAT/GetRP2014Male.csv",header=FALSE) #  get mortality improvement rates 2015-2027
m2 <- read.csv("~/R Projects/RISMAT/GetMP2014Male.csv",header=FALSE) #  get mortality improvement rates 2015-2027
# else
m1f <- read.csv("~/R Projects/RISMAT/GetRP2014Female.csv",header=FALSE) #  get mortalities for 2014
m2f <- read.csv("~/R Projects/RISMAT/GetMP2014Female.csv",header=FALSE) #  get mortality improvement rates 2015-2027
# end
#  extend mortality improvement rates to 2113 using ultimate rates for 2027

m2027 <- m2[,13]
#  make mortality table from 2014 through 2113
m3 <- rep.col(m2027,57)
m4 <- cbind (1-m1, 1-m2 ,1-m3) #  join 2014 mortality and mortality factors (1 - improvement)
#m4 <- cumprod(m4) #  multiply 2014 mortality by cumulative factors
m4 <- t(apply(m4,1,cumprod))

m2027f <- m2f[,13]
m3f <- rep.col(m2027f,57)
#  make mortality table from 2014 through 2113
m4f <- cbind (1-m1f, 1-m2f ,1-m3f) #  join 2014 mortality and mortality factors (1 - improvement)
m4f <- t(apply(m4f,1,cumprod))

currentYear <- as.integer(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))
younger <- min(wifeAge, husbandAge) # extend the shorter length matrix to tl longer life to make equal

mortTableFemale.df <- m4f
mortTableMale.df <- m4             

if (wifeAge != 0) wifeMort2.df <- mortTableFemale.df[(wifeAge-49):71,(currentYear - 2013):71]
if (husbandAge != 0) husbandMort2.df <- mortTableMale.df[(husbandAge-49):71,(currentYear - 2013):71]

mortP12annual <-  read.csv("~/R Projects/RISMAT/SharpeBothDiags.csv")
lmortP1 <- length(mortP12annual[,1])  # number of elements read into vector
mortP1 <- numeric(lmortP1) # initialize vectors
mortP2 <- mortP1

if (husbandAge != 0) mortP1[1] <- 1 - mortP12annual[husbandAge-49,1]
if (wifeAge != 0) mortP2[1] <- 1 - mortP12annual[wifeAge-49,2]
if (husbandAge == 0)  youngest <- wifeAge
if (wifeAge == 0)  youngest <- husbandAge
if (wifeAge > 0 & husbandAge > 0)  youngest <- min(wifeAge,husbandAge)

for (ii in 2:(lmortP1 - youngest + 50)) {
  if (husbandAge != 0) mortP1[ii] <- mortP1[ii-1] * (1 - mortP12annual[husbandAge-49 + ii - 1,1])
  if (wifeAge != 0) mortP2[ii] <- mortP2[ii-1] * (1 - mortP12annual[wifeAge-49 + ii - 1,1])
}

mortP1 <- mortP1[1:(lmortP1 - youngest + 50)]
mortP2 <- mortP2[1:(lmortP1 - youngest + 50)]
mortP1[is.na(mortP1)] <- 0 # replace any NA's with zero
mortP1[is.na(mortP1)] <- 0

lMort <- length(mortP1)

probs <- matrix(mortP1,nrow=n,ncol=lMort,byrow=TRUE) # probability matrix husband's mortality
randnums <- matrix(runif(lMort*n),ncol=lMort,nrow=n) # same size table of random uniform numbers
randnums[,2:lMort] <- randnums[,1]
statesP1 <- (randnums >= probs) # matrix T/F client is alive

for (d in 1:n){
  w[d] <- min(which(randnums[d,1] >= probs[d,]))
  statesP1[d,] <- c(rep(1,w[d]),rep(0,lMort-w[d]))
}


# repeat for wife
probsW <- matrix(mortP2,nrow=n,ncol=lMort,byrow=TRUE) # probability matrix wife's mortality
randnumsW <- matrix(runif(lMort*n),ncol=lMort,nrow=n) # same size table of random uniform numbers
randnumsW[,2:lMort] <- randnumsW[,1]
statesP2 <- (randnumsW >= probsW) # matrix T/F client is alive
statesP2a <- statesP2

for (d in 1:n){
  wf[d] <- min(which(randnumsW[d,1] >= probsW[d,]))
  statesP2[d,] <- c(rep(1,wf[d]),rep(0,lMort-wf[d]))
}

statesP2 <- statesP2 * 2 # 1 means husband alive, 2 means wife alive

if (husbandAge == 0) statesP1 <- 0  # single female retiree
if (wifeAge == 0) statesP2 <- 0    # single male retiree

states <- statesP1 + statesP2

estates <- (states[,2:lMort] == 0) & (states[,1:lMort - 1] > 0)
estates <- c(rep(0,n),estates)
states <- states + 4 * estates



# plot mortality table if requested by caller

# if (plotGen) {
# 
# probSurvive1only <- colMeans(states == 1)
# probSurvive2only <- colMeans(states == 2)
# probSurviveBoth <- colMeans(states == 3)
# probSurviveAll <- rbind(probSurvive1only,probSurvive2only,probSurviveBoth)
# print(plotMortTable(probSurviveAll,plotGen))
# }

return (states)
}



