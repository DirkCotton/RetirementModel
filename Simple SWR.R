# Test simple longevity-dependent SWR

# market returns are in marketVector

annualSpend <- 69000
pv <- 2615616    # initial portfolio value
state4 <- rep(0,length(states[,1]))
portbal <- state4
  

# lifespans can be derived from state == 4 (minus 1). Create vector of lifespans
for (i in 1:length(states[,1])) {
  state4[i] <- which(states[i,] == 4) - 1
}

portbal[1] <- pv
for (i in 2:nscen) {
  portbal[i] <- (portbal[i - 1] - annualSpend) * marketVector[i - 1]
}

tTPV