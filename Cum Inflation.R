survM <- matrix(0,nscen,nyrs)
for (i in 1:nyrs) {survM[i,survived[i]] <- 1}
cumInflation <- rowSums(market.cumCsM * survM)[1:max(survived)] # vector of cum inflation for each scenario at death of both spouses
# rorCumInflation <- 1/cumInflation^(1/survived[1:max(survived)]) - 1
