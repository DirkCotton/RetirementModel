#  Setup simulation parameters

printSummary <- TRUE  # default is TRUE, turns on summary print to console
plotGen <- FALSE      # generate plots if TRUE
knitrSave <- TRUE     # if TRUE, simulation results are saved to pass to knitr for report

n <- 10000           # number of scenarios to run

# upper and lower bounds for simulation parameters

spiaLB <- 0     # minimum percent allocation of savings to SPIA to simulate
spiaUB <- 0    # maximum percent allocation of savings to SPIA to simulate
diaLB <- 0      # minimum percent allocation of savings to DIA to simulate
diaUB <- 0     # maximum percent allocation of savings to DIA to simulate
equityUB <- 1   # maximum percent allocation of equity to simulate
equityLB <- 0   # minimum percent allocation of equity to simulate
spendUB <- 1  # maximum percent of initial spending simulated
spendLB <- 1   # minimum percent of initial spending simulated

inflation <- .02 # mean inflation rate
inflationSd <- .01 # sigma inflation rate

rf <- .01    # risk-free bond rate
rp <- .0425  # Equity risk premium

mrSd <- .12    # sigma of market returns

annualAdjust <- 0

survivorExpense <- .66666666

housingAppreciation <- 0  # real estate annual appreciation rate

liborMean <- .02   # risk-free bond rate (LIBOR)
liborSigma <- .01    # LIBOR sigma


