##############################################
# Document: singleRun.r
# Purpose: Used for running a single simulation
# Functions:
#   1. checkPath
# ToDo:
##############################################

checkPath = function() {
  path = getwd()
  if (substr(path,(nchar(path) + 1) - 19, nchar(path)) != "senior_seminar_2019") {

    if(grepl("senior_seminar_2019", path)) {
      subStringFound = unlist(gregexpr(pattern = "senior_seminar_2019", path))
      setwd(substr(path, 0, subStringFound + 19))
    } else {
      print("Error - Incorrect path please change it to senior_seminar_2019")
    }

  }
}

checkPath()

# Import functions library
source("core/Functions.r")
source("core/Agents.r")
source("core/Market.r")
source("core/main-lm-v1.2.r")

dependencyCheck(onHPC = FALSE)

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied - the input file", call.=FALSE)
} else if (length(args)==1) {

  GetMacros(inputfile = args[1])
  
  s <- (3 + (powers * lags) + (((lags-1) * (lags)) / 2))
  
  MO <<- new("Market",
             optimalAgents = list(),
             agents = list(),
             repAgent = new("RepresentativeAgent"),
             prices = c(1),
             dividends = c(1),
             interestRates = c(1),
             xx = c(1),
             thresholdTally = 0,
             priceDifThreshold = priceDifThreshold,
             saveData = saveData,
             memory = memory,
             pUpDate = pupdate,
             bubbles = 0,
             bubbleRound = 0,
             size = s,
             runType = runType,
             numAgents = popsize,
             numRounds = rounds,
             lInit = linit,
             randSeed = sample(1:2000, 1),
             lags = lags,
             powers = powers,
             startPrice = startPrice,
             bubbleThresholdHigh = bubbleThresholdHigh,
             bubbleThresholdLow = bubbleThresholdLow,
             interest = interest,
             dividend = dividend,
             shockRangeDiv = shockRange_div,
             riskConstant = risk_constant,
             riskType = risk_type,
             pShock = pshock,
             selectionType = selection_type,
             oldRep = vector(),
             oldOA = vector(),
             marketMatrix = matrix(),
             alphaMatrix = matrix(),
             updateParams = matrix())
  
  print(main(MarketObject = MO))
  
}