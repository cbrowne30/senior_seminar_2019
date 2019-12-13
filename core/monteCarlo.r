##############################################
# Document: monteCarlo.r
# Purpose: Used for running a montecarlo simulation
# Functions:
#   1. checkPath
# ToDo:
##############################################
#!/usr/bin/env Rscript

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

# Make sure input file was input
if (length(args)==0) {
  stop("Input File must be given", call.=FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
sink(paste(args[2], "/", args[3], sep = ""), append=FALSE, split=FALSE)

dependencyCheck(onHPC = TRUE)

startTime = Sys.time()
mem_list = seq(100, 100, 10)
pupdate_list = seq(0.1, 0.1, 0.1)
num_sims = 10
Market_List = list()
#set.seed(3)

GetMacros(paste("inputs/", args[1], sep = ""))
# args = commandArgs(trailingOnly=TRUE)
# if (length(args)==0) {
#   stop("At least one argument must be supplied - the input file", call.=FALSE)
# } else if (length(args)==1) {
#   GetMacros(inputfile = args[1])
#   if (saveData != 0 | saveData != 1){
#     print("Error.  saveData was not equal to 1 or 0. Setting saveData to 0 (Not saving market)")
#   }

Run_List = list()
Market_Runs = list()
for (mem in mem_list){
  for (pup in pupdate_list){
    Run_List = list()
    s <- (3 + (powers * lags) + (((lags-1) * (lags)) / 2))
    for (i in 1:num_sims){
      MO = new("Market", 
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
                memory = mem,
                pUpDate = pup,
                bubbles = 0,
                bubbleRound = 0,
                size = s,
                runType = runType,
                numAgents = popsize,
                numRounds = rounds,
                lInit = linit,
                randSeed = sample(1:2^15, 1),
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
      Run_List = append(Run_List, MO)
    }
    Market_Runs[[length(Market_Runs)+1]] <- Run_List
  }
}
#}

# Load the R MPI package if it is not already loaded.
if (!is.loaded("mpi_initialize")) {
  library("Rmpi")
}


ns <- mpi.universe.size() - 1
mpi.spawn.Rslaves(nslaves=ns)
#
# In case R exits unexpectedly, have it automatically clean up
# resources taken up by Rmpi (slaves, memory, etc...)
.Last <- function(){
  if (is.loaded("mpi_initialize")){
    if (mpi.comm.size(1) > 0){
      print("Please use mpi.close.Rslaves() to close slaves.")
      mpi.close.Rslaves()
    }
    print("Please use mpi.quit() to quit R")
    .Call("mpi_finalize")
  }
}
# Tell all slaves to return a message identifying themselves
mpi.bcast.cmd( id <- mpi.comm.rank() )
mpi.bcast.cmd( ns <- mpi.comm.size() )
mpi.bcast.cmd( host <- mpi.get.processor.name() )
mpi.remote.exec(paste("I am",mpi.comm.rank(),"of",mpi.comm.size()))


mpi.remote.exec(source("core/Agents.r"))
mpi.remote.exec(source("core/Functions.r"))
mpi.remote.exec(source("core/Market.r"))
mpi.bcast.Robj2slave(dependencyCheck)
mpi.remote.exec(dependencyCheck(onHPC = TRUE))


row = 1
col = 1
agg_matrix = matrix(nrow = length(mem_list), ncol = length(pupdate_list), dimnames = list(mem_list, pupdate_list))

#Set starting market number for progress checker
numMarket = 1

for(marketGroup in Market_Runs){
  returns <- mpi.applyLB(marketGroup, main)
  
  #Progress prints to output file
  print(paste0("Finished ", numMarket, " of ", length(Market_Runs), " market types"))
  numMarket = numMarket + 1
  print(returns)
  
  num_bubbles = 0
  actual_sims = num_sims
  for (run in returns){
    if (run[1] == 1){
      num_bubbles = num_bubbles + 1
    }
    else if (run[1]== -1) {
      actual_sims = actual_sims -1
    }
  }
  bub_percent = num_bubbles/actual_sims
  bub_percent = round(bub_percent, digits = 3)
  agg_matrix[row, col] = bub_percent
  if (col == length(pupdate_list)){
    row = row+1
    col = 1
  } else{
    col = col+1
  }
}

print(agg_matrix)
print(Sys.time() - startTime)
# Tell all slaves to close down, and exit the program
mpi.close.Rslaves(dellog = FALSE)
mpi.quit()
