setwd("~/senior_seminar_2019/core/")
source("main-lm-v1.2.r")

startTime = Sys.time()
mem_list = seq(10, 100, 10)
pupdate_list = seq(0.1, 0.9, 0.)
num_sims = 10
Market_List = list()
func = new("Functions")
for (mem in mem_list){
  for (pup in pupdate_list){
    s <- (3 + (powers * lags) + (((lags-1) * (lags)) / 2))
    MO = new("Market", 
               optimalAgents = list(), 
               agents = list(), 
               repAgent = new("RepresentativeAgent"),
               prices = c(1),
               dividends = c(1),
               interestRates = c(1),
               xx = c(1),
               memory = mem,
               pUpDate = pup,
               bubbles = 0,
               bubbleRound = 0,
               size = s,
               runType = runType,
               numAgents = popsize,
               numRounds = rounds,
               lInit = linit,
               randSeed = randSeed,
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
               helperFunctions = func,
               marketMatrix = matrix(),
               alphaMatrix = matrix(), 
               updateParams = matrix())
    
    Market_List = append(Market_List, MO)
  }
}

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
mpi.bcast.Robj2slave(DependecyCheckHPCversion)
mpi.remote.exec(DependecyCheckHPCversion())

row = 1
col = 1
agg_matrix = matrix(nrow = length(mem_list), ncol = length(pupdate_list), dimnames = list(mem_list, pupdate_list))

for(market in Market_List){
  runList = list()
  for (i in 1:num_sims){
    runList = append(runList, market)
  }
  returns <- mpi.applyLB(runList, mainTwo)
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
  bub_percent = num_bubbles/num_sims
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
