##############################################
# Document: main.r
# Purpose: Contains the main function for running
# a simulation 
# Functions:
#   1. main
#   2. mainTwo
#   3. initializer
#   4. create_initial_data
#   5. simulation_loop
# ToDo:
##############################################

################################################################################
################################################################################
# OVERVIEW:
#
# The whole simulation happens in 3 stages:
#  	(1) Initialization of:
#	         (a) Prices of Stock and Bond
#		 (b) Market
#		 (c) Storage
#	(2) Achieve Normal Market Conditions
#	         (a) Go for linit rounds until some market history has been
#                    created
#	(3) Add Learning Dynamics
#	    	 (a) Let the traders trade
#
################################################################################
################################################################################




#################################
### Main function ###
#################################

# 0 no bubble
# 1 bubble
# -1 fail

main = function(MarketObject) {
  # Then override the global input that we have from GetMacros with the parameter value
  
  for (round in (MarketObject$lags + 1):MarketObject$numRounds) {
    if (round == (MarketObject$lags + 1)) {
      MarketObject$init()
    } else if ( (round > (MarketObject$lags + 1))&(round <= MarketObject$lInit ) ) {
      # Not first round and before linit round
      # changed memory + 3 to linit 7/17/17
      MarketObject$createInitialData(round)
    } else if ((MarketObject$prices[round-1] < MarketObject$bubbleThresholdLow) | 
               (MarketObject$prices[round-1] > MarketObject$bubbleThresholdHigh)) { 
      # check if price has blown up
      # changed ex to prices 7/17/17
      return (c(1, round, MarketObject$memory, MarketObject$pUpDate, MarketObject$prices[round-1]))
    } else {
      MarketObject$simulation(round)
    }
    MarketObject$print(verbose=TRUE, round)
  }
  
  #Make_Zoos(round)
  #print("End of Sim")
  return (c(0, 0, MarketObject$memory, MarketObject$pUpDate))
}
# save objects then average predictions

