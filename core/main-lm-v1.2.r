# Agent Based Simulation

#### Set up environment###

# Adjust this so the working directory is inside the senior_seminar_2019 folder
setwd("~/senior_seminar_2019")
#setwd("~/senior_seminar_2019/test")
#setwd(".")

# Import functions library
source("core/Agents.r")
source("core/Functions.r")
source("core/Market.r")
source("core/functions-initialize.R")
source("core/functions-estimation.R")
source("core/functions-helper.R")

#Dependency Check and Attach Packages
#DependencyCheck()




# Loads all of the input parameters globally
GetMacros(inputfile = "inputs/input-lm-v1.2.txt")

# Commented out 2/20/18.  Will set seed in montecarlo.r
#set.seed(randSeed) #added 7/17/17


#########################  ######################### #########################
#
# GENERAL STRATEGY of sim:
#
# Build the UpdateMatrix with [XX = Price+Div] # changed Ex to XX 7/17/17
#
# t > linit (note that need linit >= leval + 3)
# Calculate the Optimal Paramters; send them to --> OptimalAgent
# selectively(randomly) adopt Optimal Params by Market Agents
# Note: "Market" at any given round stores each MarketAgent's parameters
#
# Calculate mean of market parameters and assign them to RepAgent
# Forecast P_t with market params -- i.e. calculate forecast by RepAgent
# Add P_t forecast to UpdateMatrix so we can iterate -- save this forecast for
# error calcs
#
# iterate forward;
# Calculate Market Price based on RepAgent's forecast (done in just 1 step
# during bulk of simulation)
#
# Update Price, Div, Int, EX vectors
# Update Storage Arrays --
# Send OptimalAgent Params to "log" of OptimalParams (UpdateParams)
# Send RepAgent Params to "log" of MARKET MEAN PARAMS (AlphaMatrix)
# Send P_t (really P_hat_t) to AlphaMatrix[t, 10] --> use that for later error
# calcs
#
# the price, etc. vectors (related 'zoo' objects are just numerically indexed
# versions of the same)
# will be the things to plot
# also the parameters from the two storage matrices (AlphaMatrix is what we
# really care about)
#
######################### ######################### #########################


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


###########################################################
### First Stage: Initialize Prices, Market, and Storage ###
###########################################################
initializer = function(sim_rounds, num) {
  # maximum number of terms in the model is (powers * lags) + (((lags-1) * (lags)) / 2)
  # a few other data items are saved, so 3 is added to the size to make room for these values
  size <<- (3 + (powers * lags) + (((lags-1) * (lags)) / 2))
  initialize_prices(sim_rounds)
  initialize_market(num)
  initialize_storage(sim_rounds)
}


######################################################################################
### Second Stage: Loop until linit without updating forecast rule parameter values ###
######################################################################################
create_initial_data = function(t) {
  #if (t %% 100 == 0) {
   # print(paste("round:" , t), quote = FALSE)
  #}
  
  # "Zoo" Matrix with new data XX=P+d  (there is an NA at XX_t)
  # Changed X to XX 7/17/17
  new_matrix = UpdateMatrix(PriceDivData = xx,
                            M = memory,
                            t = t)


  # Forecast XX_t = P_t + d_t based on RepAgent forecast rule Params
  # changed P_t to XX_t 7/17/17
  EX_t = Predict_t(matrix_data = new_matrix, Params = RepAgent[2:size], t)
  print(EX_t)

  # Plug estimate EX_t in for unknown XX_t in data matrix
  # Changed P_t to EX_t 7/17/17
  new_matrix[which(index(new_matrix) == t), 2] = EX_t
  
  # Use it to estimate XX_(t+1) -- aka iterate forecast and then use
  # this and the arbitrage condition to generate market price P_t1
  P_t1 = Market_Price(matrix_data = new_matrix,
                      market_params = RepAgent[2:size],
                      r = interest_rates[t-1],
                      t = t)
  print(P_t1)
  
  #APPEND P_t1 to price data
  UpdateFundamentals(newPrice = P_t1, t)
  
  print(prices)
  print(xx)
  if (t == 5) {
    stop()
  }

  #Storage -- c onstant until updating occurs
  AlphaMatrix[t, 2:size] <<- RepAgent[2:size]
  UpdateParams[t, 2:size] <<- RepAgent[2:size]
}


##############################################
### Third Stage: Rest of Simulation (Bulk) ###
##############################################
simulation_loop = function(t) {

  if (t < memory) {
    stop("t is less than memory. This should not be possible")
  } else {

    # "Zoo" Matrix with new data  (there is an NA at P_t)
    new_matrix = UpdateMatrix(PriceDivData = xx,
                              M = memory,
                              t = t)
    
    # Estimate Optimal Paramters from "memory" using Y & X
    estimated_regression <<- EstParams(new_matrix, t)
    
    # OptimalAgent[2:size] <<- EstParams(new_matrix, t)
    OptimalAgent[2:size] <<- estimated_regression
    
    # writeLines("\nOptimalAgent:")
    # print(OptimalAgent)
    
    # Now get a random vector of size: popsize:pupdate
    update_traders = RandomVec()
    # Selectively update
    for (j in seq(1:popsize)) {
      if (Market[j, 1] %in% update_traders) {
        Market[j, 2:size] <<- OptimalAgent[2:size]
      }
    }
    
    # Take the mean of agents' ("market") forecast rule params and assign
    # them to RepAgent
    RepAgent[2:size] <<- Market_Params(market_round = Market)
    
    # Forecast P_t based on mean params (which are already stored in
    # RepAgent)
    # Changed P_t to EX_t 7/17/17
    EX_t = Predict_t(matrix_data = new_matrix, Params = RepAgent[2:size], t)
    
    # Plug estimate for P_t in
    # Changed P_t to EX_t 7/17/17
    new_matrix[which(index(new_matrix) == t), 2] = EX_t
    
    # Update Storage Containers
    AlphaMatrix[t, 2:size] <<- RepAgent[2:size]
    
    # Use means of Market Forecast Rule Params to find market price based on
    # discounted mean forecast
    RepAgent[size] <<- Market_Price(matrix_data = new_matrix,
                                  market_params = RepAgent[2:size],
                                  r = interest_rates[t - 1],
                                  t = t)
    
    # Update Storage Containers
    #AlphaMatrix[t, 2:size] <<- RepAgent[2:size]
    
    # Log of 'updates'
    UpdateParams[t, 2:size] <<- OptimalAgent[2:size]
    UpdateFundamentals(newPrice = RepAgent[size], t)
  }
}


#################################
### Main function ###
#################################
main = function(MarketObject) {
  # Then override the global input that we have from GetMacros with the parameter value.  
  memory <<- memory
  pupdate <<- pupdate
  crash_t <<- 0
  
  for (round in (lags + 1):rounds) {
    if (round == (lags + 1)) {
      MarketObject$init()
      initializer(rounds, popsize)
    } else if ( (round > (lags + 1))&(round <= linit ) ) {
      # Not first round and before linit round
      # changed memory + 3 to linit 7/17/17
      MarketObject$createInitialData(round)
      create_initial_data(round)
    } else if ((prices[round-1] < bubbleThresholdLow) | (prices[round-1] > bubbleThresholdHigh)) { 
      # check if price has blown up
      # changed ex to prices 7/17/17
      return (round, 1)
      crash_t <<- round
      numBubbles = numBubbles + 1
      print("We're Experiencing a Bubble or a Crash")
      break
    } else {
      simulation_loop(round)
      MarketObject$simulation(round)
    }
    MarketObject$print(verbose=TRUE, round)
    #print_market(round)
  }
  
  Make_Zoos(round)
  #print("End of Sim")
  return (numBubbles)
}
# 0 no bubble
# 1 bubble
# -1 fail

mainTwo = function(MarketObject) {
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
      return (c(1, round, MarketObject$memory, MarketObject$pUpDate))
      # crash_t <<- round
      # numBubbles = numBubbles + 1
      # print("We're Experiencing a Bubble or a Crash")
    } else {
      MarketObject$simulation(round)
    }
    MarketObject$print(verbose=TRUE, round)
    #print_market(round)
  }
  
  #Make_Zoos(round)
  #print("End of Sim")
  return (c(0, 0, MarketObject$memory, MarketObject$pUpDate))
}
# save objects then average predictions

#Spline gives function F(x).  Each agent iteratively forecast E(x50) and E(X51) and take average of x51

#THE BUTTON: pull the trigger -- execute main()

# s <- (3 + (powers * lags) + (((lags-1) * (lags)) / 2))
# 
# func <- new("Functions")
# 
# MO <<- new("Market", 
#           optimalAgents = list(), 
#           agents = list(), 
#           repAgent = new("RepresentativeAgent"),
#           prices = c(1),
#           dividends = c(1),
#           interestRates = c(1),
#           xx = c(1),
#           memory = memory,
#           pUpDate = pupdate,
#           bubbles = 0,
#           bubbleRound = 0,
#           size = s,
#           runType = runType,
#           numAgents = popsize,
#           numRounds = rounds,
#           lInit = linit,
#           randSeed = randSeed,
#           lags = lags,
#           powers = powers,
#           startPrice = startPrice,
#           bubbleThresholdHigh = bubbleThresholdHigh,
#           bubbleThresholdLow = bubbleThresholdLow,
#           interest = interest,
#           dividend = dividend,
#           shockRangeDiv = shockRange_div,
#           riskConstant = risk_constant,
#           riskType = risk_type,
#           pShock = pshock,
#           selectionType = selection_type,
#           oldRep = vector(),
#           oldOA = vector(),
#           helperFunctions = func,
#           marketMatrix = matrix(),
#           alphaMatrix = matrix(), 
#           updateParams = matrix())

#mainTwo(MarketObject = MO)

