# Agent Based Simulation

#### Set up environment###

# This directory will work for everyone, as long as you are running the repo
# from inside the directory
setwd("~/Senior_Seminar")
#setwd(".")

# Import functions library
source("core/functions-lm-v1.2.R")

# Loads all of the input parameters globally
GetMacros(inputfile = "input-lm-v1.2.txt") # add lm

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


# First Stage: Initialize Prices, Market, and Storage
initializer = function(sim_rounds, num) {
  # maximum number of terms in the model is (powers * lags) + (((lags-1) * (lags)) / 2)
  # a few other data items are saved, so 3 is added to the size to make room for these values
  size <<- (3 + (powers * lags) + (((lags-1) * (lags)) / 2))
  initialize_prices(sim_rounds)
  initialize_market(num)
  initialize_storage(sim_rounds)
}


#Second Stage: Loop until linit without updating forecast rule parameter values
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
  
  # Plug estimate EX_t in for unknown XX_t in data matrix
  # Changed P_t to EX_t 7/17/17
  new_matrix[which(index(new_matrix) == t), 2] = EX_t
  
  # Use it to estimate XX_(t+1) -- aka iterate forecast and then use
  # this and the arbitrage condition to generate market price P_t1
  P_t1 = Market_Price(matrix_data = new_matrix,
                      market_params = RepAgent[2:size],
                      r = interest_rates[t-1],
                      t = t)
  
  #APPEND P_t1 to price data
  UpdateFundamentals(newPrice = P_t1, t)
  
  #Storage -- constant until updating occurs
  AlphaMatrix[t, 2:size] <<- RepAgent[2:size]
  UpdateParams[t, 2:size] <<- RepAgent[2:size]
}


#Third Stage: Rest of Simulation (Bulk)
simulation_loop = function(t) {
  if (t < memory) {
    print("This should definitely not be printing. You messed up.")
  }
  else {
    if (t %% 100 == 0) {
      #print(paste("round:" , t), quote = FALSE)
    }
    
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
### Controller for Simulation ###
#################################
print_market = function(t) {
  print(paste("ROUND", t))
  print("---------------------------------------")
  print("#######################################")
  
  print("Optimal Agent")
  print(OptimalAgent)
  
  print("MARKET:")
  print(Market[1:100, 1:size])
  
  
}

# If you want to include other variables in the repeated simulation, add a parameter to this function
main = function(Memory, Pupdate) {
     # Then override the global input that we have from GetMacros with the parameter value.  
     memory <<- Memory
     pupdate <<- Pupdate
     crash_t <<- 0
     
     #print(linit)
     for (t in (lags + 1):rounds) {
     	 if (t == (lags + 1)) {
	      initializer(rounds, popsize)
     	  }
		
	     else if ( (t > (lags + 1))&(t <= linit ) ) {
 	          # Not first round and before linit round
            # changed memory + 3 to linit 7/17/17
	          create_initial_data(t)
	      } 

	     else if ((prices[t-1] < bubbleThresholdLow) | (prices[t-1] > bubbleThresholdHigh)) { 
	          # check if price has blown up
  	        # changed ex to prices 7/17/17
            crash_t <<- t
	          numBubbles = numBubbles + 1
            print("We're Experiencing a Bubble or a Crash")
      	    break
 	      } 

	     else {
	        simulation_loop(t)
	     }
       #print_market(t)
       
     } #close for loop
    Make_Zoos(t)
    #print("End of Sim")
    return (numBubbles)
    } 



#THE BUTTON: pull the trigger -- execute main()
#main(Memory = 100, Pupdate = 0.5)

