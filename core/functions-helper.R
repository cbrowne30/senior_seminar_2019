# Agent-Based Trading
# DK SP '17 from Georges '15, ... ,
##


################################################################
# This file defines the functions to be called in 'main.r'
# it need NOT be run, 'main.r' reads and executes
# in the line;
# source("functions.R")
################################################################

# Functions are organized into 3 sections:

#
# # DESSERT #
# 1) Market_Price
# 2) Market_Params



######################## DESSERT ###########################


# Compute the Mean Squared Forecast Error for the RepAgent to be used in risk aversion
# To run with RepAgent (homogeneous), set agent = RepAgent[2:size]
# To run with average prediction of all agents (heterogeneous), set agent = Market[i, 2:size]
MSFE = function(matrix_data, prices, t, agent) {
  sum_sq_error = 0
  
  # Index to start calculating MSFE at
  start_index = linit
  
  # Don't want to look further back than memory
  # if the period is (memory) past linit or more, then only go back memory amount
  if (t - memory >= start_index) {
    start_index = t - memory + 1
  }
  
  for (period in start_index:t-1) {
    period_prediction = Predict_t(matrix_data = matrix_data, Params = agent, t = period)
    error_sq = (prices[period] - period_prediction)^2
    sum_sq_error = sum_sq_error + error_sq
  }
  MSFE = (sum_sq_error / (t - 1 - start_index))
  return(MSFE)
}

hetero_MSFE = function(matrix_data, prices, t) {
  # Computes a risk aversion value by calculating the mean squared 
  # forecast error for [memory] forecasts back.  Instead of using the RepAgent
  # to get the MSFE, we calculate it for all of the agents in the market.
  # NOTE: This will be much slower than homogeneous risk function because it has to calculate
  # MSFE for all agents, not just one.
  total_msfe = 0
  
  for (i in 1:nrow(Market)) {
    total_msfe = total_msfe + MSFE(matrix_data, prices, t, Market[i, 2:size])
  }
  
  avg_msfe = total_msfe / nrow(Market)
}


# Compute Market Price (under Perfect/Avg Arbitrage Condition)
# =update_matrix, =mean_params, =r
Market_Price = function(matrix_data, market_params, r, t)
{
  # take mean of Price + Dividend
  
  # Similar to forecast(i): Dot-Product of MKT-Params[2-8]=beta1 and
  # UpdateMatrix[1]
  mkt_inputs = matrix(tail(matrix_data, 1), ncol = size-1)
 
  # dot prod
  X_hat_t1 = mkt_inputs %*% (market_params) # changed P to X 7/17/17
  

  # R.E.E. mean
  
  # Initialize market price without risk aversion
  market_price = X_hat_t1 / (1 + r)
  
  # If you are past linit and including risk, reset market price
  # Should start at linit + 1
  if (risk_type > 0) {   # Risk type = 0 means no endogenous risk
    if (t - 1 > linit) {
      if (risk_type == 1) { # Risk type = 1 means homogeneous risk
        market_price = (X_hat_t1 + (risk_constant * MSFE(matrix_data, prices, t, RepAgent[2:size]))) / (1 + r)
      } else {    # Risk type = 2 means heterogeneous risk
        market_price = (X_hat_t1 + (risk_constant * hetero_MSFE(matrix_data, prices, t))) / (1 + r)
      }
    }
  }
 
  return(market_price)
}




# Compute Market Means (for 7 Params)
Market_Params = function(market_round) #=Market
{
  # take mean over columns 2:8 of market
  mkt = data.frame(market_round)
  mean_params = c()
  for (i in 2:size){
    mean_params <- c(mean_params, mean(mkt[, i], na.rm = TRUE))
  }
  return(mean_params)
  
}

Make_Zoos = function(t) { 
  Prices <<- zoo((prices), order.by = seq(1, t, 1))
	Dividends <<- zoo((dividends), order.by = seq(1, t, 1))
  Interest <<- zoo((interest_rates), order.by = seq(1, t, 1))	  
	XX <<- Prices + Dividends #changed EX to XX 7/17/17
}

print_market = function(t) {
  print(paste("ROUND", t))
  print("---------------------------------------")
  print("#######################################")
  
  print("Optimal Agent")
  print(OptimalAgent)
  #print(Market)
  print(popsize)
  printPopSize = popsize
  if(printPopSize > 100) {
    printPopSize = 100
  }
  print("MARKET:")
  print(Market[1:printPopSize, 1:size])
}
