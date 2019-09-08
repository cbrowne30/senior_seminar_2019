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

# # APERITIF #
# 1) GetMacros
# 2) initialize_prices
# 3) initialize_market
# 4) initialize_storage
# 5) UpdateFundamentals
#
# # MEAT #
# 1) UpdateMatrix
# 2) Predict_t
# 3) Predict_t1
# 4) EstParams
# 5) RandomVec
#
# # DESSERT #
# 1) Market_Price
# 2) Market_Params
#




# clear workspace
rm(list = ls())

# load packages
# setwd("F:/dkraynak/georges/agent based trading")
# setwd("F:/cgeorges/risk R")
# setwd("~/Senior_Seminar")
setwd(".")

# install.packages("zoo")
# install.packages("xts")
library(zoo)
library(xts)
library(glmnet)
library(rlist)
library(dplyr)
library(leaps)



######################## APERITIF #########################



# Get Inputs
GetMacros = function(inputfile)
{
  # Get macro inputfile
  input = read.table(inputfile, header = FALSE, sep = " ")
  input = data.frame(input)
  
  # get rid of scientific notation
  options(scipen = 999)
  # names
  colnames(input) <- c("Varnames", "V2")
  
  # macro vars
  rounds <<- subset(input, Varnames == "rounds")[[2]]
  popsize <<- subset(input, Varnames == "popsize")[[2]]
  bubbleThresholdHigh <<-
    subset(input, Varnames == "bubbleThresholdHigh")[[2]]
  bubbleThresholdLow <<-
    subset(input, Varnames == "bubbleThresholdLow")[[2]]
  pupdate <<- subset(input, Varnames == "pupdate")[[2]]
  
  powers <<- subset(input, Varnames == "powers")[[2]]
  lags <<- subset(input, Varnames == "lags")[[2]]
  numBubbles <<- subset(input, Varnames == "numBubbles")[[2]]
  
  # initial conditions
  startPrice <<- subset(input, Varnames == "startPrice")[[2]]
  dividend <<- subset(input, Varnames == "dividend")[[2]]
  interest <<- subset(input, Varnames == "interest")[[2]]
  
  # note: rename leval=memory
  memory <<- subset(input, Varnames == "memory")[[2]]
  linit <<- subset(input, Varnames == "linit")[[2]]
  # maxiter???? <<- subset(input,Varnames == "linit")[[2]]
  shockRange_div <<- subset(input, Varnames == "shockRangeDiv")[[2]]
  runType <<- subset(input, Varnames == "runType")[[2]]
  selection_type <<- subset(input, Varnames == "selection_type")[[2]]
  
  pshock <<- subset(input, Varnames == "pshock")[[2]]
  randSeed <<- subset(input, Varnames == "randSeed")[[2]]
  risk_constant <<- subset(input, Varnames ==  "risk_constant")[[2]]
  risk_type <<- subset(input, Varnames == "risk_type")[[2]]
}



# INIT
initialize_prices = function(sim_rounds) {
  
  #PRETTY SURE GETTING RID OF THIS
  #Periods <<- seq(1:sim_rounds)
  
  # Initialize vector of prices of length 3
  prices <<- rep(((dividend / interest) + dividend), (lags + 1))
  
  # Initialize a similar vector to prices, but make the first three spots = dividend
  dividends <<- rep(dividend, (lags + 1))
  
  # Initialize another vector and add the interest rate to the first three slots
  interest_rates <<- rep(interest, (lags + 1))
  
  #IMPORTANT:
  # xx is a vector of length sim_rounds that has the sum of the two vectors 
  # Each element in xx is the sum of elements i in both vectors 
  xx <<- prices+dividends # changed ex_t to xx 7/17/17
  #Note: Currently I just initialize with 3 copies of startPrice & start loop at round 3 (lags)
}



# Setup RepAgent and Market Array
initialize_market = function(num_agents)
  #=popsize
{
  # RepAgent is just a vector -- he will store the mean parameters and calc.
  # market price
  RepAgent <<- rep(0, (size+2))
  RepAgent[2] <<- (dividend / interest) + dividend
  
  # initialize matrix (or array) of agents
  Market <<-
    matrix(
      data = 0,
      nrow = num_agents,
      ncol = size,
      byrow = TRUE
    )
  
  # assign IDs
  Market[1:num_agents]  <<-  seq(1, num_agents, 1)
  
  # Full weight to a_0 in prelim rounds
  Market[1:num_agents, 2]  <<-
    rep((dividend / interest) + dividend, num_agents)
  
  # OptimalAgent is a vector -- he will update to the Optimal Alphas from
  # memory each round, & send to "UpdateParams"
  OptimalAgent <<- rep(0,(size+2))
}



# set up Alpha & Forecast array
initialize_storage = function(sim_rounds)
  #=rounds
{
  # Initialize Data Storage
  AlphaMatrix  <<-  matrix(
    data = 0,
    nrow = sim_rounds,
    ncol = size,
    byrow = TRUE
  )
  # Assign round-IDs
  AlphaMatrix[1:sim_rounds]  <<-  seq(1, sim_rounds, 1)
  
  # Initialize Data Storage
  UpdateParams  <<-  matrix(
    data = 0,
    nrow = sim_rounds,
    ncol = size,
    byrow = TRUE
  )
  # Assign round-IDs
  UpdateParams[1:sim_rounds]  <<-  seq(1, sim_rounds, 1)
}


#Update Price, Dividends, Interest (include commented out shock)
UpdateFundamentals = function(newPrice, t) #=market_price, =div or shock, =r or shock
  { 
    #insert price
    prices <<- append(prices, newPrice) 

    #insert div w/varying prob of div shock
    randnum = runif(1, min = 0, max = 1)
    if (randnum < pshock) {
      dividends[t] <<- dividend + runif(n = 1,
                                      min = -shockRange_div,
                                      max = shockRange_div)
       }
    else {
      dividends[t] <<- dividend
      }

    #insert int
    interest_rates <<- append(interest_rates, interest)

    #update xx
    xx <<- prices+dividends 
}



######################## MEAT ###########################


# Update Matrix: Takes last M prices and builds Matrix to update AR model
# Changed Price.. to PriceDiv.. 7/17/17
UpdateMatrix = function(PriceDivData, M, t)
{
  # Grab Most Recent 50 Price (and Div)
  # Chanced Price.. to PriceDiv.. 7/17/17
  if (t <= M + (lags + 1)) {
    PriceDivVec = PriceDivData[1:t]
    # early periods -- get all available price data
    mem = seq(1, t, 1)
  }
  else {
    # So early periods get lagged values ie. length(54)
    # Changed PriceVec to PriceDivVec 7/17/17
    PriceDivVec = PriceDivData[(t - M - (lags + 1)):t]
    
    # later periods -- bounded memory
    mem = seq((t - M - (lags + 1)), t, 1) #length(54)
  }
  
  # column 1 (AKA x_0):

  intercept = rep(1, length(mem))
  intercept = zoo(intercept, order.by = mem)
  
  # Turn data into zoo (ts) objects
  # Changed PriceVec to PriceDivVec 7/17/17
  TS <- zoo(x = PriceDivVec, order.by = mem)
  
  # Create list object to store zoo objects, to merge later
  lag_list <- list(intercept=intercept, TS=TS)
  # Store the current lag
  prev_lag <- TS
  # Store all previous lags, for cross products later
  prev_lag_list <- list() #
  
  # Main loop, add each lag to lag_list, and powers for each lag
  for (i in 1:lags){
    prev_lag <- lag(prev_lag, k=1, na.pad = TRUE)
    prev_lag_list <- list.append(prev_lag_list, i = prev_lag)
    lag_list <- list.append(lag_list, i = prev_lag)
    
    if (powers > 1) {
      for (j in 2:powers){
        lag_list <- list.append(lag_list, j = prev_lag ** j)
      }
    }
  }
  

  # Use prev_lag_list to get cross products, add to lag_list
  if (lags > 1){
    for (lag1 in 1:(length(prev_lag_list)-1)){
      for (lag2 in (lag1+1):length(prev_lag_list)){
        lag_list <- list.append(lag_list, inter = (prev_lag_list[lag1]$i) * (prev_lag_list[lag2]$i))
      }
    }
  }
  
  # Merge all zoo objects in lag_list, and return
  MatrixUpdate = do.call("merge", lag_list)
  MatrixUpdate = window(MatrixUpdate, seq((t - M - 1), t, 1))
  return(MatrixUpdate)
}

# Make prediction for P_t (first step of iterated forecasting,
# USING AVERAGE PARAMS)
Predict_t = function(matrix_data, Params, t)
{
  
  # Get most recent COMPLETE row of data
  pred_inputs = matrix_data[which(index(matrix_data) == t - 1), ]
  
  # writeLines("\npred_inputs:")
  # print(pred_inputs)

  # (1x7) x (7x1)
  x_hat_t = pred_inputs %*% (Params) # changed p to x 7/17/17
  
  return(x_hat_t) # changed p to x 7/17/17
}

################################################################################
### THIS FUNCTION IS UNUSED (could be used for iterated forecasting but there
### is no need, see "Market_Price()" below
################################################################################

# Make prediction for P_(t+1) (second step of iterated forecasting)
Predict_t1 = function(matrix_data, Params, t)
{
  # estimate F[P_(t+1)]
  
  # same above with index bumped forward
  pred_inputs = matrix_data[which(index(matrix_data) == t), ]
  
  # (1x7) x (7x1)
  x_hat_t1 = pred_inputs %*% (Params) # changed p to x 7/17/17
  
  return(x_hat_t1) # changed p to x 7/17/17
}




# Estimate Parameters (REP AGENT)
EstParams = function(new_matrix, t) #=MatrixUpdate // new_matrix
{ # arg1: TS of last M price(EX) obs
  # arg2: matrix of last M obs, 1-lag
  # goal is to fit AR model on M obs
  # Matrix Mult should spit out the Optimal BetaVec
  
  # writeLines("\nsize:")
  # print(size)
  # 
  # writeLines("\nt:")
  # print(t)
  # 
  # writeLines("\nnew_matrix:")
  # print(new_matrix)

  # change
  Y = new_matrix[2:which(index(new_matrix) == t - 1), 2]

  X = matrix(new_matrix, ncol = size - 1)
  
  #convert data to matrices
  Y = matrix(Y, ncol= 1)
  colnames(Y)[1] <- "Y"

  MATRIX = X[1:(nrow(X) - 2),]

  ##########
  # Note: runType is set in input-lm-v1.2.txt
  #
  # 0: LM
  # 1: glmnet with lambda = 0
  # 2: glmnet with lambda = 1
  # 3: glmnet with cross validation
  # 4: Hybrid selection agent
  ##########
  
  if (runType == 0) {
    # LM
    
    # Creating dataset to run ols
    x_y = as.data.frame(cbind(Y, MATRIX))
    regression = lm(formula = Y ~ ., data = x_y)
    
    # Retrieving regression coefficients
    test_val = (as.matrix(summary(regression)$coefficients[,1],
                      nrow = size,
                      ncol = 1))
    
    return (test_val)
  }
    
  else if (runType == 1) {
    # Standard glmnet lambda = 0
    # checking for equivalence with standard lm regression
    return (coef(glmnet(x = MATRIX[,2:ncol(MATRIX)],
                        y = Y,
                        alpha = 0.9,
                        # list should contain at least 3 lambda values
                        lambda = seq(10,0,-0.1)),
                 # s controls which lambda value is chosen from the list
                 s = 0))
  }
  
  else if (runType == 2) {
    # Standard glmnet lambda = 1
    return (coef(glmnet(x = MATRIX[,2:ncol(MATRIX)],
                        y = Y,
                        alpha = 0.9,
                        # list should contain at least 3 lambda values
                        lambda = seq(100, 0, -1)),
                 # s controls which lambda value is chosen from the list
                 s = .1))
  }

  else if (runType == 3) {
    # CV glmnet
    # probably not correct yet
    return (coef(cv.glmnet(x = MATRIX[,2:ncol(MATRIX)],
                           y = Y,
                           alpha = 1,
                           nfolds = 10,   # might need to be more dynamic
                           family = "gaussian"),
                 # s = "lambda.min" chooses the lambda with the best cv value
                 s = "lambda.min"))
  }
  
  else if (runType == 4) {
    HybridAgent = rep(0,size)
    # Perform the hybrid model selection regression
    regfit_hybrid = regsubsets(x = MATRIX[,2:ncol(MATRIX)], 
                              y = Y, nvmax = size, 
                              method = (if(selection_type == 0)
                                  "forward"
                               else if(selection_type == 1)
                                  "backward"
                               else if (selection_type == 2)
                                 "exhaustive"))
    # Rertrive the coefficients from the regression
    # Selects the model with minimized BIC (Bayesian Information Criterion)
    hybrid_coefs_bic = coef(regfit_hybrid, 
                            which.min(summary(regfit_hybrid, best = TRUE)$bic))
    
    # The following code is necessary due to the way the coefficients are returned by coef()
    # An exmaple of the return format of the coefficients:
    #
    # BIC Coefficients
    # (Intercept)            4            8           11           13 
    # 127.51242265   0.05735865 -17.88244908  -1.69758103   1.73587485
    #
    # So some modification is needed to ensure the coefficients are
    # assigned to the correct positions in the matrix
    
    # assign the coefficients to their correct positions in the model
    HybridAgent[1] = hybrid_coefs_bic[1]
    for (n in 2:length(hybrid_coefs_bic)) {
      # get the positional number of the coefficient
      names(hybrid_coefs_bic)[n] = utf8ToInt(names(hybrid_coefs_bic)[n]) - 96
      # assign it to the correct position in the overall model
      HybridAgent[(as.numeric(names(hybrid_coefs_bic)[n])+1)] = hybrid_coefs_bic[names(hybrid_coefs_bic)[n]]
    }
    
    return (HybridAgent)
  }
}

# Choose agents to update
RandomVec = function()
{
  # randomize
  random_vec = c(sample(0:popsize, popsize * pupdate))
  
  # here is the idea in the loop:
  # if Market[ID] %in% random_vec; Market[ID, 2:8] <- RepAgent[2:8]
  return(random_vec)
}



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


######################## --------------------- ###########################

##### END OF FUNCTION DEFS ######

######################## --------------------- ###########################
