#!/bin/Rscript
library(zoo)
library(xts)
library(glmnet)
library(rlist)
library(dplyr)
library(leaps)

setwd("~/Senior_Seminar")

#####
# Set these values to change how the test works

# the average price
average <<- 10
# the variation from the average for the randomly generated data
div <<- 0.5

# Maximum number of lags and powers for the basic regression function
lags <<- 3
powers <<- 3

# Random seed for reproducable results
randSeed <<- 100
# The number of rounds that the agent remembers
memory <<- 20
# The number of rounds before the agent starts
start_time <<- 30
# The total number of rounds
end_time <<- 200
# The names of the models, be careful when changing this
model_names <<- c("lm", "lasso.0", "lasso.0.01", "lasso.1", "cv", "Hybrid")

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
  if(lags > 1) {
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

# allows for reproducable results
set.seed(randSeed)

# calucate the size based on the powers and lags
size <<- (3 + (powers * lags) + (((lags-1) * (lags)) / 2))

XX = runif(end_time, average - div, average + div)

# create a matrix to hold all coefficients of all models
coefficient_matrix <<- array(0,
                             dim = c(end_time - start_time, size, length(model_names)),
                             dimnames = list(1:(end_time - start_time),
                                             1:size,
                                             model_names))

# run all models across the random data
for(t in start_time:end_time) {
  new_matrix = UpdateMatrix(XX, memory, t)
  
  Y = new_matrix[2:which(index(new_matrix) == t - 1), 2]
  
  # Drop newest 2 rows of len(52) matrix so each t corresponds to X_(t+1)
  # for Param Estimation
  X = matrix(new_matrix, ncol = size - 1)
  
  Y = matrix(Y, ncol= 1)
  colnames(Y)[1] <- "Y"
  
  #MATRIX = matrix(X, ncol= 7)
  MATRIX = X[1:(nrow(X) - 2),]
  
  # CREATING DATASET TO RUN OLS
  x_y = cbind(Y, MATRIX)
  
  ##########
  # glmnet lambda = 0
  ##########
  
  fit = glmnet(x = MATRIX[,2:ncol(MATRIX)],
               y = Y,
               alpha = 0.9,
               lambda = seq(10,0,-0.1))
  coeffs = coef(fit, s = 0)
  
  coefficient_matrix[(t - start_time),2:size,2] = matrix(coeffs)
  
  ##########
  # glmnet 2
  ##########
  
  fit = glmnet(x = MATRIX[,2:ncol(MATRIX)],
               y = Y,
               alpha = 0.9,
               lambda = seq(1,0,-0.01))
  # s = chosen lambda value
  coeffs = coef(fit, s = 0.01)
  
  coefficient_matrix[(t - start_time),2:size,3] = matrix(coeffs)
  
  ##########
  # glmnet 3
  ##########
  
  fit = glmnet(x = MATRIX[,2:ncol(MATRIX)],
               y = Y,
               alpha = 0.9,
               lambda = seq(10,0,-0.1))
  coeffs = coef(fit, s = 1)
  
  coefficient_matrix[(t - start_time),2:size,4] = matrix(coeffs)

  ##########
  # cv.glmnet
  ##########
  
  coefficients = coef(cv.glmnet(x = MATRIX[,2:ncol(MATRIX)],
                                y = Y,
                                family="gaussian",
                                nfolds = 5,
                                alpha = 1), s = "lambda.min")
  
  coefficient_matrix[(t - start_time),2:size,5] = matrix(coefficients)
  
  ##########
  # standard lm
  ##########
  
  x_y = as.data.frame(x_y)
  
  # OLD REGRESSION
  regression = lm(formula = Y ~ ., data = x_y)
  
  #RETRIEVING REGRESSION COEFFICIENTS
  beta1 <- summary(regression)$coefficients[,1]
  beta1 = as.matrix(beta1,nrow=7,ncol=1)

  coefficient_matrix[(t - start_time),2:size,1] = beta1
  
  ##########
  # Hybrid Agent
  ##########
  
  # perform the hybrid model selection regression
  regfit_hybrid = regsubsets(x = MATRIX[,2:ncol(MATRIX)],
                             y = Y,
                             nvmax = size,
                             method = "exhaustive")
  # retrive the coefficients from the regression
  hybrid_coefs_bic = coef(regfit_hybrid,
                          which.min(summary(regfit_hybrid, best=TRUE)$bic))
  
  # assign the coefficients to their correct positions in the model
  HybridAgent = rep(0, size - 1)
  HybridAgent[1] = hybrid_coefs_bic[1]
  for(n in 2:length(hybrid_coefs_bic)) {
    # get the positional number of the coefficient
    names(hybrid_coefs_bic)[n] = utf8ToInt(names(hybrid_coefs_bic)[n]) - 96
    # assign it to the correct position in the overall model
    HybridAgent[(as.numeric(names(hybrid_coefs_bic)[n])+1)] =
      hybrid_coefs_bic[names(hybrid_coefs_bic)[n]]
  }
  coefficient_matrix[(t - start_time), 2:size, 6] = HybridAgent
}

# plot all tests
source("test_plots.r")
