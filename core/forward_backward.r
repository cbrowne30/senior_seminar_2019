#!/bin/Rscript
library(zoo)
library(xts)
library(glmnet)
library(rlist)
library(dplyr)
library(leaps)

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


# possibly generate random noise here
#XX = c(10.5, 8.3, 5, 16.7, 16.5, 4.6, 12.4, 8.3, 10.9, 6.8)
#XX = c(10.5, 10.5, 10.5, 10.5, 10.5, 11, 10.5, 10.5, 10.5, 10.5)
#XX = runif(10, 9.5, 11.5)

#####
# Set these values
div <<- 0.5
lags <<- 3
powers <<- 3
average <<- 10
randSeed <<- 20
memory <<- 20
start_time <<- 30
end_time <<- 100
t = end_time

set.seed(randSeed)

size <<- (3 + (powers * lags) + (((lags-1) * (lags)) / 2))

XX = runif(end_time, average - div, average + div)

writeLines("\nXX:")
print(XX)

# agents
GlmAgent = rep(00, size)
CVAgent = rep(00, size)
RegAgent = rep(00, size)
FwdAgent = rep(00, size)
BkwdAgent = rep(00, size)
HybridAgentRSQ = rep(00,size)
HybridAgentBIC = rep(00,size)

new_matrix = UpdateMatrix(XX, memory, t)

writeLines("\nnew_matrix:")
print(new_matrix)

# Estimate Params and send to UpdateParams
# Drop oldest & newest (X_t)

writeLines("\nt:")
print(t)

Y = new_matrix[2:which(index(new_matrix) == t - 1), 2]

writeLines("\nY:")
print(Y)

# Drop newest 2 rows of len(52) matrix so each t corresponds to X_(t+1)
# for Param Estimation
#X = tail(new_matrix,-2)
X = matrix(new_matrix, ncol = size - 1)

writeLines("\nX:")
print(X)

Y = matrix(Y, ncol= 1)
colnames(Y)[1] <- "Y"

writeLines("\nEstParams Y:")
print(Y)

#MATRIX = matrix(X, ncol= 7)
MATRIX = X[1:(nrow(X) - 2),]

#writeLines("\nDimension of ESTPARAMS New_Matrix")
#print(dim(MATRIX))

writeLines("\nMATRIX:")
print(MATRIX)

# CREATING DATASET TO RUN OLS
x_y = cbind(Y, MATRIX)

writeLines("\nx_y:")
print(x_y)


#regfit_fwd = step(lm(formula = Y ~ ., data = df_x_y), scope = df_x_y, direction = "forward")
#fwd_sum = (as.matrix(summary(regfit_fwd)$coefficients[,1], nrow = size, ncol = 1))
regfit_fwd = regsubsets(x = MATRIX[,2:ncol(MATRIX)], y = Y, nvmax = size,  method = "forward")
fwd_sum = summary(regfit_fwd, best = TRUE)
fwd_rsqs = fwd_sum$adjr2


best_model = which.max(fwd_rsqs)


fwd_coefs = coef(regfit_fwd, best_model)

FwdAgent[2] = fwd_coefs[1]
for (n in 2:length(fwd_coefs)) {
  names(fwd_coefs)[n] = utf8ToInt(names(fwd_coefs)[n]) - 96
  FwdAgent[(as.numeric(names(fwd_coefs)[n])+1)] = fwd_coefs[names(fwd_coefs)[n]]
}

writeLines("Summary")
print(fwd_sum)
writeLines("\nR Squares:")
print(fwd_rsqs)
#print(regfit_fwd)
writeLines("\nCoefficients")
print(fwd_coefs)
writeLines("\nForward Agent")
print(FwdAgent)


regfit_bkwd = regsubsets(x = MATRIX[,2:ncol(MATRIX)], y = Y, nvmax = size,  method = "backward")
bkwd_sum = summary(regfit_bkwd, best = TRUE)
bkwd_rsqs = bkwd_sum$adjr2


best_model = which.max(bkwd_rsqs)


bkwd_coefs = coef(regfit_bkwd, best_model)

BkwdAgent[2] = bkwd_coefs[1]
for (n in 2:length(bkwd_coefs)) {
  names(bkwd_coefs)[n] = utf8ToInt(names(bkwd_coefs)[n]) - 96
  BkwdAgent[(as.numeric(names(bkwd_coefs)[n])+1)] = bkwd_coefs[names(bkwd_coefs)[n]]
}

writeLines("\nSummary")
print(bkwd_sum)
writeLines("\nR Squares:")
print(bkwd_rsqs)
#print(regfit_fwd)
writeLines("\nCoefficients")
print(bkwd_coefs)
writeLines("\nBackward Agent")
print(BkwdAgent)


regfit_hybrid = regsubsets(x = MATRIX[,2:ncol(MATRIX)], y = Y, nvmax = size,  method = "exhaustive")
hybrid_sum = summary(regfit_hybrid, best = TRUE)
hybrid_rsqs = hybrid_sum$adjr2
hybrid_bic = hybrid_sum$bic

best_model_rsq = which.max(hybrid_rsqs)
best_model_bic = which.min(hybrid_bic)

hybrid_coefs_rsq = coef(regfit_hybrid, best_model_rsq)
hybrid_coefs_bic = coef(regfit_hybrid, best_model_bic)


HybridAgentRSQ[2] = hybrid_coefs_rsq[1]
for (n in 2:length(hybrid_coefs_rsq)) {
  names(hybrid_coefs_rsq)[n] = utf8ToInt(names(hybrid_coefs_rsq)[n]) - 96
  HybridAgentRSQ[(as.numeric(names(hybrid_coefs_rsq)[n])+1)] = hybrid_coefs_rsq[names(hybrid_coefs_rsq)[n]]
}

HybridAgentBIC[2] = hybrid_coefs_bic[1]
for (n in 2:length(hybrid_coefs_bic)) {
  names(hybrid_coefs_bic)[n] = utf8ToInt(names(hybrid_coefs_bic)[n]) - 96
  HybridAgentBIC[(as.numeric(names(hybrid_coefs_bic)[n])+1)] = hybrid_coefs_bic[names(hybrid_coefs_bic)[n]]
}

writeLines("\n\nSummary")
print(hybrid_sum)
writeLines("\nR Squares:")
print(hybrid_rsqs)
writeLines("\nBIC:")
print(hybrid_bic)
writeLines("\nRSQ Coefficients")
print(hybrid_coefs_rsq)
writeLines("\nBIC Coefficients")
print(hybrid_coefs_bic)
writeLines("\nRSQ Hybrid Agent")
print(HybridAgentRSQ)
writeLines("\nBIC Hybrid Agent")
print(HybridAgentBIC)
