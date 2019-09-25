# # MEAT #
# 1) UpdateMatrix
# 2) Predict_t
# 3) Predict_t1
# 4) EstParams
# 5) RandomVec

# Update Matrix: Takes last M prices and builds Matrix to update AR model
# Changed Price.. to PriceDiv.. 7/17/17
UpdateMatrix = function(PriceDivData, M, t) {
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
Predict_t = function(matrix_data, Params, t) {
    
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
EstParams = function(new_matrix, t) { # arg1: TS of last M price(EX) obs
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
    # 3: glmnet with cross validation (lasso: alpha = 1)
    # 4: Hybrid selection agent
    # 5: glmnet with cv (elastic net: alpha = .5) #7/30/18
    # 6: glmnet with cv (ridge: alpha = 0) #8/15/18
    # 7: Smoothing Spline with LM
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
        # CV glmnet lasso
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
    
    else if (runType == 5) {
        # CV glmnet elastic net between ridge and lasso
        # probably not correct yet
        return (coef(cv.glmnet(x = MATRIX[,2:ncol(MATRIX)],
                               y = Y,
                               alpha = 0.5,
                               nfolds = 10,   # might need to be more dynamic
                               family = "gaussian"),
                     # s = "lambda.min" chooses the lambda with the best cv value
                     s = "lambda.min"))
    }
    
    else if (runType == 6) {
        # CV glmnet ridge 
        # probably not correct yet
        return (coef(cv.glmnet(x = MATRIX[,2:ncol(MATRIX)],
                               y = Y,
                               alpha = 0,
                               nfolds = 10,   # might need to be more dynamic
                               family = "gaussian"),
                     # s = "lambda.min" chooses the lambda with the best cv value
                     s = "lambda.min"))
    }
    
    else if (runType == 7) {
        # WORKING ON THIS CURRENLTY
        # IGNORE FOR NOW
        x_y = as.data.frame(cbind(Y, MATRIX))
        print(x_y)
        regression = lm(formula = Y ~ ., data = x_y)
        print(regression)
        break
        # Retrieving regression coefficients
        test_val = (as.matrix(summary(regression)$coefficients[,1],
                              nrow = size,
                              ncol = 1))
        return (smooth.spline())
        return (test_val)
    } else if (runType == 8) {
        return (neuralnet("SomeName",XX[],hidden=layers,))
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
