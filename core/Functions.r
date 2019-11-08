##############################################
# Document: Functions.r
# Purpose: Contains the functions needed for 
# running linear simulations
# Functions:
#   1. predictT
#   2. updateMatrix
#   3. marketPrice
#   4. MSFE
#   5. hetero_MSFE
#   6. marketParams
#   7. estParams
# ToDo:
##############################################

predictT = function(matrixData, params, round) {
    # Get most recent COMPLETE row of data
    pred_inputs = matrixData[which(index(matrixData) == round - 1), ]
    x_hat_t = pred_inputs  %*% (params) # changed p to x 7/17/17
    return(x_hat_t) # changed p to x 7/17/17
}

updateMatrix = function(round, MarketObject) {
    # Grab Most Recent 50 Price (and Div)
    # Chanced Price.. to PriceDiv.. 7/17/17

    if (round <= MarketObject$memory + (MarketObject$lags + 1)) {
        PriceDivVec = MarketObject$xx[1:round]
        # early periods -- get all available price data
        mem = seq(1, round, 1)
    } else {
        # So early periods get lagged values ie. length(54)
        # Changed PriceVec to PriceDivVec 7/17/17
        PriceDivVec = MarketObject$xx[(round - MarketObject$memory - (MarketObject$lags + 1)):round]
        
        # later periods -- bounded memory
        mem = seq((round - MarketObject$memory - (MarketObject$lags + 1)), round, 1) #length(54)
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
    for (i in 1:MarketObject$lags){
        prev_lag <- lag(prev_lag, k=1, na.pad = TRUE)
        prev_lag_list <- list.append(prev_lag_list, i = prev_lag)
        lag_list <- list.append(lag_list, i = prev_lag)
        
        if (MarketObject$powers > 1) {
            for (j in 2:MarketObject$powers){
                lag_list <- list.append(lag_list, j = prev_lag ** j)
            }
        }
    }
    
    
    # Use prev_lag_list to get cross products, add to lag_list
    if (MarketObject$lags > 1){
        for (lag1 in 1:(length(prev_lag_list)-1)){
            for (lag2 in (lag1+1):length(prev_lag_list)){
                lag_list <- list.append(lag_list, inter = (prev_lag_list[lag1]$i) * (prev_lag_list[lag2]$i))
            }
        }
    }
    
    # Merge all zoo objects in lag_list, and return
    MatrixUpdate = do.call("merge", lag_list)
    MatrixUpdate = window(MatrixUpdate, seq((round - MarketObject$memory - 1), round, 1))
    return(MatrixUpdate)
}

marketPrice = function(matrixData, marketParams, round, MarketObject) {
    # take mean of Price + Dividend
    
    # Similar to forecast(i): Dot-Product of MKT-Params[2-8]=beta1 and
    # UpdateMatrix[1]
    
    mkt_inputs = matrix(tail(matrixData, 1), ncol = MarketObject$size - 1)
    
    # dot prod
    X_hat_t1 = mkt_inputs %*% (marketParams) # changed P to X 7/17/17
    
    
    # R.E.E. mean
    
    # Initialize market price without risk aversion
    market_price = X_hat_t1 / (1 + MarketObject$interestRates[round-1])
    
    # If you are past linit and including risk, reset market price
    # Should start at linit + 1
    if (MarketObject$riskType > 0) {   # Risk type = 0 means no endogenous risk
        if (round - 1 > MarketObject$lInit) {
            if (MarketObject$riskType == 1) { # Risk type = 1 means homogeneous risk
                market_price = (X_hat_t1 + (MarketObject$riskConstant * MSFE(matrixData, round, MarketObject$oldRep[2:size], MarketObject))) / (1 + MarketObject$interestRates[round-1])
            } else {    # Risk type = 2 means heterogeneous risk
                market_price = (X_hat_t1 + (MarketObject$riskConstant * hetero_MSFE(matrixData, round, MarketObject))) / (1 + MarketObject$interestRates[round-1])
            }
        }
    }
    
    return(market_price)
}

MSFE = function(matrixData, round, agent, MarketObject) {
    sum_sq_error = 0
    
    # Index to start calculating MSFE at
    start_index = MarketObject$lInit
    
    # Don't want to look further back than memory
    # if the period is (memory) past linit or more, then only go back memory amount
    if (round - MarketObject$memory >= start_index) {
        start_index = round - MarketObject$memory + 1
    }
    
    for (period in start_index: round - 1) {
        period_prediction = predictT(matrixData = matrixData, params = agent, round = period)
        error_sq = (MarketObject$prices[period] - period_prediction)^2
        sum_sq_error = sum_sq_error + error_sq
    }
    MSFE = (sum_sq_error / (round - 1 - start_index))
    return(MSFE)
}

hetero_MSFE = function(matrixData, round, MarketObject) {
    # Computes a risk aversion value by calculating the mean squared 
    # forecast error for [memory] forecasts back.  Instead of using the RepAgent
    # to get the MSFE, we calculate it for all of the agents in the market.
    # NOTE: This will be much slower than homogeneous risk function because it has to calculate
    # MSFE for all agents, not just one.
    total_msfe = 0
    
    for (i in 1:nrow(MarketObject$marketMatrix)) {
        total_msfe = total_msfe + MSFE(matrixData, round, MarketObject$marketMatrix[i, 2:MarketObject$size])
    }
    
    avg_msfe = total_msfe / nrow(MarketObject$marketMatrix)
    return (avg_msfe)
}

marketParams = function(MarketObject) {
    # take mean over columns 2:8 of market
    mkt = data.frame(MarketObject$marketMatrix)
    mean_params = c()
    for (i in 2:MarketObject$size){
        mean_params <- c(mean_params, mean(mkt[, i], na.rm = TRUE))
    }
    return(mean_params)
}

estParams = function(new_matrix, round, MarketObject) { # arg1: TS of last M price(EX) obs
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
    Y = new_matrix[2:which(index(new_matrix) == round - 1), 2]
    
    X = matrix(new_matrix, ncol = MarketObject$size - 1)
    
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
    
    if (MarketObject$runType == 0) {
        # LM
        # Creating dataset to run ols
        
        #print(length(matrix(Y, ncol= 1)))
        #print(length(matrix(X, ncol=2)))
        x_y = as.data.frame(cbind(Y, MATRIX))
        regression = lm(formula = Y ~ ., data = x_y)
        
        # Retrieving regression coefficients
        test_val = (as.matrix(summary(regression)$coefficients[,1],
                              nrow = MarketObject$size,
                              ncol = 1))
        return (test_val)
    }
    else if (MarketObject$runType == 1) {
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
    else if (MarketObject$runType == 2) {
        # Standard glmnet lambda = 1
        return (coef(glmnet(x = MATRIX[,2:ncol(MATRIX)],
                            y = Y,
                            alpha = 0.9,
                            # list should contain at least 3 lambda values
                            lambda = seq(100, 0, -1)),
                     # s controls which lambda value is chosen from the list
                     s = .1))
    }
    else if (MarketObject$runType == 3) {
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
    else if (MarketObject$runType == 4) {
        HybridAgent = rep(0,MarketObject$size)
        # Perform the hybrid model selection regression
        regfit_hybrid = regsubsets(x = MATRIX[,2:ncol(MATRIX)], 
                                   y = Y, nvmax = MarketObject$size, 
                                   method = (if(MarketObject$selectionType == 0)
                                       "forward"
                                       else if(MarketObject$selectionType == 1)
                                           "backward"
                                       else if (MarketObject$selectionType == 2)
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
    
    else if (MarketObject$runType == 5) {
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
    
    else if (MarketObject$runType == 6) {
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
    
    else if (MarketObject$runType == 7) {
        # WORKING ON THIS CURRENLTY
        # IGNORE FOR NOW
        x_y = as.data.frame(cbind(Y, MATRIX))
        print(x_y)
        regression = lm(formula = Y ~ ., data = x_y)
        print(regression)
        break
        # Retrieving regression coefficients
        test_val = (as.matrix(summary(regression)$coefficients[,1],
                              nrow = MarketObject$size,
                              ncol = 1))
        return (smooth.spline())
        return (test_val)
    } else if (MarketObject$runType == 8) {
        
        x_y = as.data.frame(cbind(Y, MATRIX))
        smoothSpline <- smooth.spline(as.vector(x_y[,3]),as.vector(x_y[,1]), df=6)
        print(x_y)
        x_y[,1] = predict(smoothSpline, x=as.vector(x_y[,3]))$y
        regression = lm(formula = Y ~ ., data = x_y)
        print(smoothSpline)
        
        # Retrieving regression coefficients
        test_val = (as.matrix(summary(regression)$coefficients[,1],
                              nrow = MarketObject$size,
                              ncol = 1))
        
        return (test_val)
    } else if (MarketObject$runType == 9) {
        print(MarketObject$xx[1])
        print(MarketObject$xx)
        print(length(MarketObject$xx))
        predictor1 = MarketObject$xx[1:(length(MarketObject$xx) - 2)]
        predictor2 = MarketObject$xx[2:(length(MarketObject$xx) - 1)]
        label = MarketObject$xx[3:length(MarketObject$xx)]
        df = data.frame(predictor1, predictor2, label)
        print(predictor1)
        print(predictor2)
        print(label)
        customFunction <- function(x) log(1 + exp(x))
        nn = neuralnet(label~predictor1+predictor2, data=df, hidden = 3, threshold = 0.01,
                       stepmax = 1e+05, rep = 1, startweights = NULL,
                       learningrate.limit = NULL, learningrate.factor = list(minus = 0.5,plus = 1.2), 
                       learningrate = NULL, lifesign = "none",
                       lifesign.step = 1000, algorithm = "rprop+", err.fct = "sse",
                       act.fct = "logistic", linear.output = TRUE, exclude = NULL,
                       constant.weights = NULL, likelihood = FALSE)
        print(nn$act.fct(3))
        plot(nn)
        print(customFunction(3))
        print(hidden)
        print(memory)
        print(compute(nn, data.frame(c(10.5), c(10.32643))))
        print(nn$result.matrix)
        stop()
        
        
    }
    
}