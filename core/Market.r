
setRefClass("Market",
            fields = list(optimalAgents = "list", agents = "list", repAgent = "RepresentativeAgent", 
                          prices = "vector", interestRates = "vector", dividends = "vector", xx = "vector",
                          memory = "numeric", pUpDate = "numeric", bubbles = "numeric", bubbleRound = "numeric",
                          size = "numeric", runType = "numeric", numAgents = "numeric", numRounds = "numeric", 
                          lInit = "numeric", randSeed = "numeric", lags = "numeric", powers = "numeric",
                          startPrice = "numeric", bubbleThresholdHigh = "numeric", bubbleThresholdLow = "numeric", 
                          interest = "numeric", dividend = "numeric", shockRangeDiv = "numeric", riskConstant = "numeric", 
                          riskType = "numeric", pShock = "numeric", selectionType = "numeric", oldOA = "vector", 
                          oldRep = "vector", marketMatrix = "matrix", helperFunctions = "Functions",
                          alphaMatrix = "matrix", updateParams = "matrix"),
            methods = list(
                addOptimalAgent = function(oa) {
                    optimalAgents[[length(optimalAgents) + 1]] <<- oa
                },
                addAgent = function(a) {
                    agents[[length(agents) + 1]] <<- a
                    agents[[length(agents)]]$initialOptimalAgent()
                },
                updateRepAgent = function() {
                    if (runType == 9 | runType == 10 | runType == 8) {
                        print("not done yet")
                    } else {
                        weightedVectors = matrix(nrow=length(optimalAgents),ncol=length(unlist(optimalAgents[[1]]$predictors)))
                        print(weightedVectors)
                        for (i in 1:length(optimalAgents)) {
                            print(unlist(optimalAgents[[i]]$predictors) * optimalAgents[[i]]$connections)
                            weightedVectors[i,] = unlist(optimalAgents[[i]]$predictors) * optimalAgents[[i]]$connections
                        }
                        return(colSums(weightedVectors) / length(agents))
                    }
                },
                init = function() {
                    # Initialize Prices
                    prices <<- rep(((dividend / interest) + dividend), (lags + 1))
                    dividends <<- rep(dividend, (lags + 1))
                    interestRates <<- rep(interest, (lags + 1))
                    xx <<- prices + dividends
                    if (runType < 7) {
                    
                        
                        oldRep <<- rep(0, (size+2))
                        oldRep[2] <<- (dividend / interest) + dividend
                        
                        # initialize matrix (or array) of agents
                        marketMatrix <<-
                            matrix(
                                data = 0,
                                nrow = numAgents,
                                ncol = size,
                                byrow = TRUE)
                        
                        # assign IDs
                        marketMatrix[1:numAgents]  <<-  seq(1, numAgents, 1)
                        
                        # Full weight to a_0 in prelim rounds
                        marketMatrix[1:numAgents, 2]  <<-
                            rep((dividend / interest) + dividend, numAgents)
                        
                        # OptimalAgent is a vector -- he will update to the Optimal Alphas from
                        # memory each round, & send to "UpdateParams"
                        oldOA <<- rep(0,(size+2))
                        
                        # Initialize Data Storage
                        alphaMatrix  <<-  matrix(
                            data = 0,
                            nrow = numRounds,
                            ncol = size,
                            byrow = TRUE
                        )
                        # Assign round-IDs
                        alphaMatrix[1:numRounds]  <<-  seq(1, numRounds, 1)
                        
                        # Initialize Data Storage
                        updateParams  <<-  matrix(
                            data = 0,
                            nrow = numRounds,
                            ncol = size,
                            byrow = TRUE
                        )
                        # Assign round-IDs
                        updateParams[1:numRounds]  <<-  seq(1, numRounds, 1)
                    } else {
                        if (runType == 7 | runType == 8 | runType == 9) {
                            addOptimalAgent(new("OptimalAgent", predictors= vector(mode = "list", length = size), connections = 0)) 
                        } else {
                            oa <- new("OptimalAgent", predictors= rep(0, (size+2)), connections = 0)
                            addOptimalAgent(oa)
                            
                        }
                        
                        for (agent in 1:numAgents) {
                            addAgent(new("Agent", optimalAgent = optimalAgents[[1]]))
                        }
                        updateRepAgent()
                    }
                },
                createInitialData = function(round) {
                    if (runType > 6) {
                        
                    } else {
                        # "Zoo" Matrix with new data XX=P+d  (there is an NA at XX_t)
                        # Changed X to XX 7/17/17
                        new_matrix = helperFunctions$updateMatrix(round = round,
                                                  MarketObject=.self)
                        
                        # Forecast XX_t = P_t + d_t based on RepAgent forecast rule Params
                        # changed P_t to XX_t 7/17/17
                        EX_t = helperFunctions$predictT(matrixData = new_matrix, params = oldRep[2:size], round = round)
                        
                        # Plug estimate EX_t in for unknown XX_t in data matrix
                        # Changed P_t to EX_t 7/17/17
                        new_matrix[which(index(new_matrix) == round), 2] = EX_t
                        
                        # Use it to estimate XX_(t+1) -- aka iterate forecast and then use
                        # this and the arbitrage condition to generate market price P_t1
                        P_t1 = helperFunctions$marketPrice(matrixData = new_matrix,
                                            marketParams = oldRep[2:size],
                                            round = round,
                                            MarketObject = .self)
                       
                        #APPEND P_t1 to price data
                        updateFundamentals(newPrice = P_t1, round)

                        #Storage -- c onstant until updating occurs
                        alphaMatrix[round, 2:size] <<- oldRep[2:size]
                        updateParams[round, 2:size] <<- oldRep[2:size]
                    }
                },
                updatePriceDivInt = function(newPrice, round) {
                    #insert price
                    prices <<- append(prices, newPrice)
                    
                    #insert div w/varying prob of div shock
                    randnum = runif(1, min = 0, max = 1)
                    if (randnum < pshock) {
                        dividends[round] <<- dividend + runif(n = 1,
                                                              min = -shockRangeDiv,
                                                              max = shockRangeDiv)
                    }
                    else {
                        dividends[round] <<- dividend
                    }
                    
                    #insert int
                    interestRates <<- append(interest_rates, interest)
                    
                    #update xx
                    xx <<- prices+dividends 
                },
                print = function(verbose, round) {
                    
                },
                simulation = function(round) {
                    if (round < memory) {
                        stop("t is less than memory. This should not be possible")
                    } else {
                        if (runType < 7) {
                            # "Zoo" Matrix with new data  (there is an NA at P_t)
                            new_matrix = helperFunctions$updateMatrix(round = round,
                                                                      MarketObject=.self)
                            
                            # Estimate Optimal Paramters from "memory" using Y & X
                            estimated_regression <<- helperFunctions$estParams(new_matrix = new_matrix,
                                                                               round = round, 
                                                                               MarketObject = .self)
                            
                            # OptimalAgent[2:size] <<- EstParams(new_matrix, t)
                            oldOA[2:size] <<- estimated_regression
                            
                            # writeLines("\nOptimalAgent:")
                            # print(OptimalAgent)
                            
                            # Now get a random vector of size: popsize:pupdate
                            update_traders = RandomVec()
                            # Selectively update
                            for (j in seq(1:numAgents)) {
                                if (marketMatrix[j, 1] %in% update_traders) {
                                    marketMatrix[j, 2:size] <<- oldOA[2:size]
                                }
                            }
                            
                            # Take the mean of agents' ("market") forecast rule params and assign
                            # them to RepAgent
                            oldRep[2:size] <<- helperFunctions$marketParams(MarketObject = .self)
                            
                            # Forecast P_t based on mean params (which are already stored in
                            # RepAgent)
                            # Changed P_t to EX_t 7/17/17
                            EX_t = helperFunctions$predictT(matrixData = new_matrix, params = oldRep[2:size], round)
                            
                            # Plug estimate for P_t in
                            # Changed P_t to EX_t 7/17/17
                            new_matrix[which(index(new_matrix) == round), 2] = EX_t
                            
                            # Update Storage Containers
                            alphaMatrix[round, 2:size] <<- oldRep[2:size]
                            
                            # Use means of Market Forecast Rule Params to find market price based on
                            # discounted mean forecast
                            oldRep[size] <<- helperFunctions$marketPrice(matrixData = new_matrix,
                                                            marketParams = oldRep[2:size],
                                                            round = round,
                                                            MarketObject = .self)
                            
                            # Log of 'updates'
                            updateParams[round, 2:size] <<- oldOA[2:size]
                            updateFundamentals(newPrice = oldRep[size], round = round)
                        }
                    }
                },
                updateFundamentals = function(newPrice, round) {
                    #insert price
                    prices <<- append(prices, newPrice) 
                    
                    #insert div w/varying prob of div shock
                    randnum = runif(1, min = 0, max = 1)
                    if (randnum < pshock) {
                        dividends[round] <<- dividend + runif(n = 1,
                                                          min = -shockRangeDiv,
                                                          max = shockRangeDiv)
                    } else {
                        dividends[round] <<- dividend
                    }
                    
                    #insert int
                    interestRates <<- append(interestRates, interest)
                    
                    #update xx
                    xx <<- prices+dividends 
                }
            ))
