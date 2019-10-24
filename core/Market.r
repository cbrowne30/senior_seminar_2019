
setRefClass("Market",
            fields = list(optimalAgents = "list", agents = "list", repAgent = "RepresentativeAgent", 
                          prices = "vector", interestRates = "vector", dividends = "vector", xx = "vector",
                          memory = "numeric", pUpDate = "numeric", bubbles = "numeric", bubbleRound = "numeric",
                          failedTest = "logical"),
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
                updatePriceDivInt = function(newPrice, round) {
                    #insert price
                    print(prices)
                    print("hehe")
                    print(newPrice)
                    prices <<- append(prices, newPrice)
                    
                    #insert div w/varying prob of div shock
                    randnum = runif(1, min = 0, max = 1)
                    if (randnum < pshock) {
                        dividends[round] <<- dividend + runif(n = 1,
                                                              min = -shockRange_div,
                                                              max = shockRange_div)
                    }
                    else {
                        dividends[round] <<- dividend
                    }
                    
                    #insert int
                    interestRates <<- append(interest_rates, interest)
                    
                    #update xx
                    xx <<- prices+dividends 
                },
                bubble = function(round) {
                    bubbles <<- bubbles + 1
                    bubbleRound <<- round
                },
                print = function(verbose) {
                    
                }
            ))
