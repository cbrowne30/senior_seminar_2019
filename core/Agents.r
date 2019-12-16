##############################################
# Document: Agents.r
# Purpose: Contains the main function for running
# a simulation 
# Classes:
#   1. OptimalAgent
#   2. RepresentativeAgent - subclass of OptimalAgent
#   3. Agent - Points to an OptimalAgent
# ToDo:
##############################################

setRefClass("OptimalAgent",
            fields = c("predictors", "connections"),
            methods = list(
                predict = function(MarketObject) {
                    if (runType == 9) {
                      if (is.null(predictors)) {
                        return(10.5)
                      } else {
                        numberPredictors = 3
                        df = data.frame(1)
                        
                        for (i in (numberPredictors - 1):0) {
                          df[paste("p", toString(i), sep = "")] = MarketObject$xx[[length(MarketObject$xx) - i]]
                        }
                        df = subset(df, select = -c(X1))
                        return(prediction(predictors, df))
                      }
                    } else if (runType == 10) {
                      if (is.null(predictors)) {
                        return(10.5)
                      } else {
                        library("FNN")
                        knn = knn.reg(train = predictors[[1]], test = MarketObject$xx[(length(MarketObject$xx) - MarketObject$size + 1):length(MarketObject$xx)], y = predictors[[2]], k = 30)
                        return(knn$pred)
                      }
                       
                    } else if (runType == 8) {
                       
                    } else {
                       
                    }
                },
                attatch = function() {
                    connections <<- connections + 1
                },
                detatch = function() {
                    connections <<- connections - 1
                }
            ))

setRefClass("RepresentativeAgent",
            contains = c("OptimalAgent"),
            fields = list(),
            methods = list())

setRefClass("Agent",
            fields = list(optimalAgent = "OptimalAgent"),
            methods = list(
                changeOptimalAgent = function(newAgent) {
                    optimalAgent$detatch()
                    optimalAgent <<- newAgent
                    optimalAgent$attatch()
                },
                initialOptimalAgent = function() {
                    optimalAgent$attatch()
                }
            ))