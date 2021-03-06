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
                        print("YUP")
                        return(10)
                      } else {
                        library("FNN")
                        #trainSet = matrix(ncol=3,nrow=0)
                        #for (x in seq(1, length(predictors[[1]]), by=3)) {
                        #  trainSet = rbind(trainSet, predictors[[1]][x:x+3])
                        #}
                        #printSomething(trainSet)
                        #stop()
                        knn = knn.reg(train = predictors[[1]], test = MarketObject$xx[length(MarketObject$xx):length(MarketObject$xx)], y = predictors[[2]],
                                      k = 15, algorithm = "cover_tree")
                        return(knn$pred)
                      }
                    } else if (runType == 11) {
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