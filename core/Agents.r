
setRefClass("OptimalAgent",
            fields = list(predictors = "list", connections = "numeric"),
            methods = list(
                predict = function(x) {
                    if (runType == 9) {
                       
                    } else if (runType == 10) {
                       
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