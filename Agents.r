

OptimalAgent = setRefClass("OptimalAgent",
                           fields = list(predictors = "list"),
                           methods = list(
                               predict = function(x) {
                                   if (runType == 9) {
                                       
                                   } else if (runType == 10) {
                                       
                                   } else if (runType == 8) {
                                       
                                   } else {
                                       
                                   }
                               }
                           ))

Agent = setRefClass("Agent",
                    fields = list(optimalAgent = "OptimalAgent"),
                    methods = list(
                        changeOptimalAgent = function(newAgent) {
                            optimalAgent <<- newAgent
                        }
                    ))
oa <- new("OptimalAgent", predictors= list(1, 2, 3))
print(oa)
a <- new("Agent", optimalAgent = oa)
print(a)
oa2 <- new("OptimalAgent", predictors= list(4, 5))
a$changeOptimalAgent(oa2)
print(a)