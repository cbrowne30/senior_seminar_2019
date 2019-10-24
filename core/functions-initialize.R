
# # APERITIF #
# 1) GetMacros
# 2) initialize_prices
# 3) initialize_market
# 4) initialize_storage
# 5) UpdateFundamentals

# clear workspace
rm(list = ls())

# load packages
# setwd("F:/dkraynak/georges/agent based trading")
# setwd("F:/cgeorges/risk R")
# setwd("~/Senior_Seminar")
setwd(".")

######################## APERITIF #########################

#Package/Dependency Checks and Installation
DependencyCheck = function() {
    dependencies = c("zoo", "xts", "glmnet", "rlist", "dplyr", "leaps", "neuralnet", "randomForest", "Rmpi")
    for (depen in dependencies) {
        if(depen %in% rownames(installed.packages()) == FALSE){
            install.packages(depen, dependencies = TRUE)
        }
        library(depen, character.only = TRUE) 
    }
}

# Get Inputs
GetMacros = function(inputfile) {
    # Get macro inputfile
    input = read.table(inputfile, header = FALSE, sep = " ")
    input = data.frame(input)
    
    # get rid of scientific notation
    options(scipen = 999)
    # names
    colnames(input) <- c("Varnames", "V2")
    
    # macro vars
    rounds <<- subset(input, Varnames == "rounds")[[2]]
    popsize <<- subset(input, Varnames == "popsize")[[2]]
    bubbleThresholdHigh <<-
        subset(input, Varnames == "bubbleThresholdHigh")[[2]]
    bubbleThresholdLow <<-
        subset(input, Varnames == "bubbleThresholdLow")[[2]]
    pupdate <<- subset(input, Varnames == "pupdate")[[2]]
    
    powers <<- subset(input, Varnames == "powers")[[2]]
    lags <<- subset(input, Varnames == "lags")[[2]]
    layers <<- subset(input, Varnames == "layers")[[2]]
    numBubbles <<- subset(input, Varnames == "numBubbles")[[2]]
    
    # initial conditions
    startPrice <<- subset(input, Varnames == "startPrice")[[2]]
    dividend <<- subset(input, Varnames == "dividend")[[2]]
    interest <<- subset(input, Varnames == "interest")[[2]]
    
    # note: rename leval=memory
    memory <<- subset(input, Varnames == "memory")[[2]]
    linit <<- subset(input, Varnames == "linit")[[2]]
    # maxiter???? <<- subset(input,Varnames == "linit")[[2]]
    shockRange_div <<- subset(input, Varnames == "shockRangeDiv")[[2]]
    runType <<- subset(input, Varnames == "runType")[[2]]
    selection_type <<- subset(input, Varnames == "selection_type")[[2]]
    
    pshock <<- subset(input, Varnames == "pshock")[[2]]
    randSeed <<- subset(input, Varnames == "randSeed")[[2]]
    risk_constant <<- subset(input, Varnames ==  "risk_constant")[[2]]
    risk_type <<- subset(input, Varnames == "risk_type")[[2]]
    
    # if (runType == 9) {
    #     hidden <<- subset(input, Varnames == "hidden")[[2]]
    #     CheckInputs(hidden, 3)
    #     threshold <<- subset(input, Varnames == "threshold")[[2]]
    #     CheckInputs(threshold, 0.01)
    #     stepmax <<- subset(input, Varnames == "stepmax")[[2]]
    #     CheckInputs(stepmax, 1e+05)
    #     rep <<- subset(input, Varnames == "rep")[[2]]
    #     CheckInputs(rep, 1)
    #     startweights <<- subset(input, Varnames == "startweights")[[2]]
    #     CheckInputs(startweights, NULL)
    #     learningrate_limit <<- subset(input, Varnames == "learningrate_limit")[[2]]
    #     CheckInputs(learningrate_limit, NULL)
    #     learningrate_factor <<- subset(input, Varnames == "learningrate_factor")[[2]]
    #     CheckInputs(learningrate_factor, list(minus = 0.5, plus = 1.2))
    #     learningrate <<- subset(input, Varnames == "learningrate")[[2]]
    #     CheckInputs(learningrate, NULL)
    #     lifesign <<- subset(input, Varnames == "lifesign")[[2]]
    #     CheckInputs(lifesign, "none")
    #     lifesign_step <<- subset(input, Varnames == "lifesign_step")[[2]]
    #     CheckInputs(lifesign_step, 1000)
    #     algorithm <<- subset(input, Varnames == "algorithm")[[2]]
    #     CheckInputs(algorithm, "rprop+")
    #     err_fct <<- subset(input, Varnames == "err_fct")[[2]]
    #     CheckInputs(err_fct, "sse")
    #     act_fct <<- subset(input, Varnames == "act_fct")[[2]]
    #     CheckInputs(act_fct, "logistic")
    #     linear_output <<- subset(input, Varnames == "linear_output")[[2]]
    #     CheckInputs(linear_output, TRUE)
    #     exclude <<- subset(input, Varnames == "exclude")[[2]]
    #     CheckInputs(exclude, NULL)
    #     constant_weights <<- subset(input, Varnames == "constant_weights")[[2]]
    #     CheckInputs(constant_weights, NULL)
    #     likelihood <<- subset(input, Varnames == "likelihood")[[2]]
    #     CheckInputs(likelihood, FALSE)
    # }
}



# INIT
initialize_prices = function(sim_rounds) {
    MarketObject$prices = rep(((dividend / interest) + dividend), (lags + 1))
    MarketObject$dividends = rep(dividend, (lags + 1))
    MarketObject$interestRates = rep(interest, (lags + 1))
    MarketObject$xx = MarketObject$prices + MarketObject$dividends


    #PRETTY SURE GETTING RID OF THIS
    #Periods <<- seq(1:sim_rounds)
    
    # Initialize vector of prices of length 3
    prices <<- rep(((dividend / interest) + dividend), (lags + 1))
    
    # Initialize a similar vector to prices, but make the first three spots = dividend
    dividends <<- rep(dividend, (lags + 1))
    
    # Initialize another vector and add the interest rate to the first three slots
    interest_rates <<- rep(interest, (lags + 1))
    
    #IMPORTANT:
    # xx is a vector of length sim_rounds that has the sum of the two vectors 
    # Each element in xx is the sum of elements i in both vectors 
    xx <<- prices+dividends # changed ex_t to xx 7/17/17
    #Note: Currently I just initialize with 3 copies of startPrice & start loop at round 3 (lags)
}



# Setup RepAgent and Market Array
initialize_market = function(num_agents) {
    # RepAgent is just a vector -- he will store the mean parameters and calc.
    # market price
    RepAgent <<- rep(0, (size+2))
    RepAgent[2] <<- (dividend / interest) + dividend
    # initialize matrix (or array) of agents
    Market <<-
        matrix(
            data = 0,
            nrow = num_agents,
            ncol = size,
            byrow = TRUE
        )

    # assign IDs
    Market[1:num_agents]  <<-  seq(1, num_agents, 1)


    if (runType == 7 | runType == 8 | runType == 9) {
        MarketObject$addOptimalAgent(new("OptimalAgent", predictors= vector(mode = "list", length = size), connections = 0)) 
    } else {
        MarketObject$addOptimalAgent(new("OptimalAgent", predictors= list(c((dividend / interest) + dividend, rep(0, size - 2))), connections = 0))
    }
    for (agent in 1:num_agents) {
        MarketObject$addAgent(new("Agent", optimalAgent = MarketObject$optimalAgents[[1]]))
    }
    
    MarketObject$updateRepAgent()
    
    # Full weight to a_0 in prelim rounds
    Market[1:num_agents, 2]  <<-
        rep((dividend / interest) + dividend, num_agents)
    
    # OptimalAgent is a vector -- he will update to the Optimal Alphas from
    # memory each round, & send to "UpdateParams"
    OptimalAgent <<- rep(0,(size+2))
}



# set up Alpha & Forecast array
initialize_storage = function(sim_rounds) {
    # Initialize Data Storage
    AlphaMatrix  <<-  matrix(
        data = 0,
        nrow = sim_rounds,
        ncol = size,
        byrow = TRUE
    )
    # Assign round-IDs
    AlphaMatrix[1:sim_rounds]  <<-  seq(1, sim_rounds, 1)
    
    # Initialize Data Storage
    UpdateParams  <<-  matrix(
        data = 0,
        nrow = sim_rounds,
        ncol = size,
        byrow = TRUE
    )
    # Assign round-IDs
    UpdateParams[1:sim_rounds]  <<-  seq(1, sim_rounds, 1)
}


#Update Price, Dividends, Interest (include commented out shock)
UpdateFundamentals = function(newPrice, t) { 
    #insert price
    prices <<- append(prices, newPrice) 
    
    #insert div w/varying prob of div shock
    randnum = runif(1, min = 0, max = 1)
    if (randnum < pshock) {
        dividends[t] <<- dividend + runif(n = 1,
                                          min = -shockRange_div,
                                          max = shockRange_div)
    }
    else {
        dividends[t] <<- dividend
    }
    
    #insert int
    interest_rates <<- append(interest_rates, interest)
    
    #update xx
    xx <<- prices+dividends 
}

CheckInputs = function(v, defaultValue) {
    if (length(v) == 0) {
        v = defaultValue
    }
}
