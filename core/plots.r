##############################################
# Document: Functions.r
# Purpose: Used for plotting a simulation - must
# be able to pass in a MarketObject
# Functions:
#   1. multiPlot
# ToDo:
##############################################

library(ggplot2)
library(zoo)
library(reshape2)


# Multiple plot function
# This is from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

marketObject <<- readRDS("testMarket")

Price_Div = data.frame(array(merge(zoo(1:length(marketObject$xx)),
                                   as.numeric(marketObject$xx)),
                             dim = c(length(marketObject$xx), 2)))
price_div_graph = ggplot(Price_Div, aes(X1, X2)) + geom_line(aes(y=X2)) + labs(x = "Time", y = "Price + Dividend")

PRICES = data.frame(array(merge(zoo(1:length(marketObject$prices)),
                                as.numeric(marketObject$prices)),
                          dim = c(length(marketObject$prices), 2)))

price_graph = ggplot(PRICES, aes(X1, X2)) + geom_line(aes(y = X2)) + labs(x = "Time", y = "Price")

div = data.frame(array(merge(zoo(1:length(marketObject$dividends)), as.numeric(marketObject$dividends)), dim = c(length(marketObject$dividends), 2)))

div_graph = ggplot(div, aes(X1, X2)) + geom_line(aes(y=X2)) + labs(x = "Time", y = "Dividend")

interests = data.frame(array(merge(zoo(1:length(marketObject$interestRates)), as.numeric(marketObject$interestRates)), dim = c(length(marketObject$interestRates), 2)))

interest_graph = ggplot(interests, aes(X1, X2)) + geom_line(aes(y=X2)) + labs(x = "Time", y = "Interest")

# plot all fundementals together
multiplot(price_div_graph, price_graph, div_graph, interest_graph, cols=2)


Time = array(1:length(marketObject$xx),
             dim=c(marketObject$numRounds, 1),
             list(1:marketObject$numRounds, c("Time")))

alphaMatrix = array(marketObject$alphaMatrix[(marketObject$numRounds+1):length(marketObject$alphaMatrix)],
                    dim = c(marketObject$numRounds, marketObject$size - 1),
                    dimnames = list(1:marketObject$numRounds, 1:(marketObject$size - 1)))

df = melt(data.frame(merge(zoo(Time),
                           zoo(alphaMatrix))),
          id.vars="Time")

ggplot(df, aes(x=Time, y=value, group=variable, color=variable)) + geom_line() + labs(x="Time", y="Coefficient Value")



# Put some Zoo Objs. Together
# Fundamentals = merge(XX, Prices, Dividends, Interest) #changed EX to XX 7/17/17
# plot(
#   Fundamentals,
#   plot.type = "multiple",
#   xlab = 'Round',
#   ylab = c('P+div', 'Price', 'Div', 'I-rate')
# ) #changed EX to XX 7/17/17

#readline(prompt="Press RET to continue...")

# just prices&(P+d)
#P_XX = merge(XX, Prices) # change EX to XX 7/17/17
# plot(
#   P_XX,
#   plot.type = "multiple",
#   xlab = 'Round',
#   ylab = c('P+div', 'Price')
# ) #changed EX to XX 7/17/17

#readline(prompt="Press RET to continue...")

# reset graphs
#dev.off()

# zoo plots (great, another graph syntax in R...)
# pre-bubble dynamics
#plot(head(XX,-2), xlab = 'Round', ylab = 'Price+Div')

#readline(prompt="Press RET to continue...")

#plot(head(Prices,-2), xlab = 'Round', ylab = 'Price')

#readline(prompt="Press RET to continue...")

# div int
#plot(Dividends, xlab = 'Round', ylab = 'Div')

#readline(prompt="Press RET to continue...")

#plot(Interest, xlab = 'Round', ylab = 'r')

#readline(prompt="Press RET to continue...")
#print("Starting ggplot")
#print(dev.cur())

# try w/ GG!
# ggplot(Fundamentals, aes(index(Fundamentals), XX)) +
#   geom_line(aes(color = I("red")), size = .5) +
#   labs(x = "Round", y = "(Price + Div)_t", title = "Realized Pr+Div")

#readline(prompt="Press RET to continue...")

# try w/ GG --- thin line
# ggplot(Fundamentals, aes(index(Fundamentals), XX)) +
#   geom_line(aes(color = I("blue")), size = .2) +
#   labs(x = "Round", y = "(Price + Div)_t", title = "Realized Pr+Div")

#readline(prompt="Press RET to continue...")


#reset graphs
#dev.off()

#DataFrame of ALPHAS
# A_MAT = data.frame(AlphaMatrix)

#if not crash
# df_Al = A_MAT

#if crash
#df_Al = A_MAT[1:crash_t,]

# A_0
# ggplot(df_Al, aes(X1, X2)) +
#   geom_line(aes(colour = 'red'), size = .75) +
#   labs(x = "Round", y = "a0_Bar", title = "Rep Agent a0 (Forecasat Rule Intercept)")

#readline(prompt="Press RET to continue...")

# A_1
# ggplot(df_Al, aes(X1, X3)) +
#   geom_line(aes(colour = 'blue'), size = .75) +
#   labs(x = "Round", y = "a1_Bar", title = "Rep Agent a1 (L1 Coeff)")
# 
# readline(prompt="Press RET to continue...")

# A_2
# ggplot(df_Al, aes(X1, X4)) +
#   geom_line(aes(colour = 'blue'), size = .75) +
#   labs(x = "Round", y = "a1_Bar", title = "Rep Agent a2 (L1_sq Coeff)")
# 
# readline(prompt="Press RET to continue...")
# 
# # A_3
# ggplot(df_Al, aes(X1, X5)) +
#   geom_line(aes(colour = 'blue'), size = .75) +
#   labs(x = "Round", y = "a1_Bar", title = "Rep Agent a3 (L2 Coeff)")
# 
# readline(prompt="Press RET to continue...")
# 
# # A_4
# ggplot(df_Al, aes(X1, X6)) +
#   geom_line(aes(colour = 'blue'), size = .75) +
#   labs(x = "Round", y = "a1_Bar", title = "Rep Agent a4 (L2_sq Coeff)")
# 
# readline(prompt="Press RET to continue...")
# 
# # A_5
# ggplot(df_Al, aes(X1, X7)) +
#   geom_line(aes(colour = 'blue'), size = .75) +
#   labs(x = "Round", y = "a1_Bar", title = "Rep Agent a5 (L2_sq Coeff)")
# 
# readline(prompt="Press RET to continue...")
# 
# # A_6
# ggplot(df_Al, aes(X1, X8)) +
#   geom_line(aes(colour = 'blue'), size = .75) +
#   labs(x = "Round", y = "a1_Bar", title = "Rep Agent a6 (L1*L2 Coeff)")
# 
# readline(prompt="Press RET to continue...")

#Plot all (this is actually pretty cool)
# ggplot(df_Al, aes(X1, X2)) +
#   geom_line(aes(y = X2), color = I("red")) +
#   geom_line(aes(y = X3), color = I("blue")) +
#   geom_line(aes(y = X4), color = I("green")) +
#   geom_line(aes(y = X5), color = I("yellow")) +
#   geom_line(aes(y = X6), color = I("cyan")) +
#   geom_line(aes(y = X7), color = I("purple")) +
#   geom_line(aes(y = X8), color = I("brown")) +
#   labs(x = "Round", y = "a_Bars")

#readline(prompt="Press RET to end...")
