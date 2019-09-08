library(ggplot2)
library(zoo)
library(reshape2)

# plot random price data
Price_Div = data.frame(array(merge(zoo(1:end_time),
                                   zoo(XX)),
                             dim = c(end_time,2)))
ggplot(Price_Div, aes(X1, X2)) +
  geom_line(aes(y=X2))
ggsave("Price.pdf")   # save plot

Time = array(start_time:end_time,
             dim=c((end_time - start_time), 1),
             list(1:(end_time - start_time), c("Time")))

# plot once for each of the coefficients across the models
for(i in 2:size){
  df = melt(data.frame(merge(zoo(Time),
                             zoo(coefficient_matrix[,i,]))),
            id.vars="Time")
  
  ggplot(df, aes(x=Time, y=value, group=variable, color=variable)) +
    geom_line() +
    labs(title=paste("Coefficient", i))
  
  ggsave(paste("coef", i,".pdf", sep=""))
}

# plot all coefficients for each model
for(model in 1:length(model_names)) {
  df = melt(data.frame(array(merge(zoo(start_time:(end_time - 1)),
                                   zoo(coefficient_matrix[,2:size,model])),
                          dim=c(end_time - start_time, size))),
            id.vars="X1")
  
  ggplot(df, aes(x=X1, y=value, group=variable, color=variable)) +
    geom_line() +
    labs(x="Time", y="Coefficient Value", title=paste("Model:", model_names[model]))
  ggsave(paste("model", model, ".pdf", sep=""))
}
