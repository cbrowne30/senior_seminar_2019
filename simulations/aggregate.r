###############################################################################
## Script to convert montecarlo simulation output data into something useful ##
###############################################################################

library(simsalapar)
library(ggplot2)
output2df <<- function(outputFile) {
  # Read in the output file into an array
  ary <- getArray(readRDS(outputFile), "value")
  
  # Convert it to a dataframe
  output.df <- array2df(ary)
  
  # Get the number of simulations to use to calculate percentages
  n.sim <- as.numeric(output.df[nrow(output.df), "n.sim"])
  
  # Convert that into a flat contigency table to aggregate the results
  ftab.df <- as.data.frame(ftable(output.df, row.vars=c("Memory", "Pupdate"), col.vars = c("value")))
  
  # Get rid of the second half of the ftable (this is the number of crashes) 
  # This will keep the number of simulations that didn't result in a crash
  half <- nrow(ftab.df) / 2
  ftab.df <- ftab.df[0: half , c("Memory", "Pupdate", "value", "Freq")]
  
  # We probably want to look at how many crashes there were, not how many there weren't
  # so just subtract the number of non-crashes from the total number of sims
  ftab.df[0:nrow(ftab.df), "Freq"] <- n.sim - ftab.df[0:nrow(ftab.df), "Freq"]
  
  # And label those values with the correct value for crash
  ftab.df[0:nrow(ftab.df), "value"] <- 1
  
  # Calculate the percent of simulations that resulted in a crash and add it to the df
  percentage_crash <- ftab.df[1:nrow(ftab.df), "Freq"] / n.sim
  ftab.df <- cbind(ftab.df, percentage_crash)

  ftab.tab <- table(ftab.df$Memory, ftab.df$Pupdate, dnn = c("Memory", "Pupdate"))
  c.iterator <- 1
  for (i in 1:ncol(ftab.tab)) {
    for (j in 1:nrow(ftab.tab)) {
      ftab.tab[j,i] = ftab.df[c.iterator, "percentage_crash"]
      c.iterator = c.iterator + 1
    }
  }
  
  return(ftab.tab)
}

outputHeatmap <- function(output) {
  output.df <<- as.data.frame(output)
  colnames(output.df) <- c("Memory", "Pupdate", "Freq")
  output.df
  p <- ggplot(data = output.df,
         aes(x = Memory, y = Pupdate, fill = Freq)) +
    geom_tile() + geom_text(data = output.df, aes(label = Freq), color = "white") +
    labs(fill = "Frequency of Crash")
    return(p)
}

