library(simsalapar)
library(ggplot2)

output2df <<- function(outputFile) {
  # Read in the output file into an array
  ary <- getArray(readRDS(outputFile), "value")

  # Convert it to a dataframe
  output.df <- array2df(ary)

  # Convert that into a flat contigency table to aggregate the results
  ftab.df <- as.data.frame(ftable(output.df, row.vars=c("Memory", "Pupdate"), col.vars = c("value")))

  # The dataframe only contains the frequency of 0s and 1s for 
  # each set of parameters, so split it into parts to calculate number 
  # of 0s and 1s 
  zeros.df <- ftab.df[ftab.df$value==0,]
  ones.df <- ftab.df[ftab.df$value==1,]

  # Create new dataframe that contains all combinations of Memory and Pupdate,
  # the number of 0s, and the number of 1s
  test.df <- unique(ftab.df[c("Memory", "Pupdate")])
  test.df$n.zeros <- 0
  test.df$n.ones <- 0
  
  for (i in 1:nrow(test.df)) {
    # make sure there are entries in the zeros dataframe
    if (nrow(zeros.df) > 0){
      # check to see if there were any combo matches in the zeros data
      for (j in 1:nrow(zeros.df)) {
        if (test.df$Memory[i] == zeros.df$Memory[j] && test.df$Pupdate[i] == zeros.df$Pupdate[j]){
          # record number of zeros if combo matches exist
          test.df$n.zeros[i] <- zeros.df$Freq[j]
        }
      }
    }
    # make sure there are entries in the ones dataframe
    if (nrow(ones.df) > 0){
      # check to see if there were any combo matches in the ones data
      for (j in 1:nrow(ones.df)) {
        if (test.df$Memory[i] == ones.df$Memory[j] && test.df$Pupdate[i] == ones.df$Pupdate[j]){
          # record number of ones if combo matches exist
          test.df$n.ones[i] <- ones.df$Freq[j]
        }
      }
    }
  }
  
  # Get the number of simulations to use to calculate percentages
  test.df$n.sim <- test.df$n.zeros + test.df$n.ones
  
  # Calculate the fraction of runs that result in a bubble
  test.df$fracBubble <- test.df$n.ones/test.df$n.sim

  # check data
  View(test.df)
  
  # convert data to table
  ftab.tab <- table(test.df$Memory, test.df$Pupdate, dnn = c("Memory", "Pupdate"))
  c.iterator <- 1
  for (i in 1:ncol(ftab.tab)) {
    for (j in 1:nrow(ftab.tab)) {
      ftab.tab[j,i] = test.df[c.iterator, "fracBubble"]
      c.iterator = c.iterator + 1
    }
  }
  
  return(ftab.tab)
}

# EXECUTE FUNCTION
#file <- '//Client/C$/Users/kahutchi/Desktop/Project_Georges/Financial Bubble/output11.rds'
#output2df(file)



##########################################################################################
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