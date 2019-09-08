############################################################################
#  This file runs a repeated monte carlo simulation of                     #
#  main-lm-v1.2.r.  This occurs in parallel using the simsalapar           #
#  package.  This file will be run from a PBS script which will submit it  #
#  to the queue                                                            #
############################################################################

# Load the necessary packages.
# Simsalapar will load its required packages, but you need to include parallel 
# to specify the type of cluster you want.
library(simsalapar)
library(parallel)
setwd("~/Senior_Seminar/simulations/")

# Get all of the main functions in, including all the global variables
source("main-lm-v1.2.r")

#  Here is where you will create lists of possible values for a given input. 
# Right now, there are lists of different memories and probability of updates.  
mem_list <<- seq(10, 100, 10)
pupdate_list <<- seq(0.1, 0.9, 0.1)

# This value is how many runs of the simulation will occur for each combination of 
# variable values.  I have it set to 2 for testing purposes, but you will probably want more.
num_sims <<- 500

## This creates a grid of variable combinations to run simulation on.
# n.sim is the same as num_sims above and is necessary
# The other parameters have to have the same names as the parameters to the main function.
varList <<- varlist(
  n.sim = list(type="N", expr=quote(N[sim]), value = num_sims),
  Memory = list(type="grid", value=mem_list),
  Pupdate = list(type="grid", value=pupdate_list)
)

## This runs the simulations on the cluster and outputs the result to output.rds
## This result can then be read into R using the readRDS() function

#detectCores()
# clus <- makeCluster(detectCores(), type="MPI")
# clus
result = doClusterApply(varList,
                        sfile = "output.rds",
                        doOne = main,
                        cluster = makeCluster(detectCores(), type="FORK")
                        )
file.exists("output.rds")
if(file.exists("output.rds")) {
  file.rename("output.rds", "simulations/output.rds")
}
