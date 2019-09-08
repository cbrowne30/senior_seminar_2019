

# Made this file so we can run tests with the MonteCarlo package on the HPC
#library(Rmpi)
#library(MonteCarlo)
library(simsalapar)

#########################################
##      Example: t-test

# Define function that generates data and applies the method of interest

ttest<<-function(n,loc,scale){
  
  # generate sample:
  sample<-rnorm(n, loc, scale)
  
  # calculate test statistic:
  stat<-sqrt(n)*mean(sample)/sd(sample)
  
  # get test decision:
  decision<-abs(stat)>1.96
  decision
  # return result:
  #return(list("decision"=decision))
  if (decision)
    return(1)
  else
    return(0)
}

# define parameter grid:

n_grid<<-c(50,100,250,500)
loc_grid<<-seq(0,1,0.2)
scale_grid<<-c(1,2)

# collect parameter grids in list:
#param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)
#print(param_list)
varList <- varlist(
  n.sim = list(type="N", expr=quote(N[sim]), value = 100),
  n = list(type="grid", value = c(50, 100, 250, 500)),
  loc = list(type="grid", value = seq(0,1,0.2)),
  scale=list(type="grid", value = c(1,2))
)

pGrid <- mkGrid(varList)

#(hasRmpi <- require("Rmpi"))

#if (hasRmpi)

result <- doClusterApply(varList, sfile = "output.rds",
                         doOne=ttest)
#result <- doRmpi(varList, sfile = "output.rds",
 #                        doOne=ttest)


#MC_result <<-MonteCarlo(func=ttest, nrep=1000, param_list=param_list, ncpus=2)
#MC_table <<- MakeTable(output=MC_result, rows="n", cols=c("loc","scale"), digits=2, include_meta=FALSE)

