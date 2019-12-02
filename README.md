# Senior_Seminar

## /simulations
- The /simulations folder contains all of the files necessary to run repeated simulations.  
- montecarlo.r is the main controller for the repeated simulations, and is called by the batch script run.run

  ### montecarlo.r 
  - Comments are pretty good in this file.  
  - For now, you should only have to change the global variable `num_sims`.  This is the number of simulation runs you want to       perform for each variable combination
  - You may also want to change `mem_list` and `pupdate_list`.  These are the different values of memory and pupdate that will be passed to the simulation
  - For each experiment, or each time you want to save some new output, make sure to change the `sfile =` argument of        `doClusterApply` to whatever you want the new output file to be called.  
      
  - Lastly, if you want to add another variable dimension to the simulation, you will need to do the following:
1. Add a new list of possible variable values in the same section as the other lists are declared
2. Add that list to the `varlist` function in the same way as the other ones.  
3. In `main-lm-v1.2.r`, add a parameter to the `main()` function with the same name as what you gave it in `varlist`, then set the environment variable using the parameter value, as is done already in the first couple lines of the function.

## run.run
- This is the batch script to submit the job to the queue.  Everything should be working here, but you may want to change the resources that you are requesting. 
-  You can do this by changing line 2.  `nodes` is obviously the number of nodes you want, and `ppn` is the number of cores you want per node.  

## How to Run
- pull from master
`git pull origin master`
  - If there are any merge conflicts (there shouldn't be), just delete the directory and re-clone the repo
  `git clone https://github.com/nkramer44/Senior_Seminar.git`
- navigate to `Senior_Seminar/simulations`

  `cd simulations`
- Submit to the queue
`qsub run.run`
- Output files will go to the root directory, NOT /simulations.  
    - If you want to look at the output file, use the `readRDS` function and give it the output file name.  To get the actual values of the simulation output, you can do `getArray(readRDS("output.rds"), "value")`.  Then you can turn that into a dataframe using the `array2df()` function. 
    
- All other input variables to the main simulation are still in `input-lm-v1.2.r` and you don't have to worry about getting rid of the inputs that you are varying in the repeated simulation, because they just get overwritten in `main()`

## Dependencies
- zoo
- xts
- glmnet
- rlist
- dplyr
- leaps
- neuralnet
- randomforest

########################  ######################### #########################

GENERAL STRATEGY of sim:

Build the UpdateMatrix with [XX = Price+Div] # changed Ex to XX 7/17/17

t > linit (note that need linit >= leval + 3)
Calculate the Optimal Paramters; send them to --> OptimalAgent
selectively(randomly) adopt Optimal Params by Market Agents
Note: "Market" at any given round stores each MarketAgent's parameters

Calculate mean of market parameters and assign them to RepAgent
Forecast P_t with market params -- i.e. calculate forecast by RepAgent
Add P_t forecast to UpdateMatrix so we can iterate -- save this forecast for
error calcs

iterate forward;
Calculate Market Price based on RepAgent's forecast (done in just 1 step
during bulk of simulation)

Update Price, Div, Int, EX vectors
Update Storage Arrays --
Send OptimalAgent Params to "log" of OptimalParams (UpdateParams)
Send RepAgent Params to "log" of MARKET MEAN PARAMS (AlphaMatrix)
Send P_t (really P_hat_t) to AlphaMatrix[t, 10] --> use that for later error
calcs

the price, etc. vectors (related 'zoo' objects are just numerically indexed
versions of the same)
will be the things to plot
also the parameters from the two storage matrices (AlphaMatrix is what we
really care about)

######################## ######################### #########################
