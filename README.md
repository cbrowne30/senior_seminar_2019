# Senior Seminar 2019

## How to Clone Repo
- To clone the repo run `git clone https://github.com/cbrowne30/senior_seminar_2019.git`
  - Prior to each run we check if the repo is up to date if it is not run `git pull origin master` and if that yields merge conflicts then just delete the directory and re-clone the repo
- run `cd senior_seminar_2019`

# Senior Seminar 2019

## Main Files
There are four main files utilised for starting a run.  run.job, simulate.sh, the inputfile and either monteCarlo.r or singleRun.r. 
simulate.sh is located in the base directory, senior_seminar_2019, while monteCarlo.r, singleRun.r and run.job are located within the core directory.  

1. run.job:
    Location:
    senior_seminar_2019/core/run.job
    
    Information:
      -Not used in a single run, only a Monte Carlo run
    
      -This is where the base information for the hpc is stored as well as the execution command for the hpc.  
    
      -The execution command for the parallelized run is not in the original file.  This is becayse the proper input file, 
       new directory and output file are not known until runtime.  As such, this line is pasted 
       into a copied version of the run.job that is located in the new run directory.
    
    
    What you have to change:
      - The amount of nodes and processors per node
      - The memory used.  Suggest for full Monte Carlo run to try and use at least 15GB.  However, a run might 
        not start due to using too much memory.
      - Email
      - Wall time
      
2. monteCarlo.r and singleRun.r:
    Location:
    senior_seminar_2019/core/monteCarlo.r and senior_seminar_2019/core/singleRun.r

    Information:
    These files handle the process of the runs.  They set up the variables used, the numbers of sims and the execution of the
    simulations.  It keeps track of the data and aggregates it at the end.  
    
    What you have to change:  (These may be updated)
      - mem_list.  This contains the sequence of memories used
      - pupdate_list.  This contains the sequence of pupdates used
      - num_sims.  This is how many simulations will be run for each variable combination

3. simulate.sh
    Location:
    senior_seminar_2019/simulate.sh
    
    Information:  
      -This is the script called from the command line when executing a Monte Carlo or single run 
       (The command Line process is explained in the next section).  
       
      -For a single run it solely runs the singleRun.r script and feeds it the input file.  
      
      -For a Monte Carlo run it 
       creates the specified new simulation directory and copies the core directory and input file into it.  It then copies the 
       run.job file into the directory. The base run.job file stored in does not have the proper parallelized 
       run command, as mentioned before.  As such, the next step is to copy this command into copied version of run.job that is
       located in the new run directory
    
    What you have to change:
      -  Nothing.  The only time anything has to change is if you want to change the name or location of the run.job, monteCarlo.r
         or singleRun.r file. 
         If that is the case, every occurence of the changed file in simulate.sh must be changed to the new file name 
         and the proper location must be prepended.
4.  The input file:
    Location:
    Must be contained within the senior_seminar_2019/inputs directory.  The name is just what you specify on the command line.
    
    Information:
    - This is where all the internal variables for a run are set.  Use the input-lm-v1.2.txt as a base. 
    
## How to Run

~ A Single run:

   1. Set the current directory to senior_seminar_2019 on your local machine or on the hpc.
   2. Run the command "./simulate.sh singlerun input_file" where input_file is the name of your input file located 
      in the inputs directory.
   3. Your results should show where you ran it from.  The items printed can be changed from the bottom of the singleRun.r file.


~ A Monte Carlo run:
  
  1. Set the current directory to senior_seminar_2019 on on the hpc.
  2. Change variables.  Make sure you are editing the monteCarlo.r and run.job files that are in the core directory.
  3. Run the command "./simulate.sh montecarlo input_file new_directory output_file". Input_file is the name of your 
     input file located in the inputs directory.  new_directory is the name of the directory where the run information will be stored.
     This will be created in the senior_seminar_2019 (base) directory. output_file is the name of the file that will be created in 
     the new directory that will contain the ouput information.
  4. This will add the run to the hpc queue.
  5. When doing a Monte Carlo run, a lot of files are returned from the machines into the base directory. If all these files
     start with una00, a command to to delete them is "rm una00*". The star, in this case, allows all files that begin with una00
     to be selected for removal. If you wanted all files ending with una00 the command would be "rm *una00".  However, before doing
     this removal, make sure you are in the correct directory.  The rm command deletes all the files specified in the current directory.
     Also, always check which files will be deleted using the command "dir una00*".  This will list all the files in the current
     directory with the specified parameter.
======

## Dependencies
- zoo
- xts
- glmnet
- rlist
- dplyr
- leaps
- neuralnet
- randomforest

## Questions
- Email cmbrowne@hamilton.edu for any questions