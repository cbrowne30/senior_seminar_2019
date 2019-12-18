# Senior Seminar 2019

## How to Clone Repo
- To clone the repo run `git clone https://github.com/cbrowne30/senior_seminar_2019.git`
  - Prior to each run we check if the repo is up to date if it is not run `git pull origin master` and if that yields merge conflicts then just delete the directory and re-clone the repo
- run `cd senior_seminar_2019`

## Single Runs
- Withing the senior_seminar_2019 directory execute the following `./simulate.sh singlerun input-lm-v1.2.txt`

## MonteCarlo Runs
- To change the number of nodes/ppn edit the run.job file in core
- To change the settings of memory and pupdate you must edit the montecarlo file.
- Withing the senior_seminar_2019 directory execute the following `./simulate.sh montecarlo input-lm-v1.2.txt newDir outputFile` where newDir is the new directory and outputFile is the output file

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