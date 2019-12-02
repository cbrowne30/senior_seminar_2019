# Controlling script for running multiple simulations at once.
# The script creates a new simulation directory and changes 
# the files in it to correct paths/output file names and such.

# TO RUN:
# ./simulate.sh new_directory_name input_file_name output_file_name

# I've made it so there is an input file directory containing any number
# of input files.  This way you can run repeated experiments with the same input
# file without having to copy it over every time. 

### Functions ###

monteCarlo() {
  # TO RUN montecarlo simulation:
  # ./simulate.sh montecarlo inputFileName newDirectoryName outputFileName
  echo "Running MonteCarlo simulation"
  echo "Creating new simulation directory" $2
  if [ -d $2 ]; then
    echo "Deleting existing directory"
    rm -rf $2
  fi
  
  # Make a copy of the simulations folder
  cp -r core/ $1
  
  # Copy the input file specified by second argument into new directory
  cp inputs/$1 $2/$1
  
  qsub run.job $2
}

singleRun() {
  echo "Running a single simulation"
  RScript core/singleRun.r "inputs/$1"
}

unitTests() {
  echo "Running Unit Tests"
}


if [ $1 = "montecarlo" ]; then
  monteCarlo $2 $3
elif [ $1 = "unittests" ]; then
  unitTests
else
  singleRun $2
fi


