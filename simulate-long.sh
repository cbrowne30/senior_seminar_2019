# Controlling script for running multiple simulations at once.
# The script creates a new simulation directory and changes 
# the files in it to correct paths/output file names and such.

# TO RUN:
# ./simulate.sh new_directory_name input_file_name output_file_name

# I've made it so there is an input file directory containing any number
# of input files.  This way you can run repeated experiments with the same input
# file without having to copy it over every time. 




echo "Creating new simulation directory" $1
if [ -d $1 ]; then
    echo "Deleting existing directory"
    rm -rf $1
fi

# Make a copy of the simulations folder
cp -r simulations/ $1

# Copy the input file specified by second argument into new directory
cp inputs/$2 $1/$2
# Copy main file into new directory
cp core/main-lm-v1.2.r $1/

echo "Setting working directory as ~/Senior_Seminar/"$1 "and output file as" $3
wd="~/Senior_Seminar/"$1

# Replace setwd("~/Senior_Seminar/simulations") with setwd("Senior_Seminar/new_directory")
sed -i 's.\/simulations.\/'$1'.' $1/montecarlo.r

# Replace other occurences of simulations in montecarlo.r
sed -i 's.simulations\/.'$1'\/.' $1/montecarlo.r

# Replace output file name with new output file name
sed -i 's/output.rds/'$3'/g' $1/montecarlo.r

# Replace input file path in main file to local input file path
sed -i -e 's/input-lm-v1\.2\.txt/'$1'\/'$2'/' $1/main-lm-v1.2.r

# Then move into the new directory and submit the file to the queue
cd $1
qsub run-long.run

#The output file gets moved to the new directory from the R script once is it generated


