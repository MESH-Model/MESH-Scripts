#!/bin/bash
    # The above must be included for executable bash scripts

# _____GENERAL_________________________________________________________________

# Almost any bash command can be put into a script for execution

# _____STOP IF ERROR____________________________________________________________________
set -e # Stops if non-zero status

# _____RSYNC____________________________________________________________________

user=username
path=projects/rpp-hwheater/username/MESH_Project_Baker_Creek

# Form: rsync -rultvzi <from path> <to path>

# Push Example:
  # Dry Run
rsync -rultvzi --dry-run --exclude '.git' ./ $user@graham.computecanada.ca:~/$path

  # Regular
rsync -rultvzi --exclude '.git' ./ $user@graham.computecanada.ca:~/$path

# GRAHAM-SPECIFIC NOTE:
# Don't use "-a" for transferring to GRAHAM. Or if you do, you must also add "--no-g" and "--no-p". Otherwise, you will get a disk quota exceeded error message.

# r - recursive
# u - update; skip files that are newer on the receiver
# l - synlinks remain as symlinks
# t - preserve modification times
# v - increase verbosity
# z - compress file data during transfer
# i - output a change-summary for all updates

# DON'T USE
# a - archive; equals -rlptgoD; particularly -p and -g
# p - preserve permissions
# g - preserve group

# _____FIND-REPLACE IN TEXT FILE________________________________________________
###
sed -i -e 's/<find this>/<replace with this>/g' <in this file>

Ex:
sed -i -e 's/job-name=Scenario1_1/job-name='$subfolder'/g' $subfolder/Input/submitjob.sh
#and
sed -i -e 's/{001..010}/{'$Y2'..'$Z2'}/g' $subfolder/Input/submitjob.sh

# Note the use of additional single=quotes around variable names when used in the find or replace part

# _____DO MATH ON VARIABLES_____________________________________________________

Ex.
trials=4
numfolders=2
let subdiv=trials/numfolders # Note: don't need "$" when calling the variable in the "let" statement

# _____IF STATEMENTS ___________________________________________________________

# Test the value of a variable (ex. "remainder")
if (($remainder > 0)) ; then
  # do something
else
  # do something else
fi

# Check if a directory exists (-d flag)
if [[ ! -d ${subfolder} ]] ; then  # The "!" means "not", so checks if the directory doesn't exist
  mkdir $subfolder
else
  rm -rf $subfolder/*
fi

# Check if a file exists (-f flag)
if [[ ! -f Metrics_Out.txt ]] ; then
  # Use of "!" means if the file doesn't exist, then do the following (i.e. write dummy values to a file)
  echo '  Gauge   MAE   RMSE    BIAS    AbsBIAS   NSD     NegNSD    lnNSD   NeglnNSD ' >> Metrics_Out.txt
  echo '  1       999   999     999     999       -999    999       -999    999 '      >> Metrics_Out.txt
fi

# Check if file exists and if is executable
if [[ ! -x sa_mesh ]] ; then # If sa_mesh is not found or not executable
  make -f makefile.gfortran
  # Example: sa_mesh needs to be recompiled when transferring to a new device (i.e. computer, or cluster)
  # so even if it exists from the transfer, it may not be executable
fi

# _____FOR LOOPS________________________________________________________________

# For variable values from one number to another
for trial in {001..010}
do
  subfolder="../Output/ostOutput${trial}" #Create a variable by concatenating an expression with the variable value
  mkdir $subfolder
done

# From a set number to the value of a variable
for X in $(seq 1 $numfolders);
do
  # Do something with the variable X, which is a number between 1 and "numfolders"
done

# For each instance of a regular expression (ex. list of files or folders)
for f in Scenario${Scenario}_*
do
  echo $f #This will print the folder names, ex. Scenario1_1, Scenario1_2, to the screen
done

# _____OBTAIN THE FILE PATH WHERE THIS SCRIPT IS LOCATED________________________

CurrentFilePath="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
echo $CurrentFilePath

# _____OBTAIN THE FOLDER NAME WHERE THIS SCRIPT IS LOCATED______________________

CurrentFolder=$(printf '%s\n' "${PWD##*/}")
echo $CurrentFolder

# _____SYMBOLIC LINK________________________________________________________________
ln -s <file to link to> <name of link>
ln -s ../../../Data/Processed/Driving/Scenario1/basin_humidity.csv basin_humidity.csv
  # See "man ln" for more details on these flags and others
  # Note: the use / exclusion of "." and "/" are importannt, esp. for links in Windows folders

# _____CHANGE FILE PERMISSIONS__________________________________________________

# Make a file executable
chmod +x run_mesh.sh

# _____ECHO THE DATE AND TIME___________________________________________________
###

echo "Starting run at: `date`"
