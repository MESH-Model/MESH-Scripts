#!/bin/sh
#
# This script generates source code for runsvs from SPS/SVS source code
#
# Location of SVS source code

SVSFULL=.

if [ "$1" == "" ]
then
	SVSDIR=$SVSFULL/svs_v1.1.3_cleanup
else
	SVSDIR=$SVSFULL/$1
fi
# Location of the runsvs source code
if [ "$2" == "" ]
then
        RUNSVSDIR=$SVSFULL/src
else
        RUNSVSDIR=$SVSFULL/$2
fi
# Location of the working directory containing the scripts and list of bus variables
if [ "$3" == "" ]
then
	WORKDIR=$SVSFULL/bin
else
	WORKDIR=$SVSFULL/$3
fi
#
# Start by parsing SVS source code to identify variables from the physics and dynamics bus that are used
$WORKDIR/create_list_of_bus_variables_used_by_svs.sh $SVSDIR $RUNSVSDIR $WORKDIR
# This should have created a file called list_of_bus_variables_used_by_svs.txt
#
# We combine this list with a list of additional variables we wish to define
cat $WORKDIR/list_of_bus_variables_svs.txt $WORKDIR/list_of_additional_bus_variables.txt | sort -u > $WORKDIR/list_of_bus_variables.txt
#
# Create the runsvs_mod module from list_of_bus_variables.txt
# This replaces the SPS memory manager
$WORKDIR/create_runsvs_mod.sh $WORKDIR/list_of_bus_variables.txt $RUNSVSDIR
#
# We finally need to parse the code of phydebu.ftn90 to extract the BLOCK DATA statements that are used to initialize physics options
#$WORKDIR/create_phy_debu_data.sh $SVSDIR
