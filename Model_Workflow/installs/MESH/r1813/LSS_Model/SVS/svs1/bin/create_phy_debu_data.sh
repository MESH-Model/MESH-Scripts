#!/bin/bash
# create_phy_debu_data.sh
# This script creates phy_debu_data.ftn90 from phydebu.ftn90

if [ "$1" == "" ]
then
        SVSDIR=svs_v1.1.3_cleanup
else
        SVSDIR=$1
fi
CHANGES2PHYDIR=changes_to_rpnphy
PHYDEBU=$SVSDIR/phydebu.ftn90
PHY_DEBU_DATA=$CHANGES2PHYDIR/phy_debu_data.ftn90

# Extract common initialization block from phydebu source code
# ------------------------------------------------------------

grep "BLOCK DATA PHY_DEBU_DATA" -A1000 $PHYDEBU > $PHY_DEBU_DATA

