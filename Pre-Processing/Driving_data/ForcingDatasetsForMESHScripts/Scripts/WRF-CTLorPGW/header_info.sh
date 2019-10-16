#!/bin/bash
### Creating Header file Netcdf to ASCII
if [ -f header_file.txt ] ; then
rm header_file.txt
fi
echo "#########################################################################
:FileType r2c ASCII  EnSim 1.0
# National Research Council Canada (c) 1998-2014
# DataType                 2D Rect Cell
#
:Application              NCAR Command Language
:Version                  6.3.0
:WrittenBy                Sopan Kurkute - Mohamed Elshamy
:CreationDate             `date`
#
#------------------------------------------------------------------------
#
:SourceFile  WRF-ARW model 2 Dimensional Hourly Data
#
#
:Projection LatLong
:Ellipsoid GRS80
#
:xOrigin $1 
:yOrigin $2
#
#
:xCount $3
:yCount $4
:xDelta $5
:yDelta $6
#
#
:endHeader" >> header_file.txt
