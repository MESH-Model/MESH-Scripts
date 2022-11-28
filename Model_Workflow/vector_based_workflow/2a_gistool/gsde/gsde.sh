#!/bin/bash
# GIS Data Processing Workflow
# Copyright (C) 2022, University of Saskatchewan
# Copyright (C) 2021, Wouter Knoben
#
# This file is part of GIS Data Processing Workflow
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# =========================
# Credits and contributions
# =========================
# 1. Parts of the code are taken from https://www.shellscript.sh/tips/getopt/index.html
# 2. General ideas of GeoTIFF subsetting are taken from https://github.com/CH-Earth/CWARHM
#    developed mainly by Wouter Knoben (hence the header copyright credit). See the preprint
#    at: https://www.essoar.org/doi/10.1002/essoar.10509195.1


# ================
# General comments
# ================
# * All variables are camelCased for distinguishing from function names;
# * function names are all in lower_case with words seperated by underscore for legibility;
# * shell style is based on Google Open Source Projects'
#   Style Guide: https://google.github.io/styleguide/shellguide.html


# ===============
# Usage Functions
# ===============
short_usage() {
  echo "usage: $(basename $0) -cio DIR -v var1[,var2[...]] [-r INT] [-se DATE] [-ln REAL,REAL] [-f PATH] [-t BOOL] [-a stat1[,stat2,[...]] [-q q1[,q2[...]]]] [-p STR] "
}


# argument parsing using getopt - WORKS ONLY ON LINUX BY DEFAULT
parsedArguments=$(getopt -a -n gsde -o i:o:v:r:s:e:l:n:f:t:a:q:p:c: --long dataset-dir:,output-dir:,variable:,crs:,start-date:,end-date:,lat-lims:,lon-lims:,shape-file:,print-geotiff:,stat:,quantile:,prefix:,cache: -- "$@")
validArguments=$?
if [ "$validArguments" != "0" ]; then
  short_usage;
  exit 1;
fi

# check if no options were passed
if [ $# -eq 0 ]; then
  echo "$(basename $0): ERROR! arguments missing";
  exit 1;
fi

# check long and short options passed
eval set -- "$parsedArguments"
while :
do
  case "$1" in
    -i | --dataset-dir)   geotiffDir="$2"      ; shift 2 ;; # required
    -o | --output-dir)    outputDir="$2"       ; shift 2 ;; # required
    -v | --variable)      variables="$2"       ; shift 2 ;; # required
    -r | --crs)		  crs="$2"	       ; shift 2 ;; # required 
    -s | --start-date)    startDate="$2"       ; shift 2 ;; # redundant - added for compatibility
    -e | --end-date)      endDate="$2"         ; shift 2 ;; # redundant - added for compatibility
    -l | --lat-lims)      latLims="$2"         ; shift 2 ;; # required - could be redundant
    -n | --lon-lims)      lonLims="$2"         ; shift 2 ;; # required - could be redundant
    -f | --shape-file)    shapefile="$2"       ; shift 2 ;; # required - could be redundant
    -t | --print-geotiff) printGeotiff="$2"    ; shift 2 ;; # required
    -a | --stat)	  stats="$2"	       ; shift 2 ;; # optional
    -q | --quantile)	  quantiles="$2"       ; shift 2 ;; # optional
    -p | --prefix)	  prefix="$2"          ; shift 2 ;; # optional
    -c | --cache)	  cache="$2"           ; shift 2 ;; # required

    # -- means the end of the arguments; drop this, and break out of the while loop
    --) shift; break ;;

    # in case of invalid option
    *)
      echo "$(basename $0): ERROR! invalid option '$1'";
      short_usage; exit 1 ;;
  esac
done

# check if $ensemble is provided
if [[ -n "$startDate" ]] || [[ -n "$endDate" ]]; then
  echo "$(basename $0): ERROR! redundant argument (time extents) provided";
  exit 1;
fi

# check the prefix if not set
if [[ -z $prefix ]]; then
  prefix="gsde_"
fi

# parse comma-delimited variables
IFS=',' read -ra variables <<< "${variables}"


# =====================
# Necessary Assumptions
# =====================
# TZ to be set to UTC to avoid invalid dates due to Daylight Saving
alias date='TZ=UTC date'
# expand aliases for the one stated above
shopt -s expand_aliases
# hard-coded paths
renvCache="/project/rpp-kshook/Climate_Forcing_Data/assets/r-envs/" # general renv cache path
exactextractrCache="${renvCache}/exact-extract-env" # exactextractr renv cache path
renvPackagePath="${renvCache}/renv_0.16.0.tar.gz" # renv_0.16.0 source path
# some variables
latVar="lat"
lonVar="lon"


# ==========================
# Necessary Global Variables
# ==========================
# the structure of the original file names are one of the following:
#     * %{var}_M_sl%{soil_layer_depth_number}_250m_ll.tif
#     * %{var}_250m_ll.tif
# but the user is expected to include complete variable (file) name in the 
# `--variable` input argument comma-separated list.


# ===================
# Necessary Functions
# ===================
# Modules below available on Compute Canada (CC) Graham Cluster Server
load_core_modules () {
    module -q purge
    module -q load gcc/9.3.0
    module -q load r/4.1.2
    module -q load gdal/3.0.4
    module -q load udunits/2.2.28
    module -q load geos/3.10.2
    module -q load proj/9.0.0
    module -q load nco/5.0.6
    module -q load netcdf/4.7.4
}
load_core_modules


# =================
# Useful One-liners
# =================
# sorting a comma-delimited string of real numbers
sort_comma_delimited () { IFS=',' read -ra arr <<< "$*"; echo ${arr[*]} | tr " " "\n" | sort -n | tr "\n" " "; }

# log date format
logDate () { echo "($(date +"%Y-%m-%d %H:%M:%S")) "; }


#######################################
# subset GeoTIFFs
#
# Globals:
#   latLims: comma-delimited latitude
#            limits
#   lonLims: comma-delimited longitude
#            limits
#
# Arguments:
#   sourceNc: source netcdf file
#   destPath: destionation path (inclu-
#	      ding file name)
#
# Outputs:
#   one mosaiced (merged) GeoTIFF under
#   the $destDir
#######################################
subset_geotiff () {
  # local variables
  local latMin
  local latMax
  local lonMin
  local lonMax
  local sortedLats
  local sortedLons
  # reading arguments
  local sourceNc="$1"
  local destPath="$2"

  # extracting minimum and maximum of latitude and longitude respectively
  ## latitude
  sortedLats=($(sort_comma_delimited "$latLims"))
  latMin="${sortedLats[0]}"
  latMax="${sortedLats[1]}"
  ## longitude
  sortedLons=($(sort_comma_delimited "$lonLims"))
  lonMin="${sortedLons[0]}"
  lonMax="${sortedLons[1]}"

  # subset netCDF file
  ncks -d $latVar,$latMin,$latMax -d $lonVar,$lonMin,$lonMax "${sourceNc}" "${destPath}"
}


# ===============
# Data Processing
# ===============
# display info
echo "$(logDate)$(basename $0): processing Soil-Grids-v1 GeoTIFF(s)..."

# make the output directory
echo "$(logDate)$(basename $0): creating output directory under $outputDir"
mkdir -p "$outputDir" # making the output directory

# if shapefile is provided extract the extents from it
if [[ -n $shapefile ]]; then
  # extract the shapefile extent
  IFS=' ' read -ra shapefileExtents <<< "$(ogrinfo -so -al "$shapefile" | sed 's/[),(]//g' | grep Extent)"
  # transform the extents in case they are not in EPSG:4326
  IFS=':' read -ra sourceProj4 <<< "$(gdalsrsinfo $shapefile | grep -e "PROJ.4")" # source Proj4 value
  if [[ -n $sourceProj4 ]]; then
    :
  else
    echo "$(logDate)$(basename $0): WARNING! Assuming WSG84 CRS for the input ESRI shapefile"
    sourceProj4=("PROJ.4" " +proj=longlat +datum=WGS84 +no_defs") # made an array for compatibility with the following statements
  fi
  
  # transform limits and assing to variables
  IFS=' ' read -ra leftBottomLims <<< $(echo "${shapefileExtents[@]:1:2}" | gdaltransform -s_srs "${sourceProj4[1]}" -t_srs EPSG:4326 -output_xy)
  IFS=' ' read -ra rightTopLims <<< $(echo "${shapefileExtents[@]:4:5}" | gdaltransform -s_srs "${sourceProj4[1]}" -t_srs EPSG:4326 -output_xy)
  # define $latLims and $lonLims from $shapefileExtents
  lonLims="${leftBottomLims[0]},${rightTopLims[0]}"
  latLims="${leftBottomLims[1]},${rightTopLims[1]}"
fi

# modifying variable names to conform to that of the dataset's standard
variablesMod=();
for var in "${variables[@]}"; do
  for name in "$var"{1..2}; do
    variablesMod+=("${name}")
  done
done

# unzip files
for var in "${variablesMod[@]}"; do
  unzip "${geotiffDir}/${var}" -d "$cache"
done

# subset and produce stats if needed
if [[ "$printGeotiff" == "true" ]]; then
  echo "$(logDate)$(basename $0): subsetting GeoTIFFs under $outputDir"
  for var in "${variablesMod[@]}"; do
    # subset based on lat and lon values
    subset_geotiff "${cache}/${var}.nc" "${outputDir}/${prefix}${var}.nc"
  done
fi

## make R renv project directory
if [[ -n "$shapefile" ]] && [[ -n $stats ]]; then
  echo "$(logDate)$(basename $0): Extracting stats under $outputDir"
  mkdir -p "$cache/r-virtual-env/"
  ## make R renv in $cache
  virtualEnvPath="$cache/r-virtual-env/"
  cp "$(dirname $0)/../assets/renv.lock" "$virtualEnvPath"
  ## load necessary modules - excessive, mainly for clarification
  load_core_modules

  ## make the temporary directory for installing r packages
  tempInstallPath="$cache/r-packages"
  mkdir -p "$tempInstallPath"
  export R_LIBS_USER="$tempInstallPath"
  
  # extract given stats for each variable
  for var in "${variablesMod[@]}"; do
    ## build renv and create stats
    Rscript "$(dirname $0)/../assets/stats.R" \
    	    "$tempInstallPath" \
  	    "$exactextractrCache" \
  	    "$renvPackagePath" \
	    "$virtualEnvPath" \
	    "$virtualEnvPath" \
	    "${virtualEnvPath}/renv.lock" \
	    "${cache}/${var}.nc" \
	    "$shapefile" \
	    "$outputDir/${prefix}stats_${var}.csv" \
	    "$stats" \
	    "$quantiles" >> "${outputDir}/${prefix}stats_${var}.log" 2>&1;
  done
fi

# remove unnecessary files 
mkdir "$HOME/empty_dir" 
echo "$(logDate)$(basename $0): deleting temporary files from $cache"
rsync --quiet -aP --delete "$HOME/empty_dir/" "$cache"
rm -r "$cache"
echo "$(logDate)$(basename $0): temporary files from $cache are removed"
echo "$(logDate)$(basename $0): results are produced under $outputDir"

