#!/bin/bash
# GeoTIFF Processing Workflow
# Copyright (C) 2022, University of Saskatchewan
#
# This file is part of GeoTIFF Processing Workflow
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
# 1) Parts of the code are taken from https://www.shellscript.sh/tips/getopt/index.html
# 2) Parts of the code are taken from https://stackoverflow.com/a/17557904/5188208


# ================
# General comments
# ================
# 1) All variables are camelCased;


# ==============
# Help functions
# ==============
usage () {
  echo "GISTOOL: Geospatial Dataset Processing Script

Usage:
  extract-gis [options...]

Script options:
  -d, --dataset				Geospatial dataset of interest, currently
                                        available options are: 'MODIS';
                                        'MERIT-Hydro';'SoilGridsV1'
  -i, --dataset-dir=DIR			The source path of the dataset file(s)
  -r, --crs=INT				The EPSG code of interest; optional
  					[defaults to 4326]
  -v, --variable=var1[,var2[...]]	If applicable, variables to process
  -o, --output-dir=DIR			Writes processed files to DIR
  -s, --start-date=DATE			If applicable, start date of the geospatial
  					data; optional
  -e, --end-date=DATE			If applicable, end date of the geospatial
  					data; optional
  -l, --lat-lims=REAL,REAL		Latitude's upper and lower bounds; optional
  -n, --lon-lims=REAL,REAL		Longitude's upper and lower bounds; optional
  -f, --shape-file=PATH			Path to the ESRI '.shp' file; optional
  -j, --submit-job			Submit the data extraction process as a job
					on the SLURM system; optional
  -t, --print-geotiff=BOOL		Extract the subsetted GeoTIFF file; optional
  					[defaults to 'true']
  -a, --stat=stat1[,stat2[...]]		If applicable, extract the statistics of
  					interest, currently available options are:
					'min';'max';'mean';'majority';'minority';
					'median';'quantile';'variety';'variance';
					'stdev';'coefficient_of_variation';'frac';
					'coords'; optional
  -q, --quantile=q1[,q2[...]]		Quantiles of interest to be produced if 'quantile'
  					is included in the '--stat' argument. The values
					must be comma delimited float numbers between
					0 and 1; optional [defaults to every 5th quantile]
  -p, --prefix=STR			Prefix  prepended to the output files
  -c, --cache=DIR			Path of the cache directory; optional
  -E, --email=STR			E-mail when job starts, ends, and 
  					finishes; optional
  -V, --version				Show version
  -h, --help				Show this screen and exit

For bug reports, questions, discussions open an issue
at https://github.com/kasra-keshavarz/gistool/issues" >&1;

  exit 0;
}

short_usage () {
  echo "usage: $(basename $0) -d DATASET -io DIR -v var1[,var2,[...]] [-jVhE] [-t BOOL] [-c DIR] [-se DATE] [-r INT] [-ln REAL,REAL] [-f PATH} [-p STR] [-a stat1[,stat2,[...]] [-q q1[,q2[...]]]] " >&1;
}

version () {
  echo "$(basename $0): version $(cat $(dirname $0)/VERSION)";
  exit 0;
}


# =====================
# Necessary Assumptions
# =====================
# TZ to be set to UTC to avoid invalid dates due to Daylight Saving
alias date='TZ=UTC date'

# expand aliases for the one stated above
shopt -s expand_aliases


# =======================
# Parsing input arguments
# =======================
# argument parsing using getopt - WORKS ONLY ON LINUX BY DEFAULT
parsedArguments=$(getopt -a -n extract-geotiff -o d:i:r:v:o:s:e:l:n:f:jt:a:q:p:c:EVh --long dataset:,dataset-dir:,crs:,variable:,output-dir:,start-date:,end-date:,lat-lims:,lon-lims:,shape-file:,submit-job,print-geotiff:,stat:,quantile:,prefix:,cache:,email:,version,help -- "$@")
validArguments=$?
# check if there is no valid options
if [ "$validArguments" != "0" ]; then
  short_usage;
  exit 1;
fi

# check if no options were passed
if [ $# -eq 0 ]; then
  short_usage;
  exit 1;
fi

# check long and short options passed
eval set -- "$parsedArguments"
while :
do
  case "$1" in
    -d | --dataset)       geotiff="$2"         ; shift 2 ;; # required
    -i | --dataset-dir)   geotiffDir="$2"      ; shift 2 ;; # required
    -r | --crs)		  crs="$2"	       ; shift 2 ;; # optional
    -v | --variable)	  variables="$2"       ; shift 2 ;; # required
    -o | --output-dir)    outputDir="$2"       ; shift 2 ;; # required
    -s | --start-date)    startDate="$2"       ; shift 2 ;; # required
    -e | --end-date)      endDate="$2"         ; shift 2 ;; # optional
    -l | --lat-lims)      latLims="$2"         ; shift 2 ;; # optional
    -n | --lon-lims)      lonLims="$2"         ; shift 2 ;; # optional
    -f | --shape-file)	  shapefile="$2"       ; shift 2 ;; # optional
    -j | --submit-job)    jobSubmission=true   ; shift   ;; # optional
    -t | --print-geotiff) printGeotiff="$2"    ; shift 2 ;; # optional
    -a | --stat)	  stats="$2"	       ; shift 2 ;; # optional
    -q | --quantile)	  quantiles="$2"       ; shift 2 ;; # optional
    -p | --prefix)	  prefixStr="$2"       ; shift 2 ;; # required
    -c | --cache)	  cache="$2"	       ; shift 2 ;; # optional
    -E | --email)	  email="$2"	       ; shift 2 ;; # optional
    -V | --version)	  version	       ; shift   ;; # optional 
    -h | --help)          usage                ; shift   ;; # optional

    # -- means the end of the arguments; drop this, and break out of the while loop
    --) shift; break ;;

    # in case of invalid option
    *) echo "$(basename $0): invalid option '$1'" >$2;
       short_usage;
       exit 1;;
  esac
done

# check mandatory arguments whether provided
if [[ -z "${geotiffDir}" ]] || \
   [[ -z "${geotiff}"    ]] || \
   [[ -z "${variables}"  ]] || \
   [[ -z "${outputDir}"  ]] || \
   [[ -z "${prefixStr}"  ]]; then

   echo "$(basename $0): mandatory option(s) missing.";
   short_usage;
   exit 1;
fi

# if $printGeotiff is not triggered
if [[ -z $printGeotiff ]]; then
  printGeotiff=true
fi

# check the value of $printGeotiff
if [[ -n $printGeotiff ]]; then
  case "${printGeotiff,,}" in
    "true" | "1" )
      printGeotiff="true"
      ;;

    "false" | "0" )
      printGeotiff="false"
      ;;

    *)
      echo "$(basename $0): invalid value for '--print-geotiff', continuing with default value of 'true'"
      ;;
  esac
fi

# default value for cache path if not provided as an argument
if [[ -z $cache ]] && [[ -n $jobSubmission ]]; then
  cache="$HOME/scratch/.temp_data_jobid"
elif [[ -z $cache ]]; then
  cache="$HOME/scratch/.temp_data_$(date +"%N")"
fi

# email withought job submission not allowed
if [[ -n $email ]] && [[ -z $jobSubmission ]]; then
  echo "$(basename $0): Email is not supported wihtout job submission;"
  echo "$(basename $0): Continuing without email notification..."
fi

# either shapefile or spatial extents arguments are allowed
if [[ -n $shapefile ]] && [[ -n $latLims ]]; then
  echo "$(basename $0): ERROR! Either shapefile or spatial extents should be entered"
  exit 1;
elif [[ -n $shapefile ]] && [[ -n $lonLims ]]; then
  echo "$(basename $0): ERROR! Either shapefile or spatial extents should be entered"
  exit 1;
fi

# if no crs is entered, assign the default value of EPSG 4326
if [[ -z $crs ]]; then
  crs=4326
fi

# at least $printGeotiff=true or $stats=stat1[,stat2[...]] must be provided
if [[ "${printGeotiff,,}" == "false" ]] && [[ -z $stats ]]; then
  echo "$(basename $0): ERROR! At minimum, either of '--print-geotiff' or '--stats' must be provided"
  exit 1;
fi

# if quantile is not given in '--stat' but '--quantile' is provided
if [[ "$stats" != *"quantile"* ]] && [[ -n $quantiles ]]; then
  echo "$(basename $0): ERROR! 'quantile' stat is not provided in '--stat' while '--quantile' argument is filled"
  exit 1;
fi

# if quantile is given in '--stat' but '--quantile' is not provided
if [[ "$stats" == *"quantile"* ]] && [[ -z $quantiles ]]; then
  echo "$(basename $0): Warning! 'quantile' stat is provided in '--stat' while '--quantile' is not filled;"
  echo "$(basename $0): Continuing with default values of 25th, 50th, and 75th quantiles"
  quantiles="0.25,0.50,0.75"
fi


# ======================
# Necessary Preparations
# ======================
# put necessary arguments in an array - just for legibility
declare -A funcArgs=([geotiffDir]="$geotiffDir" \
		     [crs]="$crs" \
		     [variables]="$variables" \
		     [outputDir]="$outputDir" \
		     [startDate]="$startDate" \
		     [endDate]="$endDate" \
		     [latLims]="$latLims" \
		     [lonLims]="$lonLims" \
		     [shapefile]="$shapefile" \
		     [jobSubmission]="$jobSubmission" \
		     [printGeotiff]="$printGeotiff" \
		     [stats]="$stats" \
		     [quantiles]="$quantiles" \
		     [prefixStr]="$prefixStr" \
		     [cache]="$cache" \
		    );


# =================================
# Template data processing function
# =================================
call_processing_func () {

  local script="$1" # script local path
  local chunkTStep="$2" # chunking time-frame periods

  local scriptName=$(echo $script | cut -d '/' -f 2) # script/geotiff name

  # prepare a script in string format
  # all processing script files must follow same input argument standard
  local scriptRun
  read -rd '' scriptRun <<- EOF
	bash ${script} --dataset-dir="${funcArgs[geotiffDir]}" --crs="${funcArgs[crs]}" --variable="${funcArgs[variables]}" --output-dir="${funcArgs[outputDir]}" --start-date="${funcArgs[startDate]}" --end-date="${funcArgs[endDate]}" --lat-lims="${funcArgs[latLims]}" --lon-lims="${funcArgs[lonLims]}" --shape-file="${funcArgs[shapefile]}" --print-geotiff="${funcArgs[printGeotiff]}" --stat="${funcArgs[stats]}" --quantile="${funcArgs[quantiles]}" --prefix="${funcArgs[prefixStr]}" --cache="${funcArgs[cache]}"
	EOF

  # evaluate the script file using the arguments provided
  if [[ "${funcArgs[jobSubmission]}" == true ]]; then
    # Create a temporary directory for keeping job logs
    mkdir -p "$HOME/scratch/.gdt_logs"
    # SLURM batch file
    sbatch <<- EOF
	#!/bin/bash
	#SBATCH --cpus-per-task=4
	#SBATCH --nodes=1
	#SBATCH --account=rpp-kshook
	#SBATCH --time=04:00:00
	#SBATCH --mem=16GB
	#SBATCH --job-name=GWF_${scriptName}
	#SBATCH --error=$HOME/scratch/.gdt_logs/GWF_%j_err.txt
	#SBATCH --output=$HOME/scratch/.gdt_logs/GWF_%j.txt
	#SBATCH --mail-user=$email
	#SBATCH --mail-type=BEGIN,END,FAIL

	srun ${scriptRun} --cache="${cache}-\${SLURM_JOB_ID}" 
	EOF
    # echo message
    echo "$(basename $0): job submission details are printed under ${HOME}/scratch/.gdt_logs"
 
  else
    eval "$scriptRun"
  fi
}


# ======================
# Checking Input GeoTIFF 
# ======================

case "${geotiff,,}" in
  # MERIT Hydro
  "merithydro" | "merit hydro" | "merit-hydro" | "merit_hydro")
    call_processing_func "$(dirname $0)/merit_hydro/merit_hydro.sh"
    ;;

  # Soil Grids v1
  "soil-grids-v1" | "soilgridsv1" | "soil_grids_v1" | "soil grids v1")
    call_processing_func "$(dirname $0)/soil_grids/soil_grids_v1.sh"
    ;;

  # MODIS
  "modis")
    call_processing_func "$(dirname $0)/modis/modis.sh"
    ;;

  # GSDE
  "gsde")
    call_processing_func "$(dirname $0)/gsde/gsde.sh"
    ;;
  
  # Depth to Bedrock
  "depth-to-bedrock" | "depth_to_bedrock" | "dtb" | "depth-to_bedrock" | "depth_to_bedrock")
    call_processing_func "$(dirname $0)/depth_to_bedrock/depth_to_bedrock.sh"
    ;;
  
  # Landsat
  "landsat" | "landast" )
    call_processing_func "$(dirname $0)/landsat/landsat.sh"
    ;;

  # dataset not included above
  *)
    echo "$(basename $0): missing/unknown dataset";
    exit 1;;
esac

