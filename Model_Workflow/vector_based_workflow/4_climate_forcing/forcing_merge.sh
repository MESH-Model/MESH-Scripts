#!/bin/bash
#SBATCH --account=rpp-kshook
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=60G
#SBATCH --time=01:00:00
#SBATCH --job-name=RDRS_forcing_merge
#SBATCH --error=errors_RDRS_forcing_merge
#SBATCH --mail-user=example.email@usask.ca
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END

# load modules and combine forcing NetCDF
module load cdo/1.9.8

# cd to datatool.sh output directory
cd ../../forcing/1980

# merge into monthly files
#for i in 01 02 03 04 05 06 07 08 09 10 11 12
#do
#cdo -z zip -b F32 mergetime rdrsv2.1_1980$i*.nc rdrsv2.1_19080_*i.nc
#done

# merge into yearly files
cdo -z zip -b F32 mergetime rdrsv2.1_1980*.nc rdrsv2.1_1980.nc