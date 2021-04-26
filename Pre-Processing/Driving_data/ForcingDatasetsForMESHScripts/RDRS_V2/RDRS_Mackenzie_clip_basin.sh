#!/bin/bash
#SBATCH --account=rpp-kshook
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=40G
#SBATCH --time=15:00:00
#SBATCH --job-name=Ext_RDRS_MRB
module load StdEnv/2020  intel/2020.1.217  openmpi/4.0.3
module load cdo/1.9.8

# basin prefix - to be supplied as an argument - grd file should be named accordingly
basin=$1

# Change the folder to your basin
infolder=/project/6008034/Model_Output/105_RDRS_v2_10km/01_Macknezie

outfile=$basin'_RDRS_v2_2000-2017'
echo $outfile

# merge along the time axis - results in a large file
# merging was already done for all basins
# cdo -v -z zip mergetime $infolder/*.nc $outfile.nc

# Exclude non-essential variables
#vars='RDRS_v2_P_HR_09944,RDRS_v2_P_PR0_SFC,RDRS_v2_P_TT_1.5m,RDRS_v2_P_UU_09944,RDRS_v2_P_UU_10m,RDRS_v2_P_UUC_10m,RDRS_v2_P_UVC_10m,RDRS_v2_P_VVC_10m,RDRS_v2_P_UUC_09944,RDRS_v2_P_VVC_09944,RDRS_v2_P_WDC_10m'
#cdo -v -z zip delname,$vars $outfile.nc $outfile'_MESH.nc'

#seperate variables
for var in RDRS_v2_P_HU_09944 RDRS_v2_A_PR0_SFC RDRS_v2_P_P0_SFC RDRS_v2_P_FB_SFC RDRS_v2_P_FI_SFC RDRS_v2_P_TT_09944 RDRS_v2_P_UVC_09944
	do
		cdo selname,$var $outfile'_MESH'.nc $basin"_"$var"_2000-2017".nc		
	done
	
# Adjust units
# Pressure from "mb" to "Pa"
cdo mulc,100 $basin'_RDRS_v2_P_P0_SFC_2000-2017'.nc tmp.nc
cdo setattribute,RDRS_v2_P_P0_SFC@units=Pa tmp.nc $basin'_RDRS_v2_P_P0_SFC_2000-2017'.nc
rm tmp.nc

# Temperature from "deg_C" to "K"
cdo addc,273.16 $basin'_RDRS_v2_P_TT_09944_2000-2017'.nc tmp.nc
cdo setattribute,RDRS_v2_P_TT_09944@units=K tmp.nc $basin'_RDRS_v2_P_TT_09944_2000-2017'.nc
rm tmp.nc

# Wind speed from "knts" to "m/s"
cdo mulc,0.5144444444444444 $basin'_RDRS_v2_P_UVC_09944_2000-2017'.nc tmp.nc
cdo setattribute,RDRS_v2_P_UVC_09944@units="m s-1" tmp.nc $basin'_RDRS_v2_P_UVC_09944_2000-2017'.nc
rm tmp.nc

# Precipitation from "m" over the hour to a rate "mm/s" = "kg m-2 s-1"
cdo divc,3.6 $basin'_RDRS_v2_A_PR0_SFC_2000-2017'.nc tmp.nc
cdo setattribute,RDRS_v2_A_PR0_SFC@units="mm s-1" tmp.nc $basin'_RDRS_v2_A_PR0_SFC_2000-2017'.nc
rm tmp.nc

# Clip and Interpplate for the basin grid
for var in RDRS_v2_P_HU_09944 RDRS_v2_A_PR0_SFC RDRS_v2_P_P0_SFC RDRS_v2_P_FB_SFC RDRS_v2_P_FI_SFC RDRS_v2_P_TT_09944 RDRS_v2_P_UVC_09944
	do
	cdo -z zip -b F32 remapbil,$basin.grd $basin"_"$var"_2000-2017".nc $basin"_"$var"_2000-2017_MESH".nc
	done
