# Instructions for Forcing Dataset Processing Scripts
Instructions for use of the scripts in this folder are provided below and on the [MESH Wiki](https://wiki.usask.ca/display/MESH/Forcing+Datasets+for+MESH).

## WFD
**Script:** Interpolate_WFD_NC_Seq_SingleVAR.m  
**Instructions:**
- You need to have the drainage database of the basin or at least the header and RANK field of it.
- Please read the comments at the top of the script and within carefully. This script is prepared run from MATLAB on Windows and reads the data from \\datastore\GLOBALWATER which is mapped to drive V:\. It needs to be configured to write data as described in the header comments. It is recommended that data be written to a local HDD for speed.
- The script uses bilinear interpolation from a regular (LAT-LON) grid to another regular grid (LAT-LON).
- Processed data is saved in SEQ format.
- Snowfall and Rainfall are supplied as separate components that need be summed for use into MESH.

**Script:** Interpolate_WFD_Rain_Snow_NC_Seq.m   
**Instructions:**   
Use the above script: to interpolate the rain and snow components then write the sum to a single file for precipitation.

## WFDEI
**Script:** Read_WFDEI_SingleVAR.m  
**Instructions:**
- You need to have the drainage database of the basin or at least the header and RANK field of it
- Please read the comments at the top of the script and within carefully.
- This script is prepared run from MATLAB on Windows and reads the data from \\datastore\GLOBALWATER which is mapped to drive V:\. It needs to be configured to write data as described in the header comments. It is recommended that data be written to a local HDD for speed.
- The script assumes no interpolation is needed, i.e. basin grid has the same resolution as the data and is aligned to that of the data, i.e. it only clips the data for the basin, and formats it using the RANK field to write the space-saving SEQ format.
- This script processes only the North American version of the data that is already interpolated to 0.125-degree resolution, not the original data.

## WFDEI-GEM-CaPA
**Script:** Read_WFDEI_SingleVAR.m  
**Instructions:**   
This is the same script used to process data for a sub-basin as mentioned above for the NA version of the WFDEI data.

## WRF-CTL
**Script:** See the script multi_r2c_2_seq_SingleVAR.m and the scripts in the [WRF.zip folder](./WRF.zip)  
**Instructions:**  
To process WRF data for a particular basin, there are a few steps.

- The process is done partly on Graham using a set of shell and NCL scripts (written by Sopan Kurkute and edited by Zhenhua Li and Mohmed Elshamy) and then completed on Windows using a MATLAB Script.
- The scripts use the NCL lanague to interpolate the WRF data from its rotated-pole coordinate system to a regular LAT-LON grid using "conserve" interpolation which maintains the totals.
This is especially important when interpolating from a fine grid (WRF is 4 km) to a coarser grid (usually 10 km for MESH).
- Processing will need to be done on Graham (or other servers that you may move the data to)
Note that you will need to create a job script (SLURM) on Graham that calls the shell script and submit the job.
- Processing time depends on the size of the basin but is usually several hours. It parallelizes automatically so it would be good to ask for 8 cores - one per variable.

The scripts are included in this zip file: WRF.zip, and steps are as follows:

1. Configure and run the script **convert_to_r2c.wgt.sh** which will process a single month to get the weighting matrix for your basin. Configuration means setting some information about the dimensions and location of your basin and the location of input and outputs. Change the output and source (where the scripts reside) folders only. This script will produce a file called *"weights_file.nc"* in the output and you need to copy/move it to the source folder.
2. Configure and run the script **convert_to_r2c.CTL.sh** to your grid specifications. You can also configure which years to process and which months.  The first year goes from month 10 - 12 and has to be processed separately. These scripts produce monthly R2C files. The script uses NCL scripts to process all variables together. Files are arranged per variable in annual folders (each year in a separate folder). If you wish to process some variables, comment blocks of code for unwanted ones.
3. Collect all files from the annual folders into one folder per variable (keep the variable folder names) and transfer them a windows machine
4. Run the MATLAB script: **multi_r2c_2_seq_SingleVAR.m** that will collate all R2C files for a variable into a single SEQ file for that variable. You can parallelize the operation by invoking several instances of MATLAB and processing a variable in each instance. The script is currently configured to process the whole period from Oct 2000 - Sep 2015. There is one unit conversion made within the script for precipitation as it is originally in mm/hr and it is required to be in mm/sec (which is the same as kg/m2/sec given a density of 1000 kg/m3 for water).

## WRF-CTL-GEM-CaPA and WRF-PGW-GEM-CaPA
**Script:** Read_WRF_Cor_GEM_CaPA_SingleVAR.m  
**Instructions:**
- You need to have the drainage database of the basin or at least the header and RANK field of it
- Please read the comments at the top of the script and within carefully.
- This script is prepared run from MATLAB on Windows and reads the data from \\datastore\GLOBALWATER which is mapped to drive V:\ . You need to configure the output data folder and it is recommended that the processed data is written to a local HDD (not a network share) to improve speed and avoid cluttering the GLOBALWATER\Model_Output network share.
- The script assumes no interpolation is needed, i.e. basin grid has the same resolution as the data and is aligned to that of the data, i.e. it only clips the data for the basin, and formats it using the RANK field to write the space-saving SEQ format.

**Script:** Interpolate_WRF_CTL_Cor_GEM_CaPA_SingleVAR.m  
**Instructions:**  
- Use this script if you need to interpolate the data (i.e if the grid has a different resolution or is not aligned with the current grid)
- The script has a very similar structure to the clipping routine and will interpolate the data using bilinear interpolation.

## WRF-PGW
**Script:** See *"WRF-CTL"* above  
**Instructions:**
To process WRF data for a particular basin, there are a few steps:
- The process is done partly on Graham using a set of shell and NCL scripts (written by Sopan Kurkute and edited by Zhenhua Li and Mohmed Elshamy) and then completed on Windows using a MATLAB Script (refer to WRF_CTL, above, for the process).
- You do not need to repeat step 1 if you have done the CTL set.
- Use **convert_to_r2c.PGW.sh** in step 2.
- Adjust your forcing directory and file names in the MATLAB script in step 3/4.

## CanRCM4
**Script:** CanRCM4_NC_44_Seq_fullperiod.m  
**Instructions:**
- Use this MATLAB script to process the data for a sub-basin
  - You need to have the drainage database of the basin or at least the header and RANK field of it
- Please read the comments at the top of the script and within carefully.
- This script is prepared run from MATLAB on Windows and reads the data from \\datastore\GLOBALWATER which is mapped to drive V:\ . You need to configure the output data folder and it is recommended that the processed data is written to a local HDD (not a network share). It improves speed and avoids cluttering the GLOBALWATER\Model_Output network share.
- This script interpolates the data from the CanRCM4 rotated pole grid to a regular LAT-LON grid as defined by the shed file using linear (but sparsely spaced) interpolation.
- Processed data is saved in SEQ format.
- The above script can be used for all variables.
  - Windspeed at the surface is provided as sfcWind, but the lowest model level, meridional and zonal components need to be combined to get the resultant windspeed.

## CanRCM4-WFDEI-GEM-CaPA
**Script:** Read_BIAS_Corrected_CanRCM4_WFDEI_GEM_SingleVAR.m  
**Instructions:**
- You need to have the drainage database of the basin or at least the header and RANK field of it
- Please read the comments at the top of the script and within carefully.
- This script is prepared run from MATLAB on Windows and reads the data from \\datastore\GLOBALWATER which is mapped to drive V:\. It needs to be configured to write data as described in the header comments. It is recommended that data be written to a local HDD for speed.
- The script assumes no interpolation is needed, i.e. basin grid has the same resolution as the data and is aligned to that of the data, i.e. it only clips the data for the basin, and formats it using the RANK field to write the space-saving SEQ format.

**Script:** Interpolate_BIAS_Corrected_CanRCM4_WFDEI_GEM_SingleVAR.m  
**Instructions:**
- Use this script if you need to interpolate the data (i.e. if the grid has a different resolution or is not aligned to the current grid)  
- The script uses bilinear interpolation from a regular (LAT-LON) grid to another regular grid (LAT-LON). Processed data is saved in SEQ format.
