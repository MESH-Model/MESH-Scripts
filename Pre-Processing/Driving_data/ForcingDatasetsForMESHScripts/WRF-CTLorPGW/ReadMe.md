## WRF-CTL
**Script:** See those listed in the description below  
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

## WRF-PGW
**Script:** See *"WRF-CTL"* above  
**Instructions:**
To process WRF data for a particular basin, there are a few steps:
- The process is done partly on Graham using a set of shell and NCL scripts (written by Sopan Kurkute and edited by Zhenhua Li and Mohmed Elshamy) and then completed on Windows using a MATLAB Script (refer to WRF_CTL, above, for the process).
- You do not need to repeat step 1 if you have done the CTL set.
- Use **convert_to_r2c.PGW.sh** in step 2.
- Adjust your forcing directory and file names in the MATLAB script in step 3/4.
