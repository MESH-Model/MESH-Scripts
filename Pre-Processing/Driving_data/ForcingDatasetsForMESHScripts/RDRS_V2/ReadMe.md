# Purpose
This shell script is written by Mohamed Elshmay in order to extract MESH input forcing variables required for the MESH model. It has four major steps as the following:

1. Merge climate forcing variables over time period. If forcing variables have already been merged, skip this step and comment line 21. 
2.	Separates each variable in a file (excluding wind direction – required optionally for MMESH only – adjust the script if that’s needed)
3.	Adjust Units and attributes (e.g., pressure from mb to Pa)
4.	Clip and interpolate for a desired basin of study.
 

# Instruction
Users should follow some steps before submitting a job.

1.	You should prepare the basin.grd for the basin of interest (see : https://wiki.usask.ca/pages/viewpage.action?pageId=1919778872). You need to rename the basin.grd file according to the basin prefixes used in the files such that calling “./clip_basin.sh basin
2.	Update the “infolder” variable at line 15 to your basin folder.
3.	Required time and memory storage can be tuned regarding the basin size. Fifteen hours are sufficient for most cases.
4.	submit your job using this comment: sbatch clip_basin.sh basin

