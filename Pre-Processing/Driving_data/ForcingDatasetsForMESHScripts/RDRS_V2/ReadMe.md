# Purpose
This shell script is written by Mohamed Elshmay in order to extarct MESH input forcing variables required for the MESH model. It has three major parts as the following:

1) Separates each variable in a file (excluding wind direction – required optionally for MMESH only – adjust the script if that’s needed)
2) Adjust Units and attributes (e.g., pressure from mb to Pa 
3) Clip and interpolate for a desired basin of study. 

# Instrunction
Users should follow some steps before submitting job. 
1) You should prepare the basin.grd for the basin of interest (see : https://wiki.usask.ca/pages/viewpage.action?pageId=1919778872). You need to rename the basin.grd file according to the basin prefixes used in the files such that calling “./clip_basin.sh basin
2) Update the “infolder” variable at line 14 to your basin folder. 
3) Required time and memory storage can be tunned reagarding the basin size. 15 hours time is sufficient for most cases. 
4) submit your job using this comment: sbatch clip_basin.sh basin