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
