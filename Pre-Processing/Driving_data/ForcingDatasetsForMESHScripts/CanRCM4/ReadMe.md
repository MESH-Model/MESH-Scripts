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
