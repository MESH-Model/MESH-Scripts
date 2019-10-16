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
