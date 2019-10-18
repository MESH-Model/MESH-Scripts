## CanRCM4-WFDEI-GEM-CaPA and CanRCM4-WFDEI
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
