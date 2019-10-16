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
