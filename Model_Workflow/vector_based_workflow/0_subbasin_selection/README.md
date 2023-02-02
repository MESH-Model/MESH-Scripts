# EASYMORE Subbasin Selection
EASYMORE: EArth SYstem MOdeling REmapper is a collection of functions that allows extraction of the data from a NetCDF file for a given shapefile such as a basin, catchment, points or lines. It can map gridded data or model output to any given shapefile and provide area average for a target variable.
https://github.com/ShervanGharari/EASYMORE

## Programmers
Ala Bahrami
Cooper Albano

## Description
This is an optional script to extract a smaller domain of interest from within a larger domain of discretized subbasins in a shapefile format. Once the 'Subbasin Selection Settings' have been specified in the control file, users can usually run the python script or jupyter notebook to produce the subsette shapefiles. If the default settings are used, the outputs will be located in 'root_path/shapefiles'. 

###### *Note: Depending on the size of the parent shapefiles, this script could take several minutes to complete. An information line reading "EASYMORE [version] initiated" will be printed to the terminal and will remain static until the script is complete.*
