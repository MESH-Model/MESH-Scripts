# Control Files

## Author 
Summaworkflow_public and edited by MESH workflow to be compatible for MESH application.

## Reference
https://github.com/CH-Earth/CWARHM/tree/main/5_model_input/mizuRoute/1d_control_file

## Example
Example files from BowAtBanff subbasin have been provided. The necessary shapefiles (river_basins and river_network) are given as an example.

## Instructions
Modify each setting found after the '|' character as needed. You may create as many control files as needed for different basins. The workflow scripts will read inputs from the file named 'control_active.txt' so it is necessary to rename the desired file before use. Inactive control files should be named after their respective basins for clarity (e.g. control_Souris.txt). 

Note: for network topology, it is not necessary to declare the ID of the most downstream segment for basins with multiple outlets. Users can comment that line using the '#' character:
    #river_network_shp_outlet_id | -