# Control Files

## Author 
Summaworkflow_public and edited by MESH workflow to be compatible for MESH application.

## Reference
https://github.com/CH-Earth/CWARHM/tree/main/5_model_input/mizuRoute/1d_control_file

## Example
Example files from BowAtBanff subbasin have been provided. The necessary shapefiles (river_basins and river_network) are given as an example.

## Additional Documentation
Please visit the Standalone MESH Wiki for more detailed descriptions of each setting.
https://wiki.usask.ca/display/MESH/MESH+vector-based+workflow

## Instructions
Modify each setting found after the '|' character as needed. Any text to the left of the '|' character represents the name of the setting. Do not modify the setting names as this may prevent the workflow scripts from finding the setting.

You may create as many control files as needed for different basins. The workflow scripts will read inputs from the file named 'control_active.txt'. Once you are certain that all settings have been specified correctly, save the control file for the domain of interest and navigate to the directory '1_folder_prep'. Running the script 'make_folder_structure.py' or the Jupyter Notebook of the same name will copy the contents of the domain of interest into 'control_active.txt'. 

Refer to ../1_folder_prep/README.md for instructions on preparing the active control file.
 

Note: for network topology, it is not necessary to declare the ID of the most downstream segment for basins with multiple outlets. Users can comment that line using the '#' character:
    #river_network_shp_outlet_id | -