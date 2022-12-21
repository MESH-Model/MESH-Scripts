# MESH_vector_based
## Welcome to the MESH vector-based project.

Extensive online documentation is available at the MESH wiki page project web pages:

Project description:
[https://wiki.usask.ca/display/MESH/Vector-based+routing+in+MESH]( https://wiki.usask.ca/display/MESH/Vector-based+routing+in+MESH)

MESH Wiki workflow:
[https://wiki.usask.ca/display/MESH/MESH+vector-based+workflow]( https://wiki.usask.ca/display/MESH/MESH+vector-based+workflow)


# Vector-based routing in MESH

## Team
  - MESH workflow development: Ala Bahrami (USASK) and Cooper Albano (USASK)  
  - MESH code support: Daniel Princz (ECCC) 
  - Geospatial support : Shervan Gharari(USASK) and Kasra Keshavarz (USASK)
  - CWARHM workflow: Wouter Knoben (USASK) 
  - Advisors: Bruce Davison (ECCC), Alain Pietroniro (UofC), and Martyn Clark (USASK) 

## Date
The project has been started in November 2020. 

## Purpose
The purpose of this repository is to provide series of code for applied testing MESH-vector-based version. The project later has been expanded to automate the implementation of the workflow for the entire North America implementation.


## Objectives and Main Tasks
  * Develop codes for setup the MESH model for any domain of interest. 
  * Adopt Community Workflows to Advance Reproducibility in Hydrologic Modeling (CWARHM : https://github.com/CH-Earth/CWARHM#community-workflows-to-advance-reproducibility-in-hydrologic-modeling-cwarhm) principles for workflow structure


## Progress / Conclusion
The preliminary stage of this project has been accomplished. The workflow has been tested for the Bow@Banff and Fraser River Basins. 


## Instructions
Detailed workflow information is available at https://wiki.usask.ca/display/MESH/MESH+vector-based+workflow 

Several tasks in this workflow must be completed sequentially while others are optional. The numbered prefix associated with each directory in the workflow indicates the order in which that directory's scripts should be executed. 
directories beginning with "0_" are optional with the exeption of "0_control_files" which contains a separate file for each domain of interest. The control file contains all settings for the subbasin selection, network topology, and drainage database 
scripts, meaning that there is no need to modify the scripts themselves. 

### 1. Preparing the control file
Currently, three scripts in the workflow accept all inputs from a control file located inside "/0_control_files/". It may be usefull to maintain several control files tailored to unique basins. To create a new control file, copy the example provided and modify each setting as needed. Inactive files should be named "control_DomainName.txt" (e.g. control_BowAtBanff). There can only be one active file at a time, and it should be named "control_active.txt". To switch between domains, rename the currently active file to "control_DomainName.txt" and rename the file representing the desired basin to "control_active.txt".

Effort should be made to make all necessary changes to the control file before running the processing scripts. If running on Graham, the default file paths should be appropriate in most cases. 
#### 1.1. Subbasin selection
Subbasin selection is an optional process and if setting up for an entire 'PFAF' basin, this script and its associated settings in the control file can be ignored. 
Use the subbasin selection script to extract a subset from given PFAF catchment and river network shapefiles. 
Within 'control_active.txt', specify the names of, and paths to, the MERIT Hydro catchment and river network files from which your subbasin of interest will be extracted. If necessary, remember to modify the default paths with a full path.
the 'target_segment' setting should be given a the 'COMID' value of the subbasin within the MERIT Hydro catchment shapefile that represents the outlet of the basin of interest.

#### 1.2. Network topology
This portion of the workflow, derived from the public section of the SUMMA workflow, uses the EASYMORE package to derive geophysical parameters, such as unique indices of stream segments and routing parameters (such as channel length, slope, and basin area, and other required information from the catchment and river shapefiles) for the domain of interest. EASYMORE creates a "network_topology" file that is then converted to create the basin information file "drainage_database" required for MESH. 
Sepcify the names and paths to the shapefiles produced in subbasin selection (if used) or to the full MERIT Hydro catchment and river network shpefiles. If the basin of interest has only one outlet, specify the 'COMID' of the MERIT Hydro subbasin containing the outlet 
in the 'river_network_shp_outlet_id' setting. Otherwise, comment out the 'river_network_shp_outlet_id' setting as it should not be included for basins with multiple outlets. The 'settings_routing_path' and settings_routing_topology' settings define the path and name of the output file.

By default, the network topology file will be saved under '/vector_based_workflow/workflow_data/domain_[Name]/topology/landsat_[domain]_stats_NA_NALCMS_2010_v2_land_cover_30m.csv'

#### 1.3. Drainage database
The drainage database requires landcover fraction information for each subbasin within the domain. This has typically been obtained from the 2010 CEC NALMCS Land Cover map. It may be necessary to execute the scripts in either '2a_gistool/' or '2b_landcover_extract/' before entering the drainage database settings to accurately specify the input zonal statistics file. See the section on zonal statistics for mor information. The GIS Tool method is preferred for producing zonal statistics because it does not require manual processing in desktop GIS software. By default, the GIS Tool-generated zonal statistics file is saved as '/vector_based_workflow/workflow_data/domain_[Name]/zonalhist/

Specify the names and paths to the zonal statistics file and the network topology file for the domain. The 'lc_type_prefix' setting refers to the naming convention of land cover class columns in the zonal statistics file. It may be necessary to review the zonal statistics file to determine this setting, but typically, the GIS Tool-generated statistics file uses the prefix 'frac_' while the QGIS file uses 'NALCMS_'. Enter the appropriate prefix in the control file.

There is a sanity check inluded in the drainage database Python script for those who have used the subbasin selection script. To employ this check, uncomment the relevant section in the Python script and provide a path to and name of the subsetted MERIT Hydro catchment file in the contol file.

By default, the drainage database file will be saved under '/vector_based_workflow/workflow_data/domain_[Name]/drainagedatabase/'



## References
Knoben, W.J.M., Clark, M.P., Bales, J., Bennett, A., Gharari, S., Marsh, C.B., Nijssen, B., Pietroniro, A., Spiteri, R.J., Tarboton, D.G., Wood, A.W., 2021. Community workflows to advance reproducibility in hydrologic modeling : separating model-agnostic and model-specific configuration steps in applications of large-domain hydrologic models. Water Resour. Res. 1–52. https://doi.org/10.1029/2021WR031753

CWARHM: https://github.com/CH-Earth/CWARHM
