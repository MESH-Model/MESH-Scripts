# Create network topology

## Author 
Summaworkflow_public and edited by MESH workflow to be compatible for MESH application.

## Reference 
https://github.com/CH-Earth/CWARHM/tree/main/5_model_input/mizuRoute/1b_network_topology_file

## Note 
In program we see in some section commments like # added by MESH workflow and #finished edit by MESH workflow. These are the edits applied by the MESH workflow.

## Example
Example files from BowBanff subbasin have been provided. The necessary shapefiles (river_basins and river_network) are given as an example. The output network topology file is stored at workflow_data/domain_[name]/topology/. 

## Description
Creating the network topology file is a model-specific portion of the MESH workflow. The network topology file will contain geophysical parameters such as unique indices of stream segments, routing parameters – such as channel length, slope, and basin area – and other required information from the catchment and river network shapefiles for the domain of interest. 

