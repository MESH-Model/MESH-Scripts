# Climate Forcing Instructions

## Dataset
RDRSv2.1

## Description
This directory contains MESH-specific and model-agnostic components. RDRSv2.1 data extraction and remapping are model-agnostic processes. Modification of units and reordering of data are MESH-specific tasks.

Begin by executing the '1_datatool.sh' script to extract the desired variables from the RDRSv2.1 dataset on graham. This will extract daily RDRSv2.1 forcing data containing the specified variables.

Optionally, you may run the 'forcing_merge.sh' script to merge data into monthly or yearly files.

Use '2_easymore_remapping.py' to remap the RDRSv2.1 data to the desired basin. Ensure that the target shapefile is the same file used to produce network topology.

MESH requires forcing data to be presented in specific units. Use '3_Climateforcing_change_unity.sh' to modify the units of the remapped file to conform to MESH requirements.

Additionally, MESH requires forcing to be ordered based on Rank. '4_MESH_vectorbased_forcing.py' will reorder the remapped file to conform to MESH requirements.