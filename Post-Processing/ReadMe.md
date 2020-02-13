# Descriptions of Post-Processing Scripts

## RShinyHydrographCompare
This folder contains the "server.R" and "ui.R" scripts to generate a RShiny Web App to compare the measured and simulated streamflow hydrographs for one or more stations.

User must specify the path to the file within both scripts, and may then select different stations to examine on the interactive app.

## RShinyWaterBalance
This folder contains the "server.R" and "ui.R" scripts to generate a RShiny Web App to examine the model output components of the water balance from the "basin_average_water_balance.csv" file.

User specifies the folder path in the "ui.R" file.

## ScenarioWaterBalanceCompare.R
This script creates a bar plot of the total input and output water balance components for a number of scenarios.
- Parent folder is specified, with the results files for each scenario held in sub-folders
- Scenario name is read from the name of the subfolder
- NSE values are calculated for each scenario and added to the plot
- Basin area must be specified to calculate the average streamflow as a depth  

*Potential improvements:*
- *add storage to the plots to ensure closure*
- *add a legend item for the average streamflow line*
