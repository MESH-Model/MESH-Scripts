# Descriptions of Post-Processing Scripts

## RShinyHydrographCompare
This folder contains the "server.R" and "ui.R" scripts to generate a RShiny Web App to compare the measured and simulated streamflow hydrographs for one or more stations.

User must specify the path to the file within both scripts, and may then select different stations to examine on the interactive app.

*Potential improvements:*
- *add storage to the plots to ensure closure*
- *add a legend item for the average streamflow line*

## RShinyWaterBalance
This folder contains the "server.R" and "ui.R" scripts to generate a RShiny Web App to examine the model output components of the water balance from the "basin_average_water_balance.csv" file.

User specifies the folder path in the "ui.R" file.

## RShinyWaterBalance_Scenarios
This folder contains the "server.R" and "ui.R" scripts to generate a RShiny Web App to examine the model output components of the water balance from the "basin_average_water_balance.csv" files for a number of scenarios.

User specifies the folder path containing subfolders of the outputs for each scenario in the "ui.R" and "WBgenerator.R" files.

In order to run the app, first the "WBgenerator.R" script must be run to save a "plots.csv" file to the parent folder.

The app then allows for interactive investigation of output water balance variables for different scenarios.

## ScenarioWaterBalanceCompare.R
This script creates a bar plot of the total input and output water balance components for a number of scenarios.
- Parent folder is specified, with the results files for each scenario held in sub-folders
- Scenario name is read from the name of the subfolder
- NSE values are calculated for each scenario and added to the plot
- Basin area must be specified to calculate the average streamflow as a depth  
