# Descriptions of Post-Processing Scripts

## RShinyWaterBalance

## plot_out_H.R

## ScenarioWaterBalanceCompare.R
This script creates a bar plot of the total input and output water balance components for a number of scenarios.
- Parent folder is specified, with the results files for each scenario held in sub-folders
- Scenario name is read from the name of the subfolder
- NSE values are calculated for each scenario and added to the plot
- Basin area must be specified to calculate the average streamflow as a depth  

*Potential improvements:*
- *add storage to the plots to ensure closure*
- *add a legend item for the average streamflow line*
