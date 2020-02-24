#__________________________________________________________________________________________________________

# This script is used to populate the MESH_parameters_CLASS.ini/.tpl and MESH_parameters_hydrology.ini/.tpl (.tpl files for calibration with Ostrich) from a .csv file containing a list of all the parameter values. 

# Requirements:
#   - MESH_parameters_CLASS.txt and MESH_parameters_hydrology.txt files containing the parameter codes (ex._FCAN-NL1_)
#     for all the parameters for the number of GRUs desired
#         NOTE: the default .txt files included with this script assume 3 soil layers, MID=1, and 10 GRUs; delete/add           soil layers or GRUs as necessary and specify the start date and time of the meteorological data.

#   - ParamValues.csv file with the following columns; columns 1, 2, and 4 must remain, but the remaining columns are for refernece information only and can be modified by the user.
#       1) Parameter code (which corresponds to the code used in the .txt file)
#       2) Parameter Value
#       3) GRU Number
#       4) Calibration (containing TRUE or FALSE; only used if the "Calibration" variable below is set to "TRUE")
#       5) Description (which can be used to describe the parameter, or justification for the value selected)

#__________________________________________________________________________________________________________

##### SET VALUES IN THIS SECTION #####

# Modify the .txt files according to the number of GRUs used, initial values, and met. data start time, and update the ParamValues.csv file with the parameter values (initial or static)

#Set the working directory to the location of the files 
#setwd('C:/myfolder')


Calibration <- TRUE 
# TRUE for calibrating the model to only replace non-calibrated parameter values (where column 3=FALSE) and also generate .tpl files
# FALSE to replace all parameters with the values and only generate .ini files

Frozen <- TRUE
# True to use the Frozen Soil infiltration algorithm (more details on the MESH wiki)
# False to use the Class infiltration algorithm

# Set the file names of the parameter files
ParamFile = 'ParamValues.csv'
ClassTemplate = 'MESH_parameters_CLASS_template.txt'
HydroTemplate = ifelse(Frozen==TRUE,"MESH_parameters_hydrology_template_frozen.txt","MESH_parameters_hydrology_template_base.txt")

#__________________________________________________________________________________________________________

#Load libraries
library(tidyverse)
library(dplyr)

#Create a tibble of the parameter ranges

best_pars <- read_csv(ParamFile, col_names=TRUE, skip_empty_rows = TRUE) #header=c("Parameter", "Value", "GRU", "Calibrate"))
best_pars <- filter(best_pars, is.na(Parameter)==FALSE)

#Read the template files into R
class_pars  <- readLines(ClassTemplate)
hydro_pars <- readLines(HydroTemplate)

class_pars_ini <- class_pars
hydro_pars_ini <- hydro_pars

i <- 1
while(i<=nrow(best_pars)) {
  class_pars_ini  <- gsub(best_pars[[i,1]], best_pars[[i,2]], class_pars_ini)
  hydro_pars_ini  <- gsub(best_pars[[i,1]], best_pars[[i,2]], hydro_pars_ini)
  i <- i+1
}

writeLines(class_pars_ini, con="MESH_parameters_CLASS.ini")
writeLines(hydro_pars_ini, con="MESH_parameters_hydrology.ini")

if (Calibration==TRUE){ 
  best_pars_cal <- filter(best_pars, Calibrate==FALSE) #Filter out calibrated parameters to leave the parameter codes in to be read by Ostrich in the written file
  
  class_pars_tpl <- class_pars
  hydro_pars_tpl <- hydro_pars

  i <- 1
while(i<=nrow(best_pars_cal)) {
  class_pars_tpl  <- gsub(best_pars_cal[[i,1]], best_pars_cal[[i,2]], class_pars_tpl)
  hydro_pars_tpl  <- gsub(best_pars_cal[[i,1]], best_pars_cal[[i,2]], hydro_pars_tpl)
  i <- i+1
}

writeLines(class_pars_tpl, con="MESH_parameters_CLASS.tpl")
writeLines(hydro_pars_tpl, con="MESH_parameters_hydrology.tpl")
}


