# Find-Replace Script for CLASS.ini and hydrology.ini files

This script is used to populate the MESH_parameters_CLASS.ini/.tpl and MESH_parameters_hydrology.ini/.tpl (.tpl files for calibration with Ostrich) from a .csv file containing a list of all the parameter values.

## Requirements:
- MESH_parameters_CLASS.txt and MESH_parameters_hydrology.txt files containing the parameter codes (ex. \_FCAN-NL1_) for all the parameters for the number of GRUs desired
  - NOTE: the default .txt files included with this script assume 3 soil layers, MID=1, and 10 GRUs; Therefore, the user must delete/add soil layers or GRUs as necessary and specify the start date and time of the meteorological data in the .txt files before running the script.

- ParamValues.csv file with the following columns:  
    *Note: Columns 1, 2, and 4 must remain, but the remaining columns are for refernece information only and can be modified by the user.*
  1. Parameter code (which corresponds to the code used in the .txt file)
  2. Parameter Value
  3. GRU Number
  4. Calibration (containing TRUE or FALSE; only used if the "Calibration" variable below is set to "TRUE")
  5. Description (which can be used to describe the parameter, or justification for the value selected)
