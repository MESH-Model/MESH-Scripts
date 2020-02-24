# Find-Replace Script for CLASS.ini and hydrology.ini files

This folder contains both an R script and Python script which may be used to populate the MESH_parameters_CLASS.ini/.tpl and MESH_parameters_hydrology.ini/.tpl (.tpl files for calibration with Ostrich) from a .csv file containing a list of all the parameter values, and a .txt template of each. Some may find it easier to keep track of parameter values in a .csv file than in the arrangement of the .ini files. The .csv file can also be filtered, and descriptions or justification notes added in column 5+ for record-keeping purposes.

## Requirements:
- MESH_parameters_CLASS.txt and MESH_parameters_hydrology.txt files, which are the "template" for the .ini/.tpl files and contain the parameter tags (ex. \_FCAN-NL1_) for all the parameters for the number of GRUs desired
  - NOTE: the default .txt files included with this script assume 3 soil layers, MID=1, and 10 GRUs; Therefore, the user must delete/add soil layers or GRUs as necessary and specify the start date and time of the meteorological data before running the script.

- ParamValues.csv file with the following columns:  
    *Note: Columns 1, 2, and 4 must remain, but the remaining columns are for reference information only and can be modified by the user.*
    1. Parameter tag (which corresponds to the tag used in the .txt file)
    2. Parameter Value
    3. GRU Number
    4. Calibration (containing TRUE or FALSE; only used if the "Calibration" variable below is set to "TRUE")
    5. Description (which can be used to describe the parameter, or justification for the value selected)

## To Use
  1. Decide your model configuration (# of GRUs, # soil layers, algorithms to activate, etc.).
  2. Modify the .txt and .csv files to reflect the number of GRUs (note the format of the parameter tags used), number of soil layers, as well as the start time of the meteorological data (.txt file only). Note that the format of the .txt files and the parameters in the .csv file may needed to be adjusted depending on which algorithms are used in MESH; see the [MESH wiki](https://wiki.usask.ca/display/MESH/MESH+User+Page) for more details.
  3. For the R script, set the working directory to the location of the files, and set "Calibration" and "Frozen" to either TRUE or FALSE.

     For the Python script, set the file paths (or names if in the same folder as the script) for the parameter file and template file, set "Calibration" to either True or False, and "Frozen" to either True or False
  4. Run the R or Python script. This will replace the parameter tag in the .txt file with the value specified in the .csv file and generate a .ini file (and a .tpl file if Calibration=TRUE).
