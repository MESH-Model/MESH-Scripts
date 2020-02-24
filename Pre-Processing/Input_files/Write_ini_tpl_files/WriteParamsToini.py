#=============================================================================================================================================#
# WriteParamsToini.py

# Written by Haley Brauner
# January 2020
# Contact: haley.brauner@usask.ca

#=============================================================================================================================================#
# This script is used to populate the MESH_parameters_CLASS.ini and MESH_parameters_hydrology.ini files, as well as the .tpl files for calibration with Ostrich if necessary.

# The script reads a .csv file containing a list of all the parameter values and their corresponding "tag". The benefit to specifying parameter values in a .csv file first is to include additional information, such as parameters sources, justification, etc.

# MESH_parameters_CLASS_template.txt and MESH_parameters_hydrology_template.txt files contain the parameter codes (ex._FCAN-NL1_) for all the parameters for the number of GRUs desired.

#=============================================================================================================================================#
# BEFORE RUNNING, DO THE FOLLOWING:
#   - Update the *_template*.txt files with the number of soil layers, GRUs, start date of the meteorological data, number of GRUs, and number of t0_ACC values (if using the Frozen algorithm)
#   - Delete/add soil layers or grus as necessary and specify the start date and time of the meteorological data.
#   - Populate the ParamValues_template.csv file with the following columns:
#       Note: columns 1, 2, and 4 must remain, but the remaining columns are for reference information only and can be modified by the user.
#               1. Parameter code (which corresponds to the code used in the .txt file)
#               2. Parameter Value - to be written to the text file
#               3. GRU Number
#               4. Calibration (containing TRUE or FALSE; only used if the "Calibration" variable below is set to "TRUE")
#               5-6. Notes and Parameter Description can be used for documenting and justifying the values selected
#   - Set the file paths and True/False values below
#=============================================================================================================================================#

# SET SCRIPT VALUES

# Specify the file names (and path, if not in the host folder) for the .csv file containing the parameter values
ParamPath = 'ParamValues.csv'

# Specify the file names (and path, if not in host folder) for the template files
class_template = 'MESH_parameters_CLASS_template.txt'
hydro_template_base = 'MESH_parameters_hydrology_template_base.txt'
hydro_template_frozen = 'MESH_parameters_hydrology_template_frozen.txt'

# Also specify the following
Calibration = False
print('Calibration = ', Calibration)
# True for calibrating the model to only replace non-calibrated parameter values (where column 3=FALSE) and also generate .tpl files
# False to replace all parameters with the values and only generate .ini files

Frozen = True
print('Frozen = ', Frozen)
# True to use the Frozen Soil infiltration algorithm (more details on the MESH wiki)
# False to use the Class infiltration algorithm

#=============================================================================================================================================#

# IMPORT PACKAGES
import pandas as pd

# READ IN THE PARAMETER VALUES FROM CSV FILE
pars_read = pd.read_csv(ParamPath, usecols = [0,1,2,3])
pars_read = pars_read.dropna(how='all')

# WRITE THE TEMPLATE TO THE .INI FILE

    # Read the files and obtain the tags
c_templ = open(class_template,'r')
classtags = c_templ.read()
c_templ.close()

if Frozen==True:
    h_templ = open(hydro_template_frozen,'r')
else:
    h_templ = open(hydro_template_base,'r')

hydrotags = h_templ.read()
h_templ.close

    # Write the tags to initialize the .ini files
c_ini = open('MESH_parameters_CLASS.ini','w')
c_ini.write(classtags)
c_ini.close()

h_ini = open('MESH_parameters_hydrology.ini','w')
h_ini.write(hydrotags)
h_ini.close()

# CREATE THE .INI FILES
for i in range(len(pars_read)):
    tag = pars_read.iloc[i,0]
    val = pars_read.iloc[i,1]

    c = open('MESH_parameters_CLASS.ini','r')
    cdata = c.read()
    c.close()
    h = open('MESH_parameters_hydrology.ini','r')
    hdata = h.read()
    h.close()

    new_cdata = cdata.replace(tag, val)
    new_hdata = hdata.replace(tag, val)

    f = open('MESH_parameters_CLASS.ini','w')
    f.write(new_cdata)
    f.close()
    f = open('MESH_parameters_hydrology.ini','w')
    f.write(new_hdata)
    f.close()

# CREATE THE .TPL FILES IF CALIBRATION=TRUE
if Calibration==True:
    pars_write = pars_read[pars_read['Calibrate']==False]

    c_tpl = open('MESH_parameters_CLASS.tpl','w')
    c_tpl.write(classtags)
    c_tpl.close()
    h_tpl = open('MESH_parameters_hydrology.tpl','w')
    h_tpl.write(hydrotags)
    h_tpl.close()

    for i in range(len(pars_write)):
        tag_tpl = pars_write.iloc[i,0]
        val_tpl = pars_write.iloc[i,1]

        c = open('MESH_parameters_CLASS.tpl','r')
        cdata = c.read()
        c.close()
        h = open('MESH_parameters_hydrology.tpl','r')
        hdata = h.read()
        h.close()

        new_cdata = cdata.replace(tag_tpl, val_tpl)
        new_hdata = hdata.replace(tag_tpl, val_tpl)

        f = open('MESH_parameters_CLASS.tpl','w')
        f.write(new_cdata)
        f.close()
        f = open('MESH_parameters_hydrology.tpl','w')
        f.write(new_hdata)
        f.close()

#=============================================================================================================================================#
