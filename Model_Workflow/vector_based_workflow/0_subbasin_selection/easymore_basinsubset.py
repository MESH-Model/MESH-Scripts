#%% load modules
from easymore.easymore import easymore
import geopandas as gpd
import numpy as np
from pathlib import Path
#Control file handling
# Easy access to control file folder
controlFolder = Path('../0_control_files')
 
# Store the name of the 'active' file in a variable
controlFile = 'control_active.txt'
 
#Function to extract a given setting from the control file
def read_from_control( file, setting ):
     
    # Open 'control_active.txt' and ...
    with open(file) as contents:
        for line in contents:
             
            # ... find the line with the requested setting
            if setting in line and not line.startswith('#'):
                break
     
    # Extract the setting's value
    substring = line.split('|',1)[1]      # Remove the setting's name (split into 2 based on '|', keep only 2nd part)
    substring = substring.split('#',1)[0] # Remove comments, does nothing if no '#' is found
    substring = substring.strip()         # Remove leading and trailing whitespace, tabs, newlines
        
    # Return this value   
    return substring
 
#Function to specify a default path
def make_default_path(suffix):
     
    # Get the root path
    rootPath = Path( read_from_control(controlFolder/controlFile,'root_path') )
     
    # Get the domain folder
    #domain_name = read_from_control(controlFolder/controlFile,'domain_name')
    #domainFolder = 'domain_' + domain_name
     
    # Specify the forcing path
    defaultPath = rootPath / suffix
     
    return defaultPath

# Get the domain folder
domain_name = read_from_control(controlFolder/controlFile,'domain_name')
domainFolder = 'domain_' + domain_name

#%% assigning input files
# Note : this is an example for the Fraser setup
input_basin_path = read_from_control(controlFolder/controlFile,'input_basin_path')
input_basin_name = read_from_control(controlFolder/controlFile,'input_basin_name')
#input_basin = '../cat_pfaf_78_MERIT_Hydro_v07_Basins_v01_bugfix1.shp'
input_river_path = read_from_control(controlFolder/controlFile,'input_river_path')
input_river_name = read_from_control(controlFolder/controlFile,'input_river_name')
#input_river = '../rivEndoMERITpfaf_78.shp'
 
outdir_basin = read_from_control(controlFolder/controlFile,'subset_basin_outdir')
outdir_river = read_from_control(controlFolder/controlFile,'subset_river_outdir')



# Specify default path if needed
if input_basin_path == 'default':
    input_basin_path = make_default_path('shape_file/catchment/') # outputs a Path()
else:
    input_basin_path = Path(input_basin_path) # make sure a user-specified path is a Path()
    
if input_river_path == 'default':
    input_river_path = make_default_path('shape_file/river_network/') # outputs a Path()
else:
    input_river_path = Path(input_river_path) # make sure a user-specified path is a Path()
    
if outdir_basin == 'default':
    outdir_basin = make_default_path('shape_file/catchment/') # outputs a Path()
else:
    outdir_basin = Path(outdir_basin) # make sure a user-specified path is a Path()
 
if outdir_river == 'default':
    outdir_river = make_default_path('shape_file/river_network/') # outputs a Path()
else:
    outdir_river = Path(outdir_river) # make sure a user-specified path is a Path()

#%% initializing easymore object
esmr = easymore()
# load the files and calculating the downstream of each segment
riv  = gpd.read_file(input_river_path/input_river_name)
cat    = gpd.read_file(input_basin_path/input_basin_name)
# get all the upstream
seg_IDs  = np.array(riv.COMID)
down_IDs = np.array(riv.NextDownID)
NTOPO    = esmr.get_all_downstream (seg_IDs,down_IDs)
 
#%% identify target segment ID
esmr.case_name = read_from_control(controlFolder/controlFile,'case_name') #e.g. 05NG001
target_segment = int(read_from_control(controlFolder/controlFile,'target_segment')) # e.g. 78011863
 
up_subbasins = esmr.get_all_upstream(target_segment,NTOPO) # segment ID
# subset
cat_up = cat.loc[cat['COMID'].isin(up_subbasins)]
riv_up = riv.loc[riv['COMID'].isin(up_subbasins)]
# plot
cat_up.plot()
riv_up.plot()
# save
cat_up.to_file(str(outdir_basin)+'/'+esmr.case_name+'_cat.shp')
riv_up.to_file(str(outdir_river)+'/'+esmr.case_name+'_riv.shp')