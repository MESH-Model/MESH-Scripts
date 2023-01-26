#!/cvmfs/soft.computecanada.ca/easybuild/software/2020/avx2/Core/python/3.8.10/bin/python
# which python to find the location of installed pyhthon on graham

# %% loading EASYMORE
from easymore.easymore import easymore
from pathlib import Path
from shutil import copyfile
from datetime import datetime


# %%Control file handling
controlFolder = Path('../0_control_files')  # Easy access to control file folder
controlFile = 'control_active.txt'          # Store the name of the 'active' file in a variable


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
    defaultPath = rootPath / suffix 
    return defaultPath


# %% Get the domain folder
domain_name = read_from_control(controlFolder/controlFile,'domain_name')
domainFolder = 'domain_' + domain_name 

#  %% Get the target shapefile
target_shp_path = read_from_control(controlFolder/controlFile,'river_basin_shp_path')
# Specify default path if needed
if target_shp_path == 'default':
    target_shp_path = make_default_path('shapefiles/catchment/') # outputs a Path()
else:
    target_shp_path = Path(target_shp_path) # make sure a user-specified path is a Path()
target_shp_name = read_from_control(controlFolder/controlFile,'river_basin_shp_name')
target_shp      = target_shp_path / target_shp_name
target_shp_ID   = read_from_control(controlFolder/controlFile,'river_basin_shp_rm_hruid')

# %% Get the source forcing file
source_nc_path  = read_from_control(controlFolder/controlFile,'source_nc_path')
# Specify default path if needed
if source_nc_path == 'default':
    source_nc_path = make_default_path('forcing') # outputs a Path()
else:
    source_nc_path = Path(source_nc_path) # make sure a user-specified path is a Path()
source_nc_name  = read_from_control(controlFolder/controlFile,'source_nc_name')
source_nc     = str(source_nc_path / source_nc_name)


outdir = read_from_control(controlFolder/controlFile,'remapping_out')
if outdir == 'default':
    outdir = str(make_default_path('forcing/'))+'/' # outputs a Path()
else:
    outdir = outdir # make sure a user-specified path is a Path()


# %% Get the list of variable neames
var_names       = read_from_control(controlFolder/controlFile,'var_names').split(', ')

# Get the forcing dataset name
forcing_dataset = read_from_control(controlFolder/controlFile,'forcing_dataset')

# %% initializing EASYMORE object
esmr = easymore()
# specifying EASYMORE objects
# name of the case; the temporary, remapping and remapped file names include case name
esmr.case_name                = '{}_{}'.format(forcing_dataset,domain_name)
# temporary path that the EASYMORE generated GIS files and remapped file will be saved
esmr.temp_dir                 = '{}/temporary{}/'.format(source_nc_path,domain_name)
# name of target shapefile that the source netcdf files should be remapped to
esmr.target_shp               = target_shp
                                #'../shape_file/cat_pfaf_81_MERIT_Hydro_v07_Basins_v01_bugfix1_WGS84.shp'
esmr.target_shp_ID            = target_shp_ID
# name of netCDF file(s); multiple files can be specified with *
esmr.source_nc                = source_nc 
                                #'/project/6008034/baha2501/NA/forcing/NA_RDRS_v2.1_1980010112.nc'
                                
# name of variables from source netCDF file(s) to be remapped
esmr.var_names                = var_names

# rename the variables from source netCDF file(s) in the remapped files;
# it will be the same as source if not provided
#esmr.var_names_remapped       = ['RDRS']
# name of variable longitude in source netCDF files
esmr.var_lon                  =  read_from_control(controlFolder/controlFile,'var_lon')
# name of variable latitude in source netCDF files
esmr.var_lat                  = read_from_control(controlFolder/controlFile,'var_lat')
# name of variable time in source netCDF file; should be always time
esmr.var_time                 = read_from_control(controlFolder/controlFile,'var_time')
# location where the remapped netCDF file will be saved
esmr.output_dir               = outdir
# format of the variables to be saved in remapped files,
# if one format provided it will be expanded to other variables
esmr.format_list              = read_from_control(controlFolder/controlFile,'format_list').split(', ')
# fill values of the variables to be saved in remapped files,
# if one value provided it will be expanded to other variables
esmr.fill_value_list          = read_from_control(controlFolder/controlFile,'fill_value_list').split(', ')
# if required that the remapped values to be saved as csv as well
#esmr.save_csv                 = True
#esmr.complevel                 =  9
# if uncommented EASYMORE will use this and skip GIS tasks
#esmr.remap_csv                = 'temporary78/subbasin_select/RDRS_78_remapping.csv' # RDRS_81_remapping.csv
                     
# # create source shapefile 
import geopandas as gpd
esmr.NetCDF_SHP_lat_lon()
# create the source shapefile for case 1 and 2 if shapefile is not provided
if (esmr.case == 1 or esmr.case == 2)  and (esmr.source_shp == ''):
    if esmr.case == 1:
        if hasattr(esmr, 'lat_expanded') and hasattr(esmr, 'lon_expanded'):
            esmr.lat_lon_SHP(esmr.lat_expanded, esmr.lon_expanded,\
                esmr.temp_dir+esmr.case_name+'_source_shapefile.shp')
        else:
            esmr.lat_lon_SHP(esmr.lat, esmr.lon,\
                esmr.temp_dir+esmr.case_name+'_source_shapefile.shp')
    else:
        esmr.lat_lon_SHP(esmr.lat, esmr.lon,\
            esmr.temp_dir+esmr.case_name+'_source_shapefile.shp')
    print('EASYMORE is creating the shapefile from the netCDF file and saving it here:')
    print(esmr.temp_dir+esmr.case_name+'_source_shapefile.shp')

shp = gpd.read_file(esmr.temp_dir+esmr.case_name+'_source_shapefile.shp')
shp = shp [shp['lon_s']>-179]
shp.to_file(esmr.temp_dir+esmr.case_name+'_source_shapefile.shp')

# add the source shapefile 
esmr.source_shp                =   esmr.temp_dir+esmr.case_name+'_source_shapefile.shp'
esmr.source_shp_lat            =  'lat_s' # name of column latitude in the source shapefile
esmr.source_shp_lon            =  'lon_s' # name of column longitude in the source shapefile
esmr.source_shp_ID             =  'ID_s' # name of column ID in the source shapefile

# execute EASYMORE
esmr.nc_remapper()
# %%

# --- Code provenance
# Generates a basic log file in the domain folder and copies the control file and itself there.
 
# Set the log path and file name
logPath = Path(outdir)
log_suffix = '_easymore_remapping.txt'
 
# Create a log folder
logFolder = '_workflow_log'
Path( logPath / logFolder ).mkdir(parents=True, exist_ok=True)
 
# Copy this script
thisFile = '2_easymore_remapping.py'
copyfile(thisFile, logPath / logFolder / thisFile);
 
# Get current date and time
now = datetime.now()
 
# Create a log file
logFile = now.strftime('%Y%m%d') + log_suffix
with open( logPath / logFolder / logFile, 'w') as file:
     
    lines = ['Log generated by ' + thisFile + ' on ' + now.strftime('%Y/%m/%d %H:%M:%S') + '\n',
             'Generated remapped climate forcing .nc file.']
    for txt in lines:
        file.write(txt)