# -*- coding: utf-8 -*-
# """
# NAME
    # MESH_vectorbased_forcing 
# PURPOSE
    # The purpose of this script is to extract vector-based forcing files 
    # remapped from easymore and reorder 
    # them based rank, then save them to a netcdf format that can be read by 
    # MESH model
# PROGRAMMER(S)
    # Ala Bahrami
# REVISION HISTORY
    # 20210513 -- Initial version created and posted online
    # 20220626 -- 1) Instead of reading the shape file, the MeritHydro subbasin metadata 
                # is read as an input for reindexing forcing. 
                # 2) changed variable names to match RDRSV2.1
    
# See also 
        # easymore_extarct, create_MESH_drainage_database
        
# REFERENCES

# """
# %% load modules 
#import os
import numpy as np
import xarray as xs
import time 
from shutil import copyfile
from datetime import datetime
from pathlib import Path
#import pandas as pd
#import geopandas as gpd 

# Control file handling
# Access the control file folder
controlFolder = Path('../0_control_files')

# store the name of the 'active' file in a variable
controlFile = 'control_active.txt'

# function to extract a given setting from the control file
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

#function to specify a default path
def make_default_path(suffix):
     
    # Get the root path
    rootPath = Path( read_from_control(controlFolder/controlFile,'root_path') )
     
    # Specify the forcing path
    #defaultPath = rootPath / domainFolder / suffix
    defaultPath = rootPath / suffix 
    return defaultPath

# Get the domain folder
domain_name = read_from_control(controlFolder/controlFile,'domain_name')
domainFolder = 'domain_' + domain_name

# Get additional settings
dataset     =  read_from_control(controlFolder/controlFile,'forcing_dataset')
startTime   = read_from_control(controlFolder/controlFile,'forcing_start')

# %% define input file 
forcing_dir  = read_from_control(controlFolder/controlFile,'remapping_out')
if forcing_dir == 'default':
    forcing_dir = make_default_path('forcing') # outputs a Path()
else:
    forcing_dir = forcing_dir # make sure a user-specified path is a Path()

forcing_dataset = read_from_control(controlFolder/controlFile,'forcing_dataset')
forcing_name = '{}_{}_remapped_{}.nc'.format(dataset,domain_name,startTime)
ddb_folder   = read_from_control(controlFolder/controlFile,'DDB_output_dir')
if ddb_folder == 'default':
    ddb_folder = make_default_path('vector_based_workflow/workflow_data/domain_{}/drainagedatabase'.format(domain_name)) # outputs a Path()
else:
    ddb_folder = ddb_folder # make sure a user-specified path is a Path()

# Read the forcing variable names
pcpn = read_from_control(controlFolder/controlFile,'pcpn_var')
wind = read_from_control(controlFolder/controlFile,'wind_var')
lnwv = read_from_control(controlFolder/controlFile,'lnwv_var')
shwv = read_from_control(controlFolder/controlFile,'shwv_var')
temp = read_from_control(controlFolder/controlFile,'temp_var')
humi = read_from_control(controlFolder/controlFile,'humi_var')
pres = read_from_control(controlFolder/controlFile,'pres_var')

var_lon = read_from_control(controlFolder/controlFile,'var_lon')
var_lat = read_from_control(controlFolder/controlFile,'var_lat')
var_time = read_from_control(controlFolder/controlFile,'var_time')


# %% reading input basin 
# the segids are stored in the remapped forcing, so it is not necessary to read input shape file
#basin = gpd.read_file(inut_basin)

# %% reading input netcdf files db
start_time = time.time()
db = xs.open_dataset(ddb_folder / '{}_MESH_drainage_database.nc'.format(domain_name))
db.close()
segid =  db.variables['seg_id'].values
# reading for control check 
lon = db.variables[var_lon].values
lat = db.variables[var_lat].values

# %% reading input forcing 
forc = xs.open_dataset(forcing_dir / forcing_name)
forc.close()
lon_ease = forc.variables['longitude'].values
lat_ease = forc.variables['latitude'].values

# %% extract indices of forcing ids based on the drainage database
# Note: the input forcing is arranaged based on COMID of input MeritHydro Catchment 
# So, a reordering of forcing indices is required to match order Rank1 to RankN in MESH

n = len(segid)
ind = []
for i in range(n):
    fid = np.where(np.int32(forc['ID'].values) == segid[i])[0]
    ind = np.append(ind, fid)
ind = np.int32(ind)  

# %% reorder input forcing 
# should check if dim[0] is time 
# Note : name of variables is hard coded and can be modified regarding the input forcing 
# PR, FI','FB','TT','UV','P0','HU' 

forc_vec = xs.Dataset(
    {
        pcpn: (["subbasin", var_time], forc[pcpn].values[:,ind].transpose()),
    },
    coords={
        var_time: forc['time'].values.copy(),
        var_lon: (["subbasin"], lon),
        var_lat: (["subbasin"], lat),
    }
    )

forc_vec[pcpn].encoding['coordinates'] = 'time lon lat'
forc_vec[pcpn].attrs["units"]          = forc[pcpn].units
forc_vec[pcpn].attrs["grid_mapping"]   = 'crs'

for n in [lnwv,shwv,temp,
          wind,pres,humi]:
    forc_vec[n] = (("subbasin", var_time), forc[n].values[: , ind].transpose()) 
    forc_vec[n].coords[var_time]          = forc[var_time].values.copy()
    forc_vec[n].coords[var_lon]           = (["subbasin"], lon)
    forc_vec[n].coords[var_lat]           = (["subbasin"], lat)
    forc_vec[n].attrs["units"]          = forc[n].units
    forc_vec[n].attrs["grid_mapping"]   = 'crs'
    forc_vec[n].encoding['coordinates'] = 'time lon lat'

# %% update meta data attribuetes 
forc_vec.attrs['Conventions'] = 'CF-1.6'
forc_vec.attrs['License']     = 'The data were written by Ala Bahrami'
forc_vec.attrs['history']     = 'Created on June 07, 2021'
forc_vec.attrs['featureType'] = 'timeSeries'          

# editing lat attribute
forc_vec['lat'].attrs['standard_name'] = 'latitude'
forc_vec['lat'].attrs['units']         = 'degrees_north'
forc_vec['lat'].attrs['axis']          = 'Y'
 
# editing lon attribute
forc_vec['lon'].attrs['standard_name'] = 'longitude'
forc_vec['lon'].attrs['units']         = 'degrees_east'
forc_vec['lon'].attrs['axis']          = 'X'

# editing time attribute
forc_vec['time'].attrs['standard_name'] = 'time'
forc_vec['time'].attrs['axis']          = 'T'
forc_vec['time'].encoding['calendar']   = 'gregorian'
forc_vec.encoding.update(unlimited_dims = 'time')

# coordinate system
forc_vec['crs'] = db['crs'].copy()

# Define a variable for the points and set the 'timeseries_id' (required for some viewers).
forc_vec['subbasin'] = (['subbasin'], db['seg_id'].values.astype(np.int32).astype('S20'))
forc_vec['subbasin'].attrs['cf_role'] = 'timeseries_id'

#%% save to netcdf 
comp = dict(zlib=True, complevel=6)
encoding = {var: comp for var in forc_vec.data_vars}
forc_vec.to_netcdf(forcing_dir / 'MESH_{}'.format(forcing_name),encoding=encoding)
print('--%s seconds--' %(time.time() - start_time))


# --- Code provenance
# Generates a basic log file in the domain folder and copies the control file and itself there.
 
# Set the log path and file name
logPath = Path(forcing_dir)
log_suffix = '_MESH_vectorbased_forcing.txt'
 
# Create a log folder
logFolder = '_workflow_log'
Path( logPath / logFolder ).mkdir(parents=True, exist_ok=True)
 
# Copy this script
thisFile = '4_MESH_vectorbased_forcing.py'
copyfile(thisFile, logPath / logFolder / thisFile);
 
# Get current date and time
now = datetime.now()
 
# Create a log file
logFile = now.strftime('%Y%m%d') + log_suffix
with open( logPath / logFolder / logFile, 'w') as file:
     
    lines = ['Log generated by ' + thisFile + ' on ' + now.strftime('%Y/%m/%d %H:%M:%S') + '\n',
             'Generated remapped climate forcing .nc file, reordered to MESH requirements.']
    for txt in lines:
        file.write(txt)