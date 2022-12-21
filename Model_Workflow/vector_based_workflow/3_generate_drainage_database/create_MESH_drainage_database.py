# -*- coding: utf-8 -*-
"""
Name
    create_MESH_drainage_database. previously called lc_vectorbased
Purpose
    The purpose of this script is to calculate land cover fractions for each 
    subbasin of interest. Then the landcover is converted the (subbasin*lc_types)
    it is adhered to the driange database.  
Programmer(s)
    Ala Bahrami
    Cooper Albano
Revision History
    20210422 -- Initial version created  
    20210504 -- 1) changed dimension name from 'lc_type' to 'ngru' and variable 
    name from 'lc_frac' to 'GRU'. 2) Added LandUse variable
    20210505 -- append the LandUse information to drainage_ddb 
    20210506 -- changed 'ngru' dimension to 'gru' to be consistent with MESH code 
    20210604 -- modified I/O and variables for Fraser application 
    20210704 -- modified to the new_rank_extract function 
    20220623 -- 1) Modified based on the new_rank_modi2 which is adaptable for  
                multi-outlet
                2) Consider the entire 19 land cover classes instead of regrouping
                3) save subbasin reordered metadata
                4) visualize and save subbasin selection for any outlet (optional) 
    20220626 -- 1) modified the way of reindexing the zonal histogram 
    20220825 -- 1) modify code to adapt NEXT variable having multiple outlets 
    20221101 -- 1) added a line to accept GIS tool .csv zonal hist as pandas dataframe
             -- 2) added a line to rename prefix_0 to prefix_NOD for the GIS tool .csv zonal hist
             -- 3) removed landcover fraction calculation for GIS tool .csv file
    20221116 -- 1) removed .csv column reordering. No longer necessary due to gistool bug fix
    20221123 -- 1) added functionality to read I/O from control file.
See also 
    
Reference 

Todo:
    1) The lc_types is based on NALCMS 2010. The name list is hard-coded   
"""

# %% importing modules 
import geopandas as gpd
import numpy as np
import xarray as xs
import pandas as pd
from   datetime import date
from datetime import datetime
from pathlib import Path
from shutil import copyfile
import time 

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
    #domainName = read_from_control(controlFolder/controlFile,'domain_name')
    #domainFolder = 'domain_' + domainName
     
    # Specify the forcing path
    defaultPath = rootPath / suffix
     
    return defaultPath

# Get the domain folder
domain_name = read_from_control(controlFolder/controlFile,'domain_name')
domainFolder = 'domain_' + domain_name

## Find location of zonal statistics file
# Zonal statistics file path & name
lc_zh_path = read_from_control(controlFolder/controlFile,'input_lc_zh_path')
lc_zh_name = read_from_control(controlFolder/controlFile,'input_lc_zh_name')

# Specify default path if needed
if lc_zh_path == 'default':
    lc_zh_path = make_default_path('vector_based_workflow/workflow_data/domain_'+domain_name+'/zonalhist/') # outputs a Path()
else:
    lc_zh_path = Path(lc_zh_path) # make sure a user-specified path is a Path()

## Find location of network topology file
# Network topology file path & name
topo_path = read_from_control(controlFolder/controlFile,'input_topo_path')
topo_name = read_from_control(controlFolder/controlFile,'input_topo_name')

# Specify default path if needed
if topo_path == 'default':
    topo_path = make_default_path('vector_based_workflow/workflow_data/domain_'+domain_name+'/topology/') # outputs a Path()
else:
    topo_path = Path(topo_path) # make sure a user-specified path is a Path()

## Find location of MERIT Basin shapefile
# MERIT Bassin shapefile path & name
merit_path = read_from_control(controlFolder/controlFile,'merit_basin_path')
merit_name = read_from_control(controlFolder/controlFile,'merit_basin_name')

# Specify default path if needed
if merit_path == 'default':
    merit_path = make_default_path('shape_file/catchment/') # outputs a Path()
else:
    merit_path = Path(merit_path) # make sure a user-specified path is a Path()

## Find location of output directory
# output directory path
outdir = read_from_control(controlFolder/controlFile,'DDB_output_dir')

# Specify default path if needed
if outdir == 'default':
    outdir = make_default_path('vector_based_workflow/workflow_data/domain_'+domain_name+'/drainagedatabase/') # outputs a Path()
else:
    outdir = Path(outdir) # make sure a user-specified path is a Path()
outdir.mkdir(parents=True, exist_ok=True)

# %% directory of input files
# Enter path to a zonal histogram file in either .csv format from GIS tool or in .shp format from QGIS
start_time = time.time() 
input_lc_zh              = lc_zh_path/lc_zh_name     
input_topology           = topo_path/topo_name
Merit_catchment_shape    = merit_path/merit_name
domain_name              = read_from_control(controlFolder/controlFile,'domain_name')
lc_type_prefix           = read_from_control(controlFolder/controlFile,'lc_type_prefix')

#%% Function reindex to extract drainage database variables 
def new_rank_extract(input_topology): 
        #% Reading topology file and finding outlets
        drainage_db = xs.open_dataset(input_topology)
        drainage_db.close()

        segid = drainage_db['seg_id'].values
        tosegment = drainage_db['tosegment'].values

        # Count the number of outlets
        outlets = np.where(tosegment == 0)[0]

        #% Search over to extract the subbasins drain into each outlet
        rank_id_domain = np.array([]).astype(int)   
        outlet_number = np.array([]).astype(int) 
        for k in range(len(outlets)):
            # initial step 
            #segid_target = drainage_db['seg_id'].values[outlets[k]]
            segid_target = segid[outlets[k]]
            # set the rank of the outlet 
            rank_id = outlets[k]
            
            # find upstream segids drains into downstream
            while(np.size(segid_target) >= 1): 
                if (np.size(segid_target) == 1):
                    r = np.where(tosegment == segid_target)[0]
                else:
                    r = np.where(tosegment == segid_target[0])[0]    
                # updated the target segid 
                segid_target = np.append(segid_target, segid[r])
                # remove the first searched target
                segid_target = np.delete(segid_target,0,0)
                if (len(segid_target) == 0):
                    break
                # update the rank_id
                rank_id = np.append(rank_id,r)
            rank_id = np.flip(rank_id) 
            if (np.size(rank_id) > 1):
                outlet_number = np.append(outlet_number, (k)*np.ones((len(rank_id),1)).astype(int))
            else:
                outlet_number = np.append(outlet_number, (k))
            rank_id_domain = np.append(rank_id_domain, rank_id)
            rank_id = []
        #% reorder segid and tosegment 
        segid = segid[rank_id_domain]
        tosegment = tosegment[rank_id_domain]         
              
        # rearrange outlets to be consistent with MESH outlet structure
        # NB: In MESH outlets should be placed at the end of NEXT variable 
        NA = len(rank_id_domain)
        fid1 = np.where(tosegment != 0)[0]
        fid2 = np.where(tosegment == 0)[0]
        fid =  np.append(fid1,fid2)
        
        rank_id_domain = rank_id_domain[fid]
        segid =segid[fid]
        tosegment = tosegment[fid]
        outlet_number = outlet_number[fid]
        
        #% construct Rank and Next variables 
        Next = np.zeros(NA).astype(np.int32)
        
        for k in range(NA):
            if (tosegment[k] != 0):
                r = np.where(tosegment[k] == segid)[0] + 1 
                Next[k] = r
            else:
                Next[k] = 0
                
        # Construct Rank from 1:NA
        Rank = np.arange(1,NA+1).astype(np.int32)
        
        #% save subbasins reordered metadata 
        dt = {'Merit_reorderd_ID':rank_id_domain, 'Outlet_Number':outlet_number, 
              'Rank':Rank,'Next':Next,'Segid':segid,'tosegment':tosegment}
        df = pd.DataFrame(data=dt, dtype = np.int64)
        outrank = domain_name+'_Rank_ID'+'.csv'
        df.to_csv(outdir/outrank, index=False)
        
        # % reordering network topology variables based on Rank 1:NA
        for m in ['basin_area', 'length', 'slope', 'lon', 'lat', 'hruid', 
                  'seg_id', 'seg_hr_id', 'tosegment', 'width', 'manning']:
            drainage_db[m].values = drainage_db[m].values[rank_id_domain]
            
        # % check if channel slope values match the minimum threshold 
        min_slope = 0.000001
        drainage_db['slope'].values[drainage_db['slope'].values < min_slope] = min_slope
        
        # % Adding Rank and Next variables to the file
        drainage_db['Rank'] = (['n'], Rank) 
        drainage_db['Rank'].attrs.update(standard_name = 'Rank', 
                            long_name = 'Element ID', units = '1', _FillValue = -1)
        
        drainage_db['Next'] = (['n'], Next) 
        drainage_db['Next'].attrs.update(standard_name = 'Next', 
                           long_name = 'Receiving ID', units = '1', _FillValue = -1)

        # % Adding missing attributes and renaming variables
        # Add 'axis' and missing attributes for the 'lat' variable.
        drainage_db['lat'].attrs['standard_name'] = 'latitude'
        drainage_db['lat'].attrs['units'] = 'degrees_north'
        drainage_db['lat'].attrs['axis'] = 'Y'
         
        # Add 'axis' and missing attributes for the 'lon' variable.
        drainage_db['lon'].attrs['standard_name'] = 'longitude'
        drainage_db['lon'].attrs['units'] = 'degrees_east'
        drainage_db['lon'].attrs['axis'] = 'X'
         
        # Add or overwrite 'grid_mapping' for each variable (except axes).
        for v in drainage_db.variables:
            if (drainage_db[v].attrs.get('axis') is None):
                drainage_db[v].attrs['grid_mapping'] = 'crs'
         
        # Add the 'crs' itself (if none found).
        if (drainage_db.variables.get('crs') is None):
            drainage_db['crs'] = ([], np.int32(1))
            drainage_db['crs'].attrs.update(grid_mapping_name = 'latitude_longitude', longitude_of_prime_meridian = 0.0, semi_major_axis = 6378137.0, inverse_flattening = 298.257223563)
         
        # Rename variables.
        for old, new in zip(['basin_area', 'length', 'slope', 'manning'], ['GridArea', 'ChnlLength', 'ChnlSlope', 'R2N']):
            drainage_db = drainage_db.rename({old: new})
         
        # Rename the 'subbasin' dimension (from 'n').
        drainage_db = drainage_db.rename({'n': 'subbasin'})
        
        # % Specifying the NetCDF "featureType"
        # Add a 'time' axis with static values set to today (in this case, time is not actually treated as a dimension).
        drainage_db['time'] = (['subbasin'], np.zeros(len(rank_id_domain)))
        drainage_db['time'].attrs.update(standard_name = 'time', units = ('days since %s 00:00:00' % date.today().strftime('%Y-%m-%d')), axis = 'T')
         
        # Set the 'coords' of the dataset to the new axes.
        drainage_db = drainage_db.set_coords(['time', 'lon', 'lat'])
         
        # Add (or overwrite) the 'featureType' to identify the 'point' dataset.
        drainage_db.attrs['featureType'] = 'point'
        
        return rank_id_domain, drainage_db, outlet_number
    
# %% calling the new_rank_extract
rank_id_domain, drainage_db, outlet_number = new_rank_extract(input_topology)

# %% reading the input zonal histogram of landcover and reindex it. 
if str(input_lc_zh).endswith('.shp'):
    lc_zonal_hist = gpd.read_file(input_lc_zh)                        # read QGIS .shp zonal histogram
    lc_zonal_hist = lc_zonal_hist.sort_values(by=['COMID'])           # sort by COMID for QGIS zonal histogram
elif str(input_lc_zh).endswith('.csv'):
    lc_zonal_hist = pd.read_csv(input_lc_zh)                           # read GIS tool .csv zonal histogram
    lc_zonal_hist = lc_zonal_hist.sort_values(by=['COMID'])           # sort by COMID for GIS tool zonal histogram
else:
    print('Zonal histogram not recognized.')
    exit()

# rename frac_0 to frac_NOD for compatibility with verify lc_types. Not necessary for QGIS version.
if str(input_lc_zh).endswith('.csv'):
    lc_zonal_hist = lc_zonal_hist.rename(columns={lc_type_prefix+'0':lc_type_prefix+'NOD'})
    cols = lc_zonal_hist.columns.tolist()
    for i in cols:
        if lc_type_prefix in i:
            if 'NOD' in i:
                nod=i
                cols.remove(i)
                cols.append(nod)
    lc_zonal_hist = lc_zonal_hist[cols]

#%% reading source MeritHydro catchment file and visualize and save subbasin selection
## NB: this section can be uncommented if a user want to do a sanity check of the subbasin selection 
## list of major segid_target outlet ids per each PFAF
## {78011862 (Fraser), 78017388(columbia), 82000048(MRB), 
## 83012503, 71004266 (Hudson), 72039675 (St.Laurent), 
## Mississipi (74072586), 73017442, 81018374 (Yukon), 77032206,
## 75022612, 75038087 (Hondo River), 75038096 (Usumacinta)}

# shape_catchment = gpd.read_file(Merit_catchment_shape)
# shape_catchment = shape_catchment.sort_values(by=['COMID'])
# shape_catchment.reset_index(drop=True, inplace=True)

# segid = drainage_db['seg_id'].values
# segid_target = 75038096 
# r = np.where(segid == segid_target)[0] 
# r2 = np.where(outlet_number == outlet_number[r])[0]
# rank_id = rank_id_domain[r2]

# shape_catchment.loc[rank_id].plot(color='white', edgecolor='black')
# shape_catchment.loc[rank_id].to_file(outdir+'PFAF_subselect_'+'%d'%segid_target+'.shp')

# %% land class types 
# NB: the NOD here represent the No-data. The NALCMS data has no-data category which its values is zero 
lc_type = np.array(['Temperate/sub-polar needleleaf forest','Sub-polar taiga needleleaf forest','Tropical/sub-tropical broadleaf evergreen forest','Tropical/sub-tropical broadleaf deciduous forest',
           'Temperate/sub-polar broadleaf deciduous forest','Mixed Forest','Tropical/sub-tropical shrubland', 'Temperate/sub-polar shrubland',
           'Tropical/sub-tropical grassland','Temperate/sub-polar grassland','Sub-polar/polar shrubland-lichen-moss','Sub-polar/polar grassland-lichen-moss',
           'Sub-polar/polar barren-lichen-moss','Wetland','Cropland','Barren Lands',
           'Urban-Built-up','Water','Snow-Ice','No-data'])
 
# %% verify list of lc types
m = len(lc_type) + 1
st = [];
p  = [];
for i in (range(1,m)):
    if (i < m-1):
        st1 =   lc_type_prefix+ str(i)
    else:
        st1 =   lc_type_prefix+ 'NOD' 
    
    st = np.append(st, st1)
    fid = np.where(lc_zonal_hist.columns == st1)[0]
    if (fid.size == 0):
        print ('land cover %s is not presented in the list of NALCMS land cover for this PFAF' % lc_type[i-1])
        p = np.int32(np.append(p, i-1))
        
# add dummy land cover type required by MESH 
lc_type = np.append(lc_type, 'Dump')    

# remove missing land cover types from  the list 
if (len(p) != 0) :
    lc_type = np.delete(lc_type, p)
    
#%% calculate land cover fraction 
# extract land cover zonal hist
lc_frac = lc_zonal_hist.filter(like=lc_type_prefix, axis = 1)

# NB: Based on NALCMS LANDSAT data, the open water data are classified as No-DATA. 
# NB: So, if the catchments have some no-data, users should verify if it falls inside the open-water 
# NB: that later be added to t he 'Water' land cover class. 

# here the it is required to add NALCMS-NOD to land class type of Water if No-data is included in lc_type
fid = np.where(lc_type == 'No-data')[0]
if (fid.size != 0):
    r1 = np.where(lc_type == 'Water')[0]
    r2 = np.where(lc_type == 'No-data')[0]  
    print(r1,r2)
    # adding the nodata values to the water land cover type and drop it and remove from lc_type 
    lc_frac.values[:,r1] = lc_frac.values[:,r1] + lc_frac.values[:,r2]
    lc_frac = lc_frac.drop(lc_frac.columns[r2], axis=1)
    lc_type = np.delete(lc_type, r2)

# add Dump layer for MESH application
lc_frac['Dump'] = 0

# calculating land cover percentage. Only calculate if input zonal histogram is a shapefile (i.e. QGIS version)
if str(input_lc_zh).endswith('.shp'):
    lc_frac = lc_frac.apply(lambda x: round(x/x.sum(),2), axis=1)

# %% convert the lc_frac as a dataset and save it as netcdf
lon = drainage_db['lon'].values
lat = drainage_db['lat'].values
tt = drainage_db['time'].values

lc_ds =  xs.Dataset(
    {
        "GRU": (["subbasin", "gru"], lc_frac.values),
        "LandUse": (["gru"], lc_type),
    },
    coords={
        "lon": (["subbasin"], lon),
        "lat": (["subbasin"], lat),
        "time": tt,
    },
)

# meta data attributes 
lc_ds.attrs['Conventions'] = 'CF-1.6'
lc_ds.attrs['License']     = 'The data were written by Ala Bahrami'
lc_ds.attrs['history']     = 'Created on April 23, 2021'
lc_ds.attrs['featureType'] = 'point'          

# editing lat attribute
lc_ds['lat'].attrs['standard_name'] = 'latitude'
lc_ds['lat'].attrs['units'] = 'degrees_north'
lc_ds['lat'].attrs['axis'] = 'Y'
 
# editing lon attribute
lc_ds['lon'].attrs['standard_name'] = 'longitude'
lc_ds['lon'].attrs['units'] = 'degrees_east'
lc_ds['lon'].attrs['axis'] = 'X'

# editing time attribute
lc_ds['time'].attrs.update(standard_name = 'time', 
                                 units = ('days since %s 00:00:00' % date.today().strftime('%Y-%m-%d')), 
                                 axis = 'T')

# coordinate system
lc_ds['crs'] = drainage_db['crs'].copy()

## made changes here 
#lc_ds['GRU'].values = lc_ds['GRU'].values[reorder,:]

# %% Append land cover information to existing drainage database 
drainage_db["GRU"] = (["subbasin", "gru"], lc_frac.values)
drainage_db['GRU'].attrs['standard_name'] = 'GRU'
drainage_db['GRU'].attrs['long_name'] = 'Group Response Unit'
drainage_db['GRU'].attrs['units'] = '-'
drainage_db['GRU'].attrs['_FillValue'] = -1

drainage_db["LandUse"] = (["gru"], lc_type)

# Set the 'coords' of the dataset to the new axes.
drainage_db = drainage_db.set_coords(['time', 'lon', 'lat'])

# saved the drainage_database
outDDB = domain_name+'_MESH_drainage_database.nc'
drainage_db.to_netcdf(outdir/outDDB)
 
# %% Save land cover fraction (this is optional)
outFRAC = domain_name+'_MESH_LC_FRAC.nc'
lc_ds.to_netcdf(outdir/outFRAC)
print('--%s seconds--' %(time.time() - start_time))


# --- Code provenance
# Generates a basic log file in the domain folder and copies the control file and itself there.
 
# Set the log path and file name
logPath = outdir
log_suffix = '_basin_subset.txt'
 
# Create a log folder
logFolder = '_workflow_log'
Path( logPath / logFolder ).mkdir(parents=True, exist_ok=True)
 
# Copy this script
thisFile = 'easymore_basinsubset.py'
copyfile(thisFile, logPath / logFolder / thisFile);
 
# Get current date and time
now = datetime.now()
 
# Create a log file
logFile = now.strftime('%Y%m%d') + log_suffix
with open( logPath / logFolder / logFile, 'w') as file:
     
    lines = ['Log generated by ' + thisFile + ' on ' + now.strftime('%Y/%m/%d %H:%M:%S') + '\n',
             'Generated drainage database .nc file.']
    for txt in lines:
        file.write(txt)