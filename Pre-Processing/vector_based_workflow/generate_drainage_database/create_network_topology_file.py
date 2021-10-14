"""
The purpose of this script is to calculate land cover fractions for each
subbasin of interest. Then the landcover fractions are converted the (subbasin*lc_types) and
they are appended to the driange database.
 
@ Author : MESHworkflow
 
"""
 
# %% importing modules
import geopandas as gpd
import matplotlib.pyplot as plt
import numpy as np
import xarray as xs
from   datetime import date
 
# %% directory of input files
in_lc       = '../../domain_name_intersect_NGRU.shp'
input_ddb   = '../../Input/network_topology.nc'
out_lc      = '../../MESH_LC_FRAC.nc'
output_ddb  = '../../MESH_drainage_database.nc'
 
 
#%% Function reindex to extract drainage database variables
def new_rank_extract(in_ddb):
        #% reading the input DDB
        drainage_db = xs.open_dataset(in_ddb)
        drainage_db.close()
         
        # Count the number of outlets
        outlets = np.where(drainage_db['tosegment'].values == 0)[0]
         
        # %  Re-indexing seg_id and tosegment
        # Get the segment ID associated with the outlet.
        first_index = drainage_db['seg_id'].values[outlets[0]]
          
        # Create a copy of the 'tosegment' field.
        old_next = drainage_db['tosegment'].values.copy()
          
        ## Set the current 'Next' and 'Rank' values.
        # total number of values
        current_next = len(drainage_db['seg_id'])
        # total number of values less number of outlets
        current_rank = current_next - len(outlets)
          
        ## Create dummy arrays for new values.
        # size of 'seg_id'
        new_next = [0]*len(drainage_db['seg_id'])
        # empty list (to push values to)
        next_rank = []
        # list to append positions of new 'rank', first element is position of outlet
        new_rank = [outlets[0]]
         
        # % Reorder seg_id and tosegment
        while (first_index != -1):
            for i in range(len(old_next)):
                if (old_next[i] == first_index):
                    # save rank of current 'next'
                    next_rank.append(drainage_db['seg_id'].values[i])
                    # assign next using new ranking
                    new_next[i] = current_next
                    # save the current position corresponding to the new 'rank'
                    new_rank.append(i)
                    current_rank -= 1
                    # override input value to mark as completed
                    old_next[i] = 0
                    break
            if (len(next_rank) == 0):
                    # no more IDs to process
                    first_index = -1
            elif (not np.any(old_next == first_index)):
                # take next rank by 'next' order
                first_index = next_rank[0]
                # drop that element from the list
                del next_rank[0]
                # deincrement the 'next' rank
                current_next -= 1
 
        new_rank = np.flip(new_rank)
         
        # % reordering
        for m in ['basin_area', 'length', 'slope', 'lon', 'lat', 'hruid',
                  'seg_id', 'seg_hr_id', 'tosegment', 'width', 'manning']:
            drainage_db[m].values = drainage_db[m].values[new_rank]
  
        # Reorder the new 'Next'.
        new_next = np.array(new_next)[new_rank]
         
        # % check if channel slope values match the minimum threshold
        min_slope = 0.000001
        drainage_db['slope'].values[drainage_db['slope'].values < min_slope] = min_slope
         
        # % Adding the updated Rank and Next variables to the file
        drainage_db['Rank'] = (['n'], np.array(range(1, len(new_rank) + 1),
                              dtype = 'int32')) # ordered list from 1:NA
        drainage_db['Rank'].attrs.update(standard_name = 'Rank',
                            long_name = 'Element ID', units = '1', _FillValue = -1)
         
        drainage_db['Next'] = (['n'], new_next.astype('int32')) # reordered 'new_next'
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
        drainage_db['time'] = (['subbasin'], np.zeros(len(new_rank)))
        drainage_db['time'].attrs.update(standard_name = 'time', units = ('days since %s 00:00:00' % date.today().strftime('%Y-%m-%d')), axis = 'T')
          
        # Set the 'coords' of the dataset to the new axes.
        drainage_db = drainage_db.set_coords(['time', 'lon', 'lat'])
          
        # Add (or overwrite) the 'featureType' to identify the 'point' dataset.
        drainage_db.attrs['featureType'] = 'point'
         
        return new_rank, drainage_db
 
# %% calling the new_rank_extract
new_rank, drainage_db = new_rank_extract(input_ddb)
 
# %% reading the input landcover
lc = gpd.read_file(in_lc)
 
# showing the lc
fig, ax1 = plt.subplots(1,1, figsize = (10, 10))
lc.plot(ax = ax1, facecolor = 'None', edgecolor = 'red')
 
# %% land class types
 
# N-GRU
# Note : this section is hard-coded and it should be modified based on land cover types for a region of interest
lc_type = ['needleleaf-forest','broadleaf-forest','Mixed-forest','shrubland','grassland','Lichen-moss',
           'Wetland','Cropland','Barrenland','Urban','Water','SnowIce']
 
# %% verify list of lc types
m = len(lc_type) + 1
st = [];
p  = [];
for i in (range(1,m)):
    st1 =   'NALCMS'+'_'+ str(i)
    st = np.append(st, st1)
    fid = np.where(lc.columns == st1)[0]
    if (fid.size == 0):
        print ('land cover %s is not the list of extracted histogram' % lc_type[i-1])
        # this should be modified if mutiple lc types are not in the list
        p = i-1
         
# adding lancover dump
st1 =   'NALCMS'+'_'+ 'Dump'
st = np.append(st, st1)
 
if (len(p) != 0) :
    del lc_type[p]
 
lc_type = np.append(lc_type, 'Dump') 
 
# %% extract indices of lc based on the drainage database
n = len(drainage_db.hruid)
ind = []
hruid =  drainage_db.variables['hruid']
 
for i in range(n):
    fid = np.where(np.int32(lc['COMID'].values) == hruid[i].values)[0]
    ind = np.append(ind, fid)
 
ind = np.int32(ind)  
 
#%% calculate fractions
# note: the column 2 to 2+m-1 is hard-coded. Users should check before calculation
lc_frac = lc.values[ind , 2 : 2+m-1]
 
lc_frac = np.append(lc_frac, np.zeros((n , 1)) , axis = 1)
 
lc_sum = np.sum(lc_frac, axis = 1)
lc_sum = np.transpose(np.tile(lc_sum, (m,1)))
lc_frac = np.divide(lc_frac, lc_sum)
 
# %% convert the lc_frac as a dataset and save it as netcdf
lon = drainage_db['lon'].values
lat = drainage_db['lat'].values
tt = drainage_db['time'].values
 
lc_ds =  xs.Dataset(
    {
        "GRU": (["subbasin", "gru"], lc_frac),
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
 
# %% Append land cover information to existing drainage database
drainage_db["GRU"] = (["subbasin", "gru"], lc_frac)
drainage_db['GRU'].attrs['standard_name'] = 'GRU'
drainage_db['GRU'].attrs['long_name'] = 'Group Response Unit'
drainage_db['GRU'].attrs['units'] = '-'
drainage_db['GRU'].attrs['_FillValue'] = -1
 
drainage_db["LandUse"] = (["gru"], lc_type)
 
# Set the 'coords' of the dataset to the new axes.
drainage_db = drainage_db.set_coords(['time', 'lon', 'lat'])
 
# saved the drainage_database
drainage_db.to_netcdf(output_ddb)
 
# %% Save land cover fraction (this is optional)
lc_ds.to_netcdf(out_lc)