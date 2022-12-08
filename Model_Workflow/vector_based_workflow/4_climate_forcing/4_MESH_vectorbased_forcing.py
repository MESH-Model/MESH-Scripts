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
#import pandas as pd
#import geopandas as gpd 

# %% define input file 
start_time = time.time()
forcing_dir     = '../../easymore_remapping/Output78/'
forcing_name    = 'RDRS_78_remapped_1980-0112-13-00-00.nc'
domain_name     = 'PFAF78' 
outdir          = '../Output/NA/DDB/'

# %% reading input basin 
# the segids are stored in the remapped forcing, so it is not necessary to read input shape file
#basin = gpd.read_file(inut_basin)

# %% reading input netcdf files db
db = xs.open_dataset(outdir+domain_name+'_MESH_drainage_database.nc')
db.close()
segid =  db.variables['seg_id'].values
# reading for control check 
lon = db.variables['lon'].values
lat = db.variables['lat'].values

# %% reading input forcing 
forc = xs.open_dataset(forcing_dir+forcing_name)
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
        "RDRS_v2.1_A_PR0_SFC": (["subbasin", "time"], forc['RDRS_v2.1_A_PR0_SFC'].values[:,ind].transpose()),
    },
    coords={
        "time": forc['time'].values.copy(),
        "lon": (["subbasin"], lon),
        "lat": (["subbasin"], lat),
    }
    )

forc_vec['RDRS_v2.1_A_PR0_SFC'].encoding['coordinates'] = 'time lon lat'
forc_vec['RDRS_v2.1_A_PR0_SFC'].attrs["units"]          = forc['RDRS_v2.1_A_PR0_SFC'].units
forc_vec['RDRS_v2.1_A_PR0_SFC'].attrs["grid_mapping"]   = 'crs'

for n in ['RDRS_v2.1_P_FI_SFC','RDRS_v2.1_P_FB_SFC','RDRS_v2.1_P_TT_09944',
          'RDRS_v2.1_P_UVC_09944','RDRS_v2.1_P_P0_SFC','RDRS_v2.1_P_HU_09944']:
    forc_vec[n] = (("subbasin", "time"), forc[n].values[: , ind].transpose()) 
    forc_vec[n].coords["time"]          = forc['time'].values.copy()
    forc_vec[n].coords["lon"]           = (["subbasin"], lon)
    forc_vec[n].coords["lat"]           = (["subbasin"], lat)
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
forc_vec.to_netcdf(forcing_dir+domain_name+'_MESH_'+forcing_name,encoding=encoding)
print('--%s seconds--' %(time.time() - start_time))
