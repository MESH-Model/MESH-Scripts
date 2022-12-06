"""
@Author: MESHworkflow
"""
# %% load modules
import os
import numpy as np
import xarray as xs
import pandas as pd
import geopandas as gpd
import sys
 
# %% define input file
input_forcing  = '../../easymore_remapping/Output{}/{}/PFAF{}_remapped_{}.nc'.format(sys.argv[2],sys.argv[1],sys.argv[2],sys.argv[1])
input_db       = '../workflow_data/domain_PFAF{}/drainagedatabase/PFAF{}_MESH_drainage_database.nc'.format(sys.argv[2],sys.argv[2])
inut_basin     = '../../shape_file/catchment/cat_pfaf_{}_MERIT_Hydro_v07_Basins_v01_bugfix1_WGS84.shp'.format(sys.argv[2])
output_forcing = '../../easymore_remapping/Output{}/{}/PFAF{}_rRemapped_RDRSV2.1_{}.nc'.format(sys.argv[2],sys.argv[1],sys.argv[2],sys.argv[1])

# %% reading input basin
basin = gpd.read_file(inut_basin)
print('Basin: {}'.format(inut_basin)) 
# %% reading input netcdf files db
db = xs.open_dataset(input_db)
db.close()
print('DDB: {}'.format(input_db))
hruid =  db.variables['hruid'].values
# reading for control check
lon = db.variables['lon'].values
lat = db.variables['lat'].values
 
# %% reading input forcing
forc = xs.open_dataset(input_forcing)
forc.close()
print('Forcing: {}'.format(input_forcing))
lon_ease = forc.variables['longitude'].values
lat_ease = forc.variables['latitude'].values

# %% extract indices of forcing ids based on the drainage database
n = len(basin)
ind = []
 
for i in range(n):
    fid = np.where(np.int32(basin['COMID'].values) == hruid[i])[0]
    ind = np.append(ind, fid)
 
ind = np.int32(ind) 
 
# %% reorder input forcing
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
forc_vec.to_netcdf(output_forcing)