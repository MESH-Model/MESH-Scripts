"""   
REFERENCE
https://github.com/ShervanGharari/EASYMORE   
"""
#%% load module
from easymore.easymore import easymore
import os
import numpy as np
import xarray as xs
import pandas as pd
import sys
 
# %% initializing EASYMORE object
esmr = easymore()
 
# specifying EASYMORE objects
# name of the case; the temporary, remapping and remapped file names include case name            
esmr.case_name                 = 'PFAF78'             
# temporary path that the EASYMORE generated GIS files and remapped file will be saved
esmr.temp_dir                 = 'temporary78_{}/'.format(sys.argv[1])
# name of target shapefile that the source netcdf files should be remapped to
esmr.target_shp               = '/project/6008034/Model_Output/MESH/NA_workflow/shape_file/catchment/cat_pfaf_78_MERIT_Hydro_v07_Basins_v01_bugfix1_WGS84.shp'
 
# name of netCDF file(s); multiple files can be specified with *
#esmr.source_nc                 =  '/scratch/calbano/rdrs_output/1980/rdrsv2.1_1980_01.nc'
esmr.source_nc                 =  '/scratch/calbano/rdrs_output/{}/rdrsv2.1_{}.nc'.format(sys.argv[1],sys.argv[1])
 
# note : You can include forcing variables together if they are combined in one file or considere them separately
# name of variables from source netCDF file(s) to be remapped
esmr.var_names                 = ['RDRS_v2.1_A_PR0_SFC', 'RDRS_v2.1_P_FI_SFC',
                                  'RDRS_v2.1_P_FB_SFC','RDRS_v2.1_P_TT_09944',
                                  'RDRS_v2.1_P_UVC_09944','RDRS_v2.1_P_P0_SFC',
                                  'RDRS_v2.1_P_HU_09944']
 
#esmr.var_names                 = ['RDRS_v2_P_HU_09944']
# rename the variables from source netCDF file(s) in the remapped files;
# it will be the same as source if not provided
# esmr.var_names_remapped      = ['PR','RDRS_v2_P_FI_SFC','FB','RDRS_v2_P_TT_09944','UV','RDRS_v2_P_P0_SFC','HU']
#esmr.var_names_remapped        = ['RDRS_v2_P_HU_09944']
 
# name of variable longitude in source netCDF files
esmr.var_lon                  = 'lon'
# name of variable latitude in source netCDF files
esmr.var_lat                  = 'lat'
# name of variable time in source netCDF file; should be always time
esmr.var_time                 = 'time'
# location where the remapped netCDF file will be saved
esmr.output_dir               = '/scratch/calbano/rdrs_output/{}/'.format(sys.argv[1])
# format of the variables to be saved in remapped files,
# if one format provided it will be expanded to other variables
esmr.format_list              = ['f4']
# fill values of the variables to be saved in remapped files,
# if one value provided it will be expanded to other variables
esmr.fill_value_list          = ['-9999.00']
# if required that the remapped values to be saved as csv as well
esmr.save_csv                 = False
# if uncommented EASYMORE will use this and skip GIS tasks
# esmr.remap_csv                = '../temporary/RDRS_Medicine_Hat_remapping.csv'
 
#%% execute EASYMORE
# Note:  remapped forcing has the precision of float32
esmr.nc_remapper()