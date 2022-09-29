"""
@ Author : MESHworkflow
"""
 
# %% impporting module
import os
import numpy as np
import xarray as xs
import pandas as pd
 
#%% define I/O dirs
input_forc  = '../domain_[name]_remapped_*.nc'
output_forc = '../domain_[name]_remapped_RDRSV2_input_200001_201801.nc'
 
# %% combining forcing variables 
ds = xs.open_mfdataset(input_forc, combine = 'by_coords', concat_dim="time", data_vars="minimal")
 
# %% write the output to a netcdf format
ds.to_netcdf(output_forc)
