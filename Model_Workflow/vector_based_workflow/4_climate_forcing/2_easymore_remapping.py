#!/cvmfs/soft.computecanada.ca/easybuild/software/2020/avx2/Core/python/3.8.10/bin/python
# which python to find the location of installed pyhthon on graham

# loading EASYMORE
from easymore.easymore import easymore

# initializing EASYMORE object
esmr = easymore()
# specifying EASYMORE objects
# name of the case; the temporary, remapping and remapped file names include case name
esmr.case_name                =  'RDRS_BowAtBanff'
# temporary path that the EASYMORE generated GIS files and remapped file will be saved
esmr.temp_dir                 =  'temporaryBowAtBanff/'
# name of target shapefile that the source netcdf files should be remapped to
esmr.target_shp               =  '../shape_file/catchment/BowAtBanff.shp'
                                #'../shape_file/cat_pfaf_81_MERIT_Hydro_v07_Basins_v01_bugfix1_WGS84.shp'
esmr.target_shp_ID            =  'COMID'
# name of netCDF file(s); multiple files can be specified with *
esmr.source_nc                = '../forcing/rdrsv2.1_1980.nc' 
                                #'/project/6008034/baha2501/NA/forcing/NA_RDRS_v2.1_1980010112.nc'
                                
# name of variables from source netCDF file(s) to be remapped
esmr.var_names                = ['RDRS_v2.1_A_PR0_SFC', 'RDRS_v2.1_P_FI_SFC',
                                  'RDRS_v2.1_P_FB_SFC','RDRS_v2.1_P_TT_09944',
                                  'RDRS_v2.1_P_UVC_09944','RDRS_v2.1_P_P0_SFC',
                                  'RDRS_v2.1_P_HU_09944']
# rename the variables from source netCDF file(s) in the remapped files;
# it will be the same as source if not provided
#esmr.var_names_remapped       = ['RDRS']
# name of variable longitude in source netCDF files
esmr.var_lon                  = 'lon'
# name of variable latitude in source netCDF files
esmr.var_lat                  = 'lat'
# name of variable time in source netCDF file; should be always time
esmr.var_time                 = 'time'
# location where the remapped netCDF file will be saved
esmr.output_dir               = 'OutputBowAtBanff/'
# format of the variables to be saved in remapped files,
# if one format provided it will be expanded to other variables
esmr.format_list              = ['f4']
# fill values of the variables to be saved in remapped files,
# if one value provided it will be expanded to other variables
esmr.fill_value_list          = ['-9999.00']
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
print(shp)
shp = shp [shp['lon_s']>-179]
shp.to_file(esmr.temp_dir+esmr.case_name+'_source_shapefile.shp')

# add the source shapefile 
esmr.source_shp                =   esmr.temp_dir+esmr.case_name+'_source_shapefile.shp'
esmr.source_shp_lat            =  'lat_s' # name of column latitude in the source shapefile
esmr.source_shp_lon            =  'lon_s' # name of column longitude in the source shapefile
esmr.source_shp_ID             =  'ID_s' # name of column ID in the source shapefile

# execute EASYMORE
esmr.nc_remapper()