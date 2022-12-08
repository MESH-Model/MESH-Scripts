"""
@ Author : MESHworkflow
 
"""
# %% import module
import processing
 
# %% run the algorithm
input_raster = '../../landcover.tif'
input_shape  = '../../MERITHydro_cat_fixgeo.shp'
output_shpae = '../../domain_name_intersect_NGRU.shp'
band           =  1
 
processing.run("native:zonalhistogram" , {'COLUMN_PREFIX' : 'NALCMS_',
            'INPUT_RASTER' : input_raster,
            'INPUT_VECTOR' : input_shape,
            'OUTPUT'       : output_shpae,
            'RASTER_BAND'  : band })