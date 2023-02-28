"""
@ Author : MESHworkflow
 
"""
# %% import module
import processing
# %% run the algorithm
input_raster = 'C:/Users/<user>/path/to/landcover/NA_NALCMS_landcover_2015v2_30m.tif'
input_shape  = 'C:/Users/<user>/path/to/MESH-Scripts/Model_Workflow/shapefiles/catchment/BowAtBanff_cat_fixgeo.shp'
output_shpae = 'C:/Users/<user>/path/to/MESH-Scripts/Model_Workflow/vector_based_workflow/workflow_data/domain_BowAtBanff/zonalhist/BowAtBanff_lczh.shp'
band           =  1
processing.run("native:zonalhistogram" , {'COLUMN_PREFIX' : 'NALCMS_','INPUT_RASTER' : input_raster,'INPUT_VECTOR' : input_shape,'OUTPUT'       : output_shpae,'RASTER_BAND'  : band })