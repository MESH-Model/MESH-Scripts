"""
@ Author : MESHworkflow
 
"""
# %% import module
import processing
# %% run the algorithm
# WARNING: If the paths below are too long, the python console will produce an error. You may need to temporarily store files in such a way that paths are shorter.
input_raster = 'C:/Users/<user>/path/to/MESH-Scripts/Model_Workflow/vector_based_workflow/workflow_data/domain_BowAtBanff/NA_NALCMS_2010_v2_land_cover_30m.tif'
input_shape  = 'C:/Users/<user>/path/to/MESH-Scripts/Model_Workflow/shape_file/catchment/BowAtBanff_cat_fg.shp'
output_shpae = 'C:/Users/<user>/path/to/MESH-Scripts/Model_Workflow/vector_based_workflow/workflow_data/domain_BowAtBanff/zonalhist/BowAtBanff_lczh.shp'
band           =  1
 
processing.run("native:zonalhistogram" , {'COLUMN_PREFIX' : 'NALCMS_',
            'INPUT_RASTER' : input_raster,
            'INPUT_VECTOR' : input_shape,
            'OUTPUT'       : output_shpae,
            'RASTER_BAND'  : band })