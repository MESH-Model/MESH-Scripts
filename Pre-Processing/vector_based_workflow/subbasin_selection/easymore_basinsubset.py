#%% load modules
from easymore.easymore import easymore
import geopandas as gpd
import numpy as np
 
#%% assigning input files
# Note : this is an example for the Fraser setup
input_basin = '../cat_pfaf_78_MERIT_Hydro_v07_Basins_v01_bugfix1.shp'
input_river = '../rivEndoMERITpfaf_78.shp'
 
outdir_basin = '../Basin_Boundary/'
outdir_river = '../Streamflow/'
 
#%% initializing easymore object
esmr = easymore()
# load the files and calculating the downstream of each segment
riv  = gpd.read_file(input_river)
cat    = gpd.read_file(input_basin)
# get all the upstream
seg_IDs  = np.array(riv.COMID)
down_IDs = np.array(riv.NextDownID)
NTOPO    = esmr.get_all_downstream (seg_IDs,down_IDs)
 
#%% identify target segment ID
esmr.case_name = 'FRASER_08MH126'
target_segment = 78011863
 
up_subbasins = esmr.get_all_upstream(target_segment,NTOPO) # segment ID
# subset
cat_up = cat.loc[cat['COMID'].isin(up_subbasins)]
riv_up = riv.loc[riv['COMID'].isin(up_subbasins)]
# plot
cat_up.plot()
riv_up.plot()
# save
cat_up.to_file(outdir_basin+esmr.case_name+'_cat.shp')
riv_up.to_file(outdir_river+esmr.case_name+'_riv.shp')