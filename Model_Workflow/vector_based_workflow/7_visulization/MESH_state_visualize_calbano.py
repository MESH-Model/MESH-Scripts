# -*- coding: utf-8 -*-
"""
NAME 
    MESH_state_visualize_test
    
Input 
    MESH_state          MESH state varibles that need to be visualized
    Shapefile           A shpae file which matches the drainage database
    
Output     
                        saved plot
    
PURPOSE    
    The purpose of this program is to read the MESH model state and visualize 
    the output along with MeritHydro catchment shape file 
    
PROGRAMMERS: Ala Bahrami
             Cooper Albano

REVISION HISTORY
    20220929 -- Initial version created and posted online 
    20230111 -- Bugfix. Previous version plotted only a single subbasin for most GRUs 
    20230111 -- Added .gif output

REFERENCE

"""

import xarray as xs
import geopandas as gpd
import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import imageio
import os
from pathlib import Path

# Control file handling
# Easy access to control file folder
controlFolder = Path('../0_control_files')

# Store the name of the 'active' file in a variable
controlFile = 'control_active.txt'

#Function to extract a given setting from the control file
def read_from_control( file, setting ):
     
    # Open 'control_active.txt' and ...
    with open(file) as contents:
        for line in contents:
             
            # ... find the line with the requested setting
            if setting in line and not line.startswith('#'):
                break
     
    # Extract the setting's value
    substring = line.split('|',1)[1]      # Remove the setting's name (split into 2 based on '|', keep only 2nd part)
    substring = substring.split('#',1)[0] # Remove comments, does nothing if no '#' is found
    substring = substring.strip()         # Remove leading and trailing whitespace, tabs, newlines
        
    # Return this value   
    return substring

#Function to specify a default path
def make_default_path(suffix):
     
    # Get the root path
    rootPath = Path( read_from_control(controlFolder/controlFile,'root_path') )
     
    # Get the domain folder
    domainName = read_from_control(controlFolder/controlFile,'domain_name')
    #domainFolder = 'domain_' + domainName
     
    # Specify the forcing path
    defaultPath = rootPath / 'domain_{}'.format(domainName) / suffix
     
    return defaultPath

rootPath = Path( read_from_control(controlFolder/controlFile,'root_path') )
domainName = read_from_control(controlFolder/controlFile,'domain_name')
#%% input files
# Get the directory containing the MESH results 
MESH_state_dir           = read_from_control(controlFolder/controlFile,'simulations_path')
if MESH_state_dir == 'default':
    MESH_state_dir = make_default_path('simulations/')
else:
    MESH_state_dir = MESH_state_dir

# Specify the output folder
outdir = read_from_control(controlFolder/controlFile,'visualization_folder')
if outdir == 'default':
    outdir = make_default_path('visualization/')
else:
    outdir = outdir

# Specify the basin shapefile location
Merit_catchment_shape_path = read_from_control(controlFolder/controlFile,'visualization_folder')
if Merit_catchment_shape_path == 'default':
    Merit_catchment_shape_path = make_default_path('shapefiles/catchment/')
else:
    Merit_catchment_shape_path = Merit_catchment_shape_path

Merit_catchment_shape_name = read_from_control(controlFolder/controlFile,'river_basin_shp_name')
Merit_catchment_shape = Merit_catchment_shape_path/Merit_catchment_shape_name

# Read drainage database location
DDB_path = read_from_control(controlFolder/controlFile,'DDB_output_dir')
if DDB_path == 'default':
    DDB_path =  make_default_path('parameters/drainagedatabase/')
else:
    DDB_path = DDB_path

# Specify the visualization mode
mode = read_from_control(controlFolder/controlFile,'vis_mode')
mode_short = mode[0].upper()

soilLayers = read_from_control(controlFolder/controlFile,'soilLayers')

state_name = []
var        = []
# Retrieve the visualization variables
with open(make_default_path('parameters/outputs_balance.txt'),'r') as outputs_balance:
    for i in outputs_balance:
        if "SOL" in i and not '!' in i:
            for j in range(1,int(soilLayers)+1):
                layer = 'IG{}'.format(j)
                variable = i.split(' ')[0]
                state_name.append('{}_{}_GRD.nc'.format(variable,layer,mode_short))
                var.append(variable)

        elif not "!" in i:
            variable = i.split(' ')[0]
            state_name.append('{}_{}_GRD.nc'.format(variable,mode_short))
            var.append(variable)



Output_Variables =  {

                        'variable'      : ['FSIN','FSVH','FSIH','FSDIR','FSDFF','FSOUT','FLIN','FLOUT','TA','QA','PRES','UV','WDIR','UU','VV','PRE','PRERN','PRESNO',
                                           'PREC','PRECRN','PRECSNO','LQWSCAN','FZWSCAN','CMAS','TCAN','GRO','SNO','RHOSNO','ZSNO','FSNO','LQWSSNO','TSNO','DRAINSNO',
                                           'ALBT','ALVS','ALIR','GTE','ZPND','LQWSPND','TPND','POTEVP','ET','EVPB','ARRD','OVRFLW','QEVP','QSENS','GZERO','THLQSOL',
                                           'THICSOL','LQWSSOL','FZWSSOL','ALWSSOL','TSOL','GFLX','LATFLW','DRAINSOL','RCHG','STGGW','DZS','STGW','DSTGW','STGE','DSTGE',
                                           'RFF','ROF','QI','QO','STGCH','ZLVL'],
                        'Description'   : ['Incoming shortwave radiation at the surface','Visible component of incoming shortwave radiation at the surface.','Infrared component of incoming shortwave radiation at the surface.',
                                           'Direct component of incoming shortwave radiation at the surface.','Diffuse component of incoming shortwave radiation at the surface.','Outgoing shortwave radiation at the surface.',
                                           'Incoming longwave radiation at the surface.','Outgoing longwave radiation at the surface.','Air temperature (at user-specified reference height).','Specific humidity (at user-specificed reference height).',
                                           'Air pressure at the surface.','Wind speed (at user-specified reference height).','Wind direction (at user-specified referenced height).','U-component of wind speed (at user-specified reference height).',
                                           'V-component of wind speed (at user-specified reference height).','Total incoming precipitation rate.','Total incoming liquid precipitation rate.','Total incoming solid precipitation rate.',
                                           'Total precipitation.','Total liquid precipitation.','Total solid precipitation.','Liquid water interception in the canopy.','Frozen water interception in the canopy.','Organic mass of the canopy.',
                                           'Vegetation canopy temperature.','Vegetation growth index.','Snow mass.','Snow density.','Snow depth.','Fraction of fully snow covered area.','Liquid water content of the snow.','Snowpack temperature.',
                                           'Drainage (runoff rate) from the bottom of the snowpack (see notes here)','Total albedo of the surface (visible and near-infrared).','Visible component of the total albedo of the surface.',
                                           'Near-infrared components of the total albedo of the surface.','Effective black-body temperature at the surface.','Depth of ponded water.','Liquid water storage of ponded water.','Temperature of ponded water.',
                                           'Potential evaporation.','Evapotranspiration.','Evaporation efficiency (ET to POTEVP) of the canopy.','Arridity index (PRE to POTEVP).','Overland runoff.','Latent heat flux at the surface.','Sensible heat flux at the surface.',
                                           'Heat flux into the ground.','Volumetric liquid water content of the soil.','Volumetric frozen water content of the soil.','Liquid water storage in the soil.','Frozen water storage in the soil.','Total water storage in the soil.',
                                           'Temperature of the soil.','Heat conduction between soil layers.','Interflow runoff.','Drainage from the bottom of the permeable soil column.','Drainage into groundwater/lower zone storage.','Groundwater/lower zone storage.',
                                           'Deep aquifer water storage.','Total water storage (HLSS).','Change in water storage from the previous time-step.','Total energy storage (HLSS).','Change in energy storage from the previous time-step',
                                           'Total runoff (from all surface, subsurface, and groundwater components).','Total runoff (from all surface and subsurface components).','Flow rate entering the channel.','Flow rate leaving the channel (discharge).',
                                           'Channel storage.','Stage level.'],
                        'Units'         : ['W m-2','W m-2','W m-2','W m-2','W m-2','W m-2','W m-2','W m-2','K','kg kg-1','Pa','m s-1','degrees','m s-1','m s-1','kg m-2 s-1','kg m-2 s-1','kg m-2 s-1','kg m-2','kg m-2','kg m-2','kg m-2','kg m-2','kg m-2','K','--','kg m-2','kg m-3',
                                           'm','fraction','kg m-2','K','kg m-2 s-1','fraction','fraction','fraction','K','m','kg m-2','K','kg m-2','kg m-2','--','--','kg m-2','W m-2','W m-2','W m-2','m3 m-3','m3 m-3','kg m-2','kg m-2','kg m-2','K','W m-2','kg m-2','kg m-2','mm',
                                           'mm','mm','kg m-2','kg m-2','W m-2','W m-2','mm','kg m-2','m3 s-1','m3 s-1','m3','m']

                    }

df = pd.DataFrame.from_dict(Output_Variables)
df = df.set_index('variable')
print(df.loc['FSIN']['Units'])

# Specify the variables to be visualized
# state_name               = ['STGW_D_GRD.nc','SNO_D_GRD.nc', 'LQWSSNO_D_GRD.nc','LQWSPND_D_GRD.nc',
#                             'LQWSSOL_D_IG1_GRD.nc','LQWSSOL_D_IG2_GRD.nc','LQWSSOL_D_IG3_GRD.nc',
#                             'FZWSSOL_D_IG1_GRD.nc','FZWSSOL_D_IG2_GRD.nc','FZWSSOL_D_IG3_GRD.nc',
#                             'LQWSCAN_D_GRD.nc','FZWSCAN_D_GRD.nc',
#                             'STGGW_D_GRD.nc','RFF_D_GRD.nc']   
# var                      = ['STGW','SNO','LQWSSNO', 'LQWSPND',
#                             'LQWSSOL','LQWSSOL','LQWSSOL','FZWSSOL','FZWSSOL','FZWSSOL', 
#                             'LQWSCAN', 'FZWSCAN',
#                             'STGGW','RFF']
# cbar_tl                  = ['Total Storage','Snow Mass [mm]', 'Liquid water content of the snow [mm]','Liquid water storage of ponded water [mm]',
#                             'Liquid water storage in the soil [mm]','Liquid water storage in the soil [mm]','Liquid water storage in the soil [mm]',
#                             'Frozen water storage in the soil [mm]','Frozen water storage in the soil [mm]','Frozen water storage in the soil [mm]',
#                             'Liquid water interception in the canopy [mm]', 'Frozen water interception in the canopy [mm]',
#                             'Groundwater zone storage [mm]','Total runoff [mm]']

#%% plot style 
cmaps = ['viridis','Blues_r', 'cividis', 'seismic_r']
font = {'family' : 'Times New Roman',
         'weight' : 'bold',
         'size'   : 24}
matplotlib.rc('font', **font)

#%% reading the inputs 
# reading MESH drainage database
db = xs.open_dataset(str(DDB_path) +'/'+ domainName+'_MESH_drainage_database.nc')
db.close()

# extracting SEGIDs     
segid       = db['seg_id'].values

#%% reading source shape file 
shp = gpd.read_file(Merit_catchment_shape) 
#shp = shp[shp['COMID'].isin(df ['ID'])]
shp = shp.sort_values(by=['COMID'])
shp = shp.reset_index(drop=True)

df          = pd.DataFrame()
df['ID']    = segid
filenames =[]

N = len(var)
for date in range(0,6): # Do for each day
    for i in range(N): # Do for each variable
        state = xs.open_dataset(MESH_state_dir+state_name[i])
        state.close()
        
        subbasins = int(state.dims['lat'])
        state_var = []
        for j in range(0,subbasins): # Do for each subbasin
            # check condition for soil layers 
            if 'IG1' in state_name[i]:
                ly = 'Layer1'
            elif 'IG2' in state_name[i]:
                ly = 'Layer2'
            elif 'IG3' in state_name[i]:
                ly = 'Layer3'
            else:
                ly = []
            value = state[var[i]][date][0][j].values
            state_var.append(float(value))

            #% visualize MESH output
        fig, ax = plt.subplots(figsize=(20, 20))
        df['value'] = state_var # diplay first monthly data for    
        df = df.sort_values(by=['ID'])
        df = df.reset_index(drop=True)

        shp ['value'] = df ['value']
        mn = np.min(shp ['value'])    
        mx = np.max(shp ['value'])


        if (len(ly) != 0):
            tl = 'BowBanff'+'_MESH_'+var[i]+'_'+ly+'_'+str(state[var[i]][date]['time'].values).split('T')[0]
        else:
            tl = 'BowBanff'+'_MESH_'+var[i]+'_'+str(state[var[i]][date]['time'].values).split('T')[0]

        ax.set_title(tl)
        ax.set_xlabel('Longitude [degree east]')
        ax.set_ylabel('Latitude [degree north]')

        # create the colorbar
        norm = colors.Normalize(vmin=mn,vmax=mx)            

        # NB: when the mn and mx are equal (e.g., mn = mx = 0), the normalization is not required 
        if (mn != mx):
            cbar = plt.cm.ScalarMappable(norm=norm, cmap=cmaps[0])
        else:
            cbar = plt.cm.ScalarMappable(cmap=cmaps[0])

        shp.plot(column='value', cmap=cmaps[0], edgecolor='k',linewidth=0.1,ax = ax, vmin = mn, vmax = mx)
        # add colorbar and its label
        ax_cbar = fig.colorbar(cbar, ax=ax, extend='max') 
        cbar.set_clim(0,22) 
        # ax_cbar.set_label(cbar_tl[i])
        ax_cbar.set_label(df.loc[var[i]]['Units'])
        if not os.path.exists(outdir+var[i]):
            os.mkdir(outdir+var[i])
        #plt.clim(0,24)
        plt.savefig(outdir+var[i]+'/'+tl+'_{}_state.png'.format(mode), format='png', dpi=100)
        plt.close()
        
        filenames.append(tl+'_{}_state.png'.format(mode))



from PIL import Image
import glob

for i in var:
    # Create the frames
    frames = []
    imgs = glob.glob('{}{}/*.png'.format(outdir,i),recursive=True)

    for j in imgs:
        new_frame = Image.open(j)
        frames.append(new_frame)
 
    # Save into a GIF file that loops forever
    frames[0].save('{}{}.gif'.format(outdir+i+'/',i), format='GIF',
                append_images=frames[1:],
                save_all=True,
                duration=300, loop=0)
    