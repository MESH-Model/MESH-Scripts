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
    
PROGRAMMER: Ala Bahrami

REVISION HISTORY
    20220929 -- Initial version created and posted online 

REFERENCE

"""
#%% load modules 
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

# control file handling
# access the control file folder
controlFolder = Path('../0_control_files')

# Store the name of the 'active' file in a variable
controlFile = 'control_active.txt'

# function to extract a given setting from the control file
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

# function to specif a default path
def make_default_path(suffix):
     
    # Get the root path
    rootPath = Path( read_from_control(controlFolder/controlFile,'root_path') )
     
     
    # Specify the forcing path
    defaultPath = rootPath / suffix
     
    return defaultPath

# get the domain name
rootPath = Path( read_from_control(controlFolder/controlFile,'root_path') )
domainName = read_from_control(controlFolder/controlFile,'domain_name')

# get the inputs
# Get the directory containing the MESH results 
MESH_state_dir           = read_from_control(controlFolder/controlFile,'simulation_path')
if MESH_state_dir == 'default':
    MESH_state_dir = make_default_path('vector_based_workflow/6_model_runs/results/')
else:
    MESH_state_dir = MESH_state_dir

# Specify the output folder
outdir = read_from_control(controlFolder/controlFile,'visualization_folder')
if outdir == 'default':
    outdir = make_default_path('vector_based_workflow/workflow_data/domain_{}/visualizations/'.format(domainName))
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
    DDB_path =  make_default_path('vector_based_workflow/workflow_data/domain_{}/drainagedatabase/'.format(domainName))
else:
    DDB_path = DDB_path

# Specify the visualization mode
mode = read_from_control(controlFolder/controlFile,'vis_mode')
mode_short = mode[0].upper()

soilLayers = read_from_control(controlFolder/controlFile,'soilLayers')

state_name = []
var        = []

# retrieve the visualization variables
with open(make_default_path('vector_based_workflow/6_model_runs/outputs_balance.txt'),'r') as outputs_balance:
    for i in outputs_balance:
        if "SOL" in i and not '!' in i:
            for j in range(1,3+1): # 1 to number of soil layers +1
                layer = 'IG{}'.format(j)
                variable = i.split(' ')[0]
                state_name.append('{}_{}_GRD.nc'.format(variable,layer,mode_short))
                var.append(variable)

        elif not "!" in i:
            variable = i.split(' ')[0]
            state_name.append('{}_{}_GRD.nc'.format(variable,mode_short))
            var.append(variable)

# specify the nariables to be visualized
state_name               = ['STGW_D_GRD.nc','SNO_D_GRD.nc', 'LQWSSNO_D_GRD.nc','LQWSPND_D_GRD.nc',
                            'LQWSSOL_D_IG1_GRD.nc','LQWSSOL_D_IG2_GRD.nc','LQWSSOL_D_IG3_GRD.nc',
                            'FZWSSOL_D_IG1_GRD.nc','FZWSSOL_D_IG2_GRD.nc','FZWSSOL_D_IG3_GRD.nc',
                            'LQWSCAN_D_GRD.nc','FZWSCAN_D_GRD.nc',
                            'STGGW_D_GRD.nc','RFF_D_GRD.nc']   
var                      = ['STGW','SNO','LQWSSNO', 'LQWSPND',
                            'LQWSSOL','LQWSSOL','LQWSSOL','FZWSSOL','FZWSSOL','FZWSSOL', 
                            'LQWSCAN', 'FZWSCAN',
                            'STGGW','RFF']
cbar_tl                  = ['Total Storage','Snow Mass [mm]', 'Liquid water content of the snow [mm]','Liquid water storage of ponded water [mm]',
                            'Liquid water storage in the soil [mm]','Liquid water storage in the soil [mm]','Liquid water storage in the soil [mm]',
                            'Frozen water storage in the soil [mm]','Frozen water storage in the soil [mm]','Frozen water storage in the soil [mm]',
                            'Liquid water interception in the canopy [mm]', 'Frozen water interception in the canopy [mm]',
                            'Groundwater zone storage [mm]','Total runoff [mm]']

# Plot style
cmaps = ['viridis','Blues_r', 'cividis', 'seismic_r']
font = {'family' : 'Times New Roman',
         'weight' : 'bold',
         'size'   : 24}
matplotlib.rc('font', **font)

#reading inputs
db = xs.open_dataset(str(DDB_path) +'/'+ domainName+'_MESH_drainage_database.nc')
db.close()

# extracting seg_ids
segid       = db['seg_id'].values

# reading the source shapefile
shp = gpd.read_file(Merit_catchment_shape) 
shp = shp.sort_values(by=['COMID'])
shp = shp.reset_index(drop=True)

df          = pd.DataFrame()
df['ID']    = segid
filenames =[]

# create plots
N = len(var)
for date in range(0,5): # Do for each day
    for i in range(N): # Do for each variable
        state = xs.open_dataset(MESH_state_dir/state_name[i])
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
        ax_cbar.set_label(cbar_tl[i])

        if not os.path.exists(outdir/var[i]):
            os.mkdir(outdir/var[i])
        #plt.clim(0,24)
        plt.savefig(outdir/var[i]/'{}_{}_state.png'.format(tl,mode), format='png', dpi=100)
        plt.close()
        
        filenames.append('{}_{}_state.png'.format(tl,mode))
