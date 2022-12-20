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
import datetime

#%% input files 
MESH_state_dir           = 'Input/MESH_state/'
state_name               = ['STGW_M_GRD.nc', 'SNO_M_GRD.nc', 'LQWSSNO_M_GRD.nc','LQWSPND_M_GRD.nc',
                            'LQWSSOL_M_IG1_GRD.nc','LQWSSOL_M_IG2_GRD.nc','LQWSSOL_M_IG3_GRD.nc',
                            'FZWSSOL_M_IG1_GRD.nc','FZWSSOL_M_IG2_GRD.nc','FZWSSOL_M_IG3_GRD.nc',
                            'LQWSCAN_M_GRD.nc','FZWSCAN_M_GRD.nc',
                            'STGGW_M_GRD.nc','RFF_M_GRD.nc']   
var                      = ['STGW','SNO','LQWSSNO', 'LQWSPND',
                            'LQWSSOL','LQWSSOL','LQWSSOL','FZWSSOL','FZWSSOL','FZWSSOL', 
                            'LQWSCAN', 'FZWSCAN',
                            'STGGW','RFF']
cbar_tl                  = ['Total water storage [mm]', 'Snow Mass [mm]', 'Liquid water content of the snow [mm]','Liquid water storage of ponded water [mm]',
                            'Liquid water storage in the soil [mm]','Liquid water storage in the soil [mm]','Liquid water storage in the soil [mm]',
                            'Frozen water storage in the soil [mm]','Frozen water storage in the soil [mm]','Frozen water storage in the soil [mm]',
                            'Liquid water interception in the canopy [mm]', 'Frozen water interception in the canopy [mm]',
                            'Groundwater zone storage [mm]','Total runoff [mm]']
domain_name              = 'BowAtBanff'
outdir                   = 'Output/BowBanff/'
mode                     = 'monthly'
Merit_catchment_shape    = 'Input/Shape/bow_distributed.shp'

#%% plot style 
cmaps = ['viridis','Blues_r', 'cividis', 'seismic_r']
font = {'family' : 'Times New Roman',
         'weight' : 'bold',
         'size'   : 24}
matplotlib.rc('font', **font)

#%% reading the inputs 
# reading MESH drainage database
db = xs.open_dataset('Input/Drainage_database/'+domain_name+'_MESH_drainage_database.nc')
db.close()

# extracting SEGIDs     
segid       = db['seg_id'].values

#% reading source shape file 
shp = gpd.read_file(Merit_catchment_shape) 
#shp = shp[shp['COMID'].isin(df ['ID'])]
shp = shp.sort_values(by=['COMID'])
shp = shp.reset_index(drop=True)

df          = pd.DataFrame()
df['ID']    = segid
    
N = len(var)
for i in range(N):
    #% reading MESH state variables 
    state = xs.open_dataset(MESH_state_dir+state_name[i])
    state.close()
    
    # check condition for soil layers 
    if 'IG1' in state_name[i]:
        ly = 'Layer1'
    elif 'IG2' in state_name[i]:
        ly = 'Layer2'
    elif 'IG3' in state_name[i]:
        ly = 'Layer3'
    else:
        ly = []    
    state_var       = state[var[i]].values[:,:,0].transpose() 
    
    #% visualize MESH output 
    fig, ax = plt.subplots(figsize=(20, 20))
    df['value'] = state_var[:,0] # diplay first monthly data for 
    df = df.sort_values(by=['ID'])
    df = df.reset_index(drop=True)
    
    #% setting values in the shape file  
    # shapefile
    shp ['value'] = df ['value']
    
    mn = np.min(shp ['value'])    
    mx = np.max(shp ['value'])
    
    tm = datetime.datetime(1980, 1, 2).strftime('%Y-%m') 
    if (len(ly) != 0):
        tl = 'BowBanff'+'_MESH_'+var[i]+'_'+ly+'_'+tm
    else:
        tl = 'BowBanff'+'_MESH_'+var[i]+'_'+tm
        
    ax.set_title(tl)
    ax.set_xlabel('Longitude [degree east]')            
    ax.set_ylabel('Latitude [degree north]')
    
    # create the colorbar
    norm = colors.Normalize(vmin=mn,vmax=mx)
    # NB: when the mn and mx are equal (e.g., mn = mx = 0), the normalization is not 
    #required 
    if (mn != mx):
        cbar = plt.cm.ScalarMappable(norm=norm, cmap=cmaps[0])
    else:
        cbar = plt.cm.ScalarMappable(cmap=cmaps[0])
    
    shp.plot(column='value', cmap=cmaps[0], edgecolor='k',linewidth=0.1,ax = ax, vmin = mn, vmax = mx)
    
    # add colorbar and its label
    ax_cbar = fig.colorbar(cbar, ax=ax, extend='max')  
    ax_cbar.set_label(cbar_tl[i])
    
    # save plot
    plt.savefig(outdir+tl+'monthly_state.png', format='png', dpi=600)
    plt.close()