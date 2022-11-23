# This script is intended to compare, and identify discrepancies between, the outputs of 
# the QGIS and GIS Tool zonal statistics outputs.

import geopandas as gpd
import pandas as pd
import warnings
warnings.filterwarnings("ignore")

## Setting Inputs
qgis_zh = '../landcover_extract/Output/NALCMS2010_BowBanff_zonalhist.shp'       # Import the QGIS zonal histogram shapefile
gistool_zh = './Output/landsat_bow_stats_NA_NALCMS_2010_v2_land_cover_30m.csv'         # Import the GIS Tool zonal statistics .csv file
tolerance = abs(0.01)                                                           # set the level of acceptable difference between QGIS and GIS Tool fraction values
                             
##Reading inputs to DataFrames
gdf = gpd.read_file(qgis_zh)                        # Read QGIS .shp to GeoDataFrame, 
qgis = pd.DataFrame(gdf)                            # then convert to Pandas DataFrame.
gtool = pd.read_csv(gistool_zh)                     # Read GIS Tool .csv into Pandas DataFrame.

# Remove unnecessary columns from QGIS DataFrame
cols=[]
for i in qgis.columns:
    if "NALCMS" in i:
        cols.append(i)
lc_zh = qgis[cols]

## Calculating landcover fraction for QGIS DataFrame
# Add a sum column for QGIS DataFrame
lc_sum = lc_zh.sum(axis=1)
lc_zh['sum'] = lc_sum

# Calculate the fraction for each land cover. Replaces 'NALCMS' with 'frac'.
for i in lc_zh:
    lc_zh['frac_{}'.format(i.replace('NALCMS_',''))] = lc_zh[i]/lc_zh['sum']

# Remove the sum column and original NALCMS_ columns containing counts 
cols=[]
for i in lc_zh.columns:
    if "frac" in i:
        if 'sum' not in i:
            cols.append(i)
frac = lc_zh[cols]

# Calculate sum of fractions to ensure they add up to 1
fc_sum = frac.sum(axis=1)
frac['sum'] = fc_sum

#check that total of fractions is 1.0 (+- 0.01)
for i in frac['sum']:
    if i - 1.0 > abs(0.01):
        print('Bad total: {}'.format(i))

#Remove unneecessary columns from GIS Tool DataFrame
cols=[]
for i in gtool.columns:
    if "frac" in i:
        cols.append(i)
gfrac = gtool[cols]

# rename frac_0 to frac_NOD.
gfrac = gfrac.rename(columns={"frac_0":"frac_NOD"})

#Calculate the difference between QGIS and GIS Tool fraction values (QGIS value - GIS Tool value)
diff = frac.subtract(gfrac)

# Add COMID column to the Difference DataFrame
diff['COMID'] = qgis['COMID']

# Create a new DataFrame for problem basins
problem_basins = pd.DataFrame(columns=diff.columns)
problem_basins = problem_basins.set_index('COMID')

# Set diff index to COMID
diff = diff.set_index('COMID')

## Identify problems
problems = []
for i in diff.columns:
    if 'frac' in i:
        for j in diff[i]:
            if j > tolerance:   # Set the acceptable level of difference between QGIS and GIS Tool values
                problems.append([i,j])

# Add the problem basins to a new DataFrame
problemindex = 0
for i in problems:
    problem_basins = problem_basins.append(diff.loc[diff.index.values[diff[i[0]]==i[1]][0]])

# Sum the differences across all columns for each subbasin. 
# If sum = 0, then the problem should be due to differences between QGIS and GIS TOOL in land cover class allocation to that subbasin
# sum column is rounded to 4 decimals. There may be very small differences due to rounding. 
diff_sum = round(problem_basins.sum(axis=1),4)
problem_basins['sum'] = diff_sum

problem_basins.to_csv('./Output/differences.csv')

exit()


## OPTIONAL: save list of problem basins to text file
# Print Problems to console
print("Input QGIS shapefile: {}".format(qgis_zh))
print("Input GIS Tool .csv: {}".format(gistool_zh))
print("Problem tolerance is set to {}".format(tolerance))
print("{} problems found: ".format(len(problems)),'\n')
for i in problems:
    print("COMID: {}".format(i[2]))
    print("Land Cover Class: {}".format(i[0].strip("frac_")))
    print("Difference (QGIS - GIS Tool): {}".format(i[1]),"\n")

# Save problems as 'difference.txt'
with open('differences.txt', 'w') as f:
    f.write("Input QGIS shapefile: {}".format(qgis_zh))
    f.write('\n')
    f.write("Input GIS Tool .csv: {}".format(gistool_zh))
    f.write('\n')
    f.write("Problem tolerance is set to {}".format(tolerance))
    f.write('\n')
    f.write("{} problems found: ".format(len(problems)))
    f.write('\n')
    f.write('\n')
    for i in problems:
        f.write("COMID: {}".format(i[2]))
        f.write('\n')
        f.write("Land Cover Class: {}".format(i[0].strip("frac_")))
        f.write('\n')
        f.write("Difference (QGIS - GIS Tool): {}".format(i[1]))
        f.write('\n')
        f.write('\n')