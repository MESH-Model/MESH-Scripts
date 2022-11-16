# This script is intended to compare, and identify discrepancies between, the outputs of 
# the QGIS and GIS Tool zonal statistics outputs.

import geopandas as gpd
import pandas as pd
import warnings
warnings.filterwarnings("ignore")

## Setting Input files
# Import the QGIS zonal histogram shapefile
input_zh = './NALCMS2010_PFAF71_zonalhist.shp'
# Import the GIS Tool zonal statistics .csv file
in_zhg = './zh_gistool_PFAF71.csv'
# set the level of acceptable difference between QGIS and GIS Tool fraction values
tolerance = abs(0.01)
# Read QGIS .shp to GeoDataFrame then convert to Pandas DataFrame
gdf = gpd.read_file(input_zh)
lc_zonal_hist = pd.DataFrame(gdf)
# Read GIS Tool .csv into Pandas DataFrame
gtool = pd.read_csv(in_zhg)

# Remove unnecessary columns from QGIS DataFrame
cols=[]
for i in lc_zonal_hist.columns:
    if "NALCMS" in i:
        cols.append(i)
lc_zh = lc_zonal_hist[cols]

## Calculating landcover fraction for QGIS DataFrame
# Add a sum column for QGIS DataFrame
lc_sum = lc_zh.sum(axis=1)
lc_zh['sum'] = lc_sum
# Calcullate the fraction for each land cover
for i in lc_zh:
    lc_zh['frac_{}'.format(i.replace('NALCMS_',''))] = lc_zh[i]/lc_zh['sum']
# Remove the original NALCMS_ columns containing counts and the sum column
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

#Calculate the difference between QGIS and GIS Tool fraction values (QGIS value - GIS Tool value)
diff = frac.subtract(gfrac)
# Add COMID column to the Difference DataFrame
diff['COMID'] = lc_zonal_hist['COMID']

## Identify problems
problems = []
for i in diff.columns:
    if 'frac' in i:
        for j in diff[i]:
            if j > tolerance:   # Set the acceptable level of difference between QGIS and GIS Tool values
                problems.append([i,j])

# find the COMID of the problem cell
problemindex = 0

for i in problems:
    problemindex = int(diff[diff[i[0]]==i[1]].index.values)
    i.append(diff['COMID'].loc[problemindex])

print("Problem tolerance is set to {}".format(tolerance))
print("{} problems found: ".format(len(problems)),'\n')
for i in problems:
    print("COMID: {}".format(i[2]))
    print("Land Cover Class: {}".format(i[0].strip("frac_")))
    print("Difference (QGIS - GIS Tool): {}".format(i[1]),"\n")

with open('differences.txt', 'w') as f:
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