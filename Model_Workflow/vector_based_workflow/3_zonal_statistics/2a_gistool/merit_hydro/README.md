# `MERIT-Hydro` Geospatial Dataset
In this file, the necessary technical details of the dataset is explained.

## Location of the `MERIT-Hydro` Dataset Files
The `MERIT-Hydro` geospatial dataset files are located under the following directory accessible from Digital Alliance (formerly Compute Canada) Graham cluster:

```console
/project/rpp-kshook/CompHydCore/merit_hydro/raw_data
```

And the structure of the files is as following:
```console
/project/rpp-kshook/CompHydCore/merit_hydro/raw_data
├── elv
│   ├── elv_n00e000.tar
│   ├── elv_n00e030.tar
│   ├── .
│   ├── .
│   ├── .
│   └── elv_s60w180.tar
.
.
.
└── %var
    ├── %var_n00e000.tar
    ├── %var_n00e030.tar
    ├── .
    ├── .
    ├── .
    └── %var_s60w180.tar
```
In each `.tar` file, there are multiple GeoTIFF (`.tif`) files that cover the extents indicated in the parent `.tar` file name. For example, the `elv_n00e000.tar` contains the following files:
```console
/path/to/elv_n00e000.tar/contents
├── n00e005_elv.tif
├── n00e010_elv.tif
├── n00e015_elv.tif
├── n00e020_elv.tif
├── n00e025_elv.tif
├── n05e000_elv.tif
├── n05e005_elv.tif
├── n05e010_elv.tif
├── n05e015_elv.tif
├── n05e020_elv.tif
├── n05e025_elv.tif
├── n10e000_elv.tif
├── n10e005_elv.tif
├── n10e010_elv.tif
├── n10e015_elv.tif
├── n10e020_elv.tif
├── n10e025_elv.tif
├── n15e000_elv.tif
├── n15e005_elv.tif
├── n15e010_elv.tif
├── n15e015_elv.tif
├── n15e020_elv.tif
├── n15e025_elv.tif
├── n20e000_elv.tif
├── n20e005_elv.tif
├── n20e010_elv.tif
├── n20e015_elv.tif
├── n20e020_elv.tif
├── n20e025_elv.tif
├── n25e000_elv.tif
├── n25e005_elv.tif
├── n25e010_elv.tif
├── n25e015_elv.tif
├── n25e020_elv.tif
└── n25e025_elv.tif
```

## Spatial and Temporal Extents

The spatial extent of this dataset covers longitudes from `-180` to `+180` degress and latitudes from `-60` to `+90` degress. This dataset is static and does not vary with time. 

## Dataset Variables
This dataset has 6 main variables that are described in the following table:

|#	|Variable Name (used in `gistool`)	|Description				|Comments	|
|-------|---------------------------------------|---------------------------------------|---------------|
|1	|`dir`					|Flow Direction Map			|		|
|2	|`elv`					|Adjusted Elevation			|		|
|3	|`upa`					|Upstream Drainage Area			|		|
|4	|`upg`					|Number of Upstream Drainage Pixels	|		|
|5	|`wth`					|River Width				|		|
|6	|`hnd`					|Height Above Nearest Drainage		|		|

A description of all the variables included in this dataset is explained on the `MERIT-Hydro`'s [website](http://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/).

