# `Landsat` Geospatial Dataset
In this file, the necessary technical details of the dataset is explained.

## Location of the `Landsat` Dataset Files
The `LandSat` geospatial dataset files are located under the following directory accessible from Digital Alliance (formerly Compute Canada) Graham cluster:

```console
/project/rpp-kshook/Model_Output/Landsat/
```

And the structure of the files is as following:

```console
/project/rpp-kshook/Model_Output/Landsat/
├── NA_NALCMS_2010_v2_land_cover_30m.zip
├── .
├── .
└── .
```

## Spatial and Temporal Extents

The spatial extent of this dataset (so far only `NALCMS` that is a land cover dataset) covers longitudes from approximately `-180` to `-50` degress and latitudes from approximately `+14` to `+84` degress. This dataset is static and does not vary with time. 

## Dataset Variables
This variables of this dataset are detailed in the table below:

|#	|Variable Name (used in `gistool`)	|Description				|Comments	|
|-------|---------------------------------------|---------------------------------------|---------------|
|1      |NA_NALCMS_2010_v2_land_cover_30m       |Land cover classes			|[link](http://www.cec.org/north-american-environmental-atlas/land-cover-2010-landsat-30m/)|
