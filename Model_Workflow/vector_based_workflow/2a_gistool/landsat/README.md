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


# Other relevant information
## Land Cover Types
Below the land cover types for each values of the `.tif` files is detailed based on [1].

|#      |Class Value (used in `gistool`)	|Land Cover Description						|RGB Value		|
|-------|---------------------------------------|---------------------------------------------------------------|-----------------------|
|1	|1					|Temperate or sub-polar needleleaf forest			|RGB (0, 61, 0)		|
|2	|2					|Sub-polar taiga needleleaf forest				|RGB (148, 156, 112)	|
|3	|3					|Tropical or sub-tropical broadleaf evergreen forest		|RGB (0, 99, 0)		|
|4	|4					|Tropical or sub-tropical broadleaf deciduous forest		|RGB (30, 171, 5)	|
|5	|5					|Temperate or sub-polar broadleaf deciduous forest		|RGB (20, 140, 61)	|
|6	|6					|Mixed forest							|RGB (92, 117, 43)	|
|7	|7					|Tropical or sub-tropical shrubland				|RGB (179, 158, 43)	|
|8	|8					|Temperate or sub-polar shrubland				|RGB (179, 138, 51)	|
|9	|9					|Tropical or sub-tropical grassland				|RGB (232, 220, 94)	|
|10	|10					|Temperate or sub-polar grassland				|RGB (225, 207, 138)	|
|11	|11					|Sub-polar or polar shrubland-lichen-moss			|RGB (156, 117, 84)	|
|12	|12					|Sub-polar or polar grassland-lichen-moss			|RGB (186, 212, 143)	|
|13	|13					|Sub-polar or polar barren-lichen-moss				|RGB (64, 138, 112)	|
|14	|14					|Wetland							|RGB (107, 163, 138)	|
|15	|15					|Cropland							|RGB (230, 174, 102)	|
|16	|16					|Barren lands							|RGB (168, 171, 174)	|
|17	|17					|Urban								|RGB (220, 33, 38)	|
|18	|18					|Water								|RGB (76, 112, 163)	|
|19	|19					|Snow and ice							|RGB (255, 250, 255)	|

Also, the details of the above table has been included in the following files: [landsat_classes.csv](./landsat_classes.csv).

