# MESH Scripts Repository

The purpose of this repository is to provide a place for sharing and improving scripts for pre- and post-processing data, as well as running, the MESH Hydrology Land-Surface model.

R is the primary language of development for MESH pre- and post-processing scripts, and some useful tools (listed below) have already been developed. However, scripts and functions in any programming language are welcome on this repository.

Key R-Language Packages:
- R Packages developed at the Centre for Hydrology, University of Saskatchewan
  - [MESHr](https://github.com/CentreForHydrology/MESHr) - R package containing functions for pre- and post- processing MESH models.
  - [CRHMr](https://github.com/CentreForHydrology/CRHMr) - R package for the CRHM model, but also contains functions for gap removal, infilling and imputation of time series data, which may be useful for MESH.
  - [tidyhydat](https://github.com/ropensci/tidyhydat) - an R package managed by the BCMOE with functions for accessing and plotting hydrometric data from the Water Survey of Canada.
  - [HYDAT](https://github.com/CentreForHydrology/HYDAT) - R package to interface to Canadian Hydrometric Data (HYDAT) published by Water Survey of Canada  
  - [WISKIr](https://github.com/CentreForHydrology/WISKIr) - R package containing functions for retrieving data from a Wiski database
- Canadian Society for Hydrological Scieces (CSHS) hydRology package [CSHShydRology](https://github.com/CSHS-CWRA/CSHShydRology) contains functions useful to Canadian hydrologists, such as statistical hydrology, data manipulation, visualization, spatial hydrology, and streamflow analysis.
- [weathercan](https://github.com/ropensci/weathercan) R package for searching and downloading multiple months/years of historical weather data from Environment and Climate Change Canada's (ECCC) website.
- [CRAN Task View for Hydrology](https://cran.r-project.org/web/views/Hydrology.html) Information about R packages broadly relevant to hydrology.
- The [humidity](https://cran.r-project.org/web/packages/humidity/index.html) package for R (CRAN) which contains useful functions for converting relative humidity and water vapour pressure to specific humidity (ie. see functions SVP.Murray, WVP2, and SH, as well as C2K).

Code in this repository is organized according to function, as listed below:

- Pre-processing
  - Driving_data
  - Drainage_database
  - Input_files
- Run_MESH *(bash command line, running on servers (Plato, Graham, etc.), etc.)*
- Post-processing

Within each main folder, each script should be in its own appropriately-named sub-folder. The script folders should including the code file(s) and a "how-to" document (markdown format preferred) outlining instructions for use such as language used, dependent packages, data / format requirements, how to use the script/functions, outputs produced, and any relevant reference documentation.
