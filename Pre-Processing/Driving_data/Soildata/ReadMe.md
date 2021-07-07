# Extraction of GSDE soil dataset

## Purpose 
The purpose is to read the Global GSDE NetCDF dataset and extract soil variables, such as sand, clay, organic carbon, and soil depth to bedrock for a domain of interest. The original GSDE soil texture dataset for each soil information has eight vertical layers at  30  ̋ × 30  ̋ spatial resolution that is converted to MESH soil layers (e.g., 4 layers). 

## Data access 
The dataset is available online at [GSDE] (http://globalchange.bnu.edu.cn)

## code structure 
The GSDE_SoilLayers_recombine reads original soil texture GSDE soil layers (eg., CLAY, SAND, OC) or SDEP, then it extracts required layers for a domain of interest by using the Extract_GSDE function. Thereafter, based on the number of soil layers (e.g., four-layer), the desired soil layers are constructed based on 8 soil depths. Finally, the GSDE soil texture data is saved in the GeoTiff format. Note that, the specification of the domain, including boundary and grid size, is hardcoded and should be declared by the user.  

## References 
Shangguan, W., Dai, Y., Duan, Q., Liu, B., & Yuan, H. (2014). A global soil data set for earth system modeling. Journal of Advances in Modeling Earth Systems, 6(1), 249–263. https://doi.org/10.1002/2013MS000293

Shangguan, W., Hengl, T., Mendes de Jesus, J., Yuan, H., & Dai, Y. (2017). Mapping the global depth to bedrock for land surface modeling. Journal of Advances in Modeling Earth Systems, 9(1), 65–88. https://doi.org/10.1002/2016MS000686
