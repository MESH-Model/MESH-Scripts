# `Soil Grids V1` Geospatial Dataset
In this file, the necessary technical details of the dataset is explained.

## Location of the `Soil Grids V1` Dataset Files
The `Soil Grids V1` geospatial dataset files are located under the following directory accessible from Digital Alliance (formerly Compute Canada) Graham cluster:

```console
/project/rpp-kshook/Model_Output/SoilGridsV1/soilgrids/former/2017-03-10/data
```

And the structure of the files is as following:

```console
/project/rpp-kshook/Model_Output/SoilGridsV1/soilgrids/former/2017-03-10/data
├── ACDWRB_M_ss_250m_ll.tif 
├── ACDWRB_M_ss_250m_ll.tif.xml
├── AWCh1_M_sl1_250m_ll.tif
├── AWCh1_M_sl1_250m_ll.tif.xml
├── AWCh1_M_sl2_250m_ll.tif
├── AWCh1_M_sl2_250m_ll.tif.xml
├── %var_M_sl{%depth_level}_250m_ll.tif
├── %var_M_sl{%depth_level}_250m_ll.tif.xml
├── . 
├── .
├── .
├── AWCh1_M_sl7_250m_ll.tif
├── AWCh1_M_sl7_250m_ll.tif.xml
├── .
├── .
├── .
├── SNDPPT_M_sl7_250m_ll.tif
├── SNDPPT_M_sl7_250m_ll.tif.xml
├── WWP_M_sl7_250m_ll.tif
├── WWP_M_sl7_250m_ll.tif.xml
├── .
├── .
├── .
├── TAXNWRB_250m_ll.tif
├── TAXNWRB_250m_ll.tif.xml
├── TAXNWRB_Acric.Ferralsols_250m_ll.tif
├── TAXNWRB_Acric.Ferralsols_250m_ll.tif.xml
├── TAXNWRB_Acric.Plinthosols_250m_ll.tif
├── TAXNWRB_Acric.Plinthosols_250m_ll.tif.xml
├── .
├── .
├── .
├── TAXNWRB_%{short_group_name}_250m_ll.tif
├── TAXNWRB_%{short_group_name}_250m_ll.tif.xml
├── .
├── .
├── .
├── TAXNWRB_Vitric.Cryosols_250m_ll.tif
├── TAXNWRB_Vitric.Cryosols_250m_ll.tif.xml
├── TAXOUSDA_250m_ll.tif
├── TAXOUSDA_250m_ll.tif.xml
├── TAXOUSDA_Albolls_250m_ll.tif
├── TAXOUSDA_Albolls_250m_ll.tif.xml
├── .
├── .
├── .
├── TAXOUSDA_%{group_name}_250m_ll.tif
├── TAXOUSDA_${group_name}_250m_ll.tif.xml
├── .
├── .
├── .
├── TAXOUSDA_Xerults_250m_ll.tif
└── TAXOUSDA_Xerults_250m_ll.tif.xml 
```

## Spatial and Temporal Extents

The spatial extent of this dataset covers longitudes from `-180` to `+180` degress and latitudes from `-90` to `+90` degress. This dataset is static and does not vary with time. 

## Dataset Variables
This variables of this dataset are detailed in the table below:

|#	|Variable Name (used in `gistool`)	|Description				|Comments	|
|-------|---------------------------------------|---------------------------------------|---------------|
|1		|BDRICM_M_250m_ll			 					|Depth to bedrock (R horizon) up to 200 cm								|	|
|2		|BDRLOG_M_250m_ll			 					|Probability of occurrence of R horizon									|	|
|3		|BDTICM_M_250m_ll			 					|Absolute depth to bedrock (in cm)										|	|
|4		|BLDFIE_M_sl1_250m_ll			 				|Bulk density (fine earth) in kg / cubic-meter							|	|
|5		|BLDFIE_M_sl2_250m_ll			 				|Bulk density (fine earth) in kg / cubic-meter							|	|
|6		|BLDFIE_M_sl3_250m_ll			 				|Bulk density (fine earth) in kg / cubic-meter							|	|
|7		|BLDFIE_M_sl4_250m_ll			 				|Bulk density (fine earth) in kg / cubic-meter							|	|
|8		|BLDFIE_M_sl5_250m_ll			 				|Bulk density (fine earth) in kg / cubic-meter							|	|
|9		|BLDFIE_M_sl6_250m_ll			 				|Bulk density (fine earth) in kg / cubic-meter							|	|
|10		|BLDFIE_M_sl7_250m_ll			 				|Bulk density (fine earth) in kg / cubic-meter							|	|
|11		|CECSOL_M_sl1_250m_ll			 				|Cation exchange capacity of soil in cmolc/kg							|	|
|12		|CECSOL_M_sl2_250m_ll			 				|Cation exchange capacity of soil in cmolc/kg							|	|
|13		|CECSOL_M_sl3_250m_ll			 				|Cation exchange capacity of soil in cmolc/kg							|	|
|14		|CECSOL_M_sl4_250m_ll			 				|Cation exchange capacity of soil in cmolc/kg							|	|
|15		|CECSOL_M_sl5_250m_ll			 				|Cation exchange capacity of soil in cmolc/kg							|	|
|16		|CECSOL_M_sl6_250m_ll			 				|Cation exchange capacity of soil in cmolc/kg							|	|
|17		|CECSOL_M_sl7_250m_ll			 				|Cation exchange capacity of soil in cmolc/kg							|	|
|18		|CLYPPT_M_sl1_250m_ll			 				|Clay content (0-2 micro meter) mass fraction in %						|	|
|19		|CLYPPT_M_sl2_250m_ll			 				|Clay content (0-2 micro meter) mass fraction in %						|	|
|20		|CLYPPT_M_sl3_250m_ll			 				|Clay content (0-2 micro meter) mass fraction in %						|	|
|21		|CLYPPT_M_sl4_250m_ll			 				|Clay content (0-2 micro meter) mass fraction in %						|	|
|22		|CLYPPT_M_sl5_250m_ll			 				|Clay content (0-2 micro meter) mass fraction in %						|	|
|23		|CLYPPT_M_sl6_250m_ll			 				|Clay content (0-2 micro meter) mass fraction in %						|	|
|24		|CLYPPT_M_sl7_250m_ll			 				|Clay content (0-2 micro meter) mass fraction in %						|	|
|25		|CRFVOL_M_sl1_250m_ll			 				|Coarse fragments volumetric in %										|	|
|26		|CRFVOL_M_sl2_250m_ll			 				|Coarse fragments volumetric in %										|	|
|27		|CRFVOL_M_sl3_250m_ll			 				|Coarse fragments volumetric in %										|	|
|28		|CRFVOL_M_sl4_250m_ll			 				|Coarse fragments volumetric in %										|	|
|29		|CRFVOL_M_sl5_250m_ll			 				|Coarse fragments volumetric in %										|	|
|30		|CRFVOL_M_sl6_250m_ll			 				|Coarse fragments volumetric in %										|	|
|31		|CRFVOL_M_sl7_250m_ll			 				|Coarse fragments volumetric in %										|	|
|32		|OCSTHA_M_sd1_250m_ll			 				|Soil organic carbon stock in tons per ha								|	|
|33		|OCSTHA_M_sd2_250m_ll			 				|Soil organic carbon stock in tons per ha								|	|
|34		|OCSTHA_M_sd3_250m_ll			 				|Soil organic carbon stock in tons per ha								|	|
|35		|OCSTHA_M_sd4_250m_ll			 				|Soil organic carbon stock in tons per ha								|	|
|36		|OCSTHA_M_sd5_250m_ll			 				|Soil organic carbon stock in tons per ha								|	|
|37		|OCSTHA_M_sd6_250m_ll			 				|Soil organic carbon stock in tons per ha								|	|
|38		|OCSTHA_M_30cm_250m_ll			 				|Soil organic carbon stock in tons per ha								|	|
|39		|OCSTHA_M_100cm_250m_ll			 				|Soil organic carbon stock in tons per ha								|	|
|40		|OCSTHA_M_200cm_250m_ll			 				|Soil organic carbon stock in tons per ha								|	|
|41		|OCDENS_M_sl1_250m_ll			 				|Soil organic carbon density in kg per cubic-m							|	|
|42		|OCDENS_M_sl2_250m_ll			 				|Soil organic carbon density in kg per cubic-m							|	|
|43		|OCDENS_M_sl3_250m_ll			 				|Soil organic carbon density in kg per cubic-m							|	|
|44		|OCDENS_M_sl4_250m_ll			 				|Soil organic carbon density in kg per cubic-m							|	|
|45		|OCDENS_M_sl5_250m_ll			 				|Soil organic carbon density in kg per cubic-m							|	|
|46		|OCDENS_M_sl6_250m_ll			 				|Soil organic carbon density in kg per cubic-m							|	|
|47		|OCDENS_M_sl7_250m_ll			 				|Soil organic carbon density in kg per cubic-m							|	|
|48		|ORCDRC_M_sl1_250m_ll			 				|Soil organic carbon content (fine earth fraction) in g per kg			|	|
|49		|ORCDRC_M_sl2_250m_ll			 				|Soil organic carbon content (fine earth fraction) in g per kg			|	|
|50		|ORCDRC_M_sl3_250m_ll			 				|Soil organic carbon content (fine earth fraction) in g per kg			|	|
|51		|ORCDRC_M_sl4_250m_ll			 				|Soil organic carbon content (fine earth fraction) in g per kg			|	|
|52		|ORCDRC_M_sl5_250m_ll			 				|Soil organic carbon content (fine earth fraction) in g per kg			|	|
|53		|ORCDRC_M_sl6_250m_ll			 				|Soil organic carbon content (fine earth fraction) in g per kg			|	|
|54		|ORCDRC_M_sl7_250m_ll			 				|Soil organic carbon content (fine earth fraction) in g per kg			|	|
|55		|PHIHOX_M_sl1_250m_ll			 				|Soil pH x 10 in H2O 													|	|
|56		|PHIHOX_M_sl2_250m_ll			 				|Soil pH x 10 in H2O 													|	|
|57		|PHIHOX_M_sl3_250m_ll			 				|Soil pH x 10 in H2O 													|	|
|58		|PHIHOX_M_sl4_250m_ll			 				|Soil pH x 10 in H2O 													|	|
|59		|PHIHOX_M_sl5_250m_ll			 				|Soil pH x 10 in H2O 													|	|
|60		|PHIHOX_M_sl6_250m_ll			 				|Soil pH x 10 in H2O 													|	|
|61		|PHIHOX_M_sl7_250m_ll			 				|Soil pH x 10 in H2O 													|	|
|62		|PHIKCL_M_sl1_250m_ll			 				|Soil pH x 10 in KCl													|	|
|63		|PHIKCL_M_sl2_250m_ll			 				|Soil pH x 10 in KCl													|	|
|64		|PHIKCL_M_sl3_250m_ll			 				|Soil pH x 10 in KCl													|	|
|65		|PHIKCL_M_sl4_250m_ll			 				|Soil pH x 10 in KCl													|	|
|66		|PHIKCL_M_sl5_250m_ll			 				|Soil pH x 10 in KCl													|	|
|67		|PHIKCL_M_sl6_250m_ll			 				|Soil pH x 10 in KCl													|	|
|68		|PHIKCL_M_sl7_250m_ll			 				|Soil pH x 10 in KCl													|	|
|69		|SLTPPT_M_sl1_250m_ll			 				|Silt content (2-50 micro meter) mass fraction in %						|	|
|70		|SLTPPT_M_sl2_250m_ll			 				|Silt content (2-50 micro meter) mass fraction in %						|	|
|71		|SLTPPT_M_sl3_250m_ll			 				|Silt content (2-50 micro meter) mass fraction in %						|	|
|72		|SLTPPT_M_sl4_250m_ll			 				|Silt content (2-50 micro meter) mass fraction in %						|	|
|73		|SLTPPT_M_sl5_250m_ll			 				|Silt content (2-50 micro meter) mass fraction in %						|	|
|74		|SLTPPT_M_sl6_250m_ll			 				|Silt content (2-50 micro meter) mass fraction in %						|	|
|75		|SLTPPT_M_sl7_250m_ll			 				|Silt content (2-50 micro meter) mass fraction in %						|	|
|76		|SNDPPT_M_sl1_250m_ll			 				|Sand content (50-2000 micro meter) mass fraction in %					|	|
|77		|SNDPPT_M_sl2_250m_ll			 				|Sand content (50-2000 micro meter) mass fraction in %					|	|
|78		|SNDPPT_M_sl3_250m_ll			 				|Sand content (50-2000 micro meter) mass fraction in %					|	|
|79		|SNDPPT_M_sl4_250m_ll			 				|Sand content (50-2000 micro meter) mass fraction in %					|	|
|80		|SNDPPT_M_sl5_250m_ll			 				|Sand content (50-2000 micro meter) mass fraction in %					|	|
|81		|SNDPPT_M_sl6_250m_ll			 				|Sand content (50-2000 micro meter) mass fraction in %					|	|
|82		|SNDPPT_M_sl7_250m_ll			 				|Sand content (50-2000 micro meter) mass fraction in %					|	|
|83		|TEXMHT_M_sl1_250m_ll			 				|Texture class (USDA system)											|	|
|84		|TEXMHT_M_sl2_250m_ll			 				|Texture class (USDA system)											|	|
|85		|TEXMHT_M_sl3_250m_ll			 				|Texture class (USDA system)											|	|
|86		|TEXMHT_M_sl4_250m_ll			 				|Texture class (USDA system)											|	|
|87		|TEXMHT_M_sl5_250m_ll			 				|Texture class (USDA system)											|	|
|88		|TEXMHT_M_sl6_250m_ll			 				|Texture class (USDA system)											|	|
|89		|TEXMHT_M_sl7_250m_ll			 				|Texture class (USDA system)											|	|
|90		|TAXNWRB_250m_ll			 					|WRB 2006 class															|	|
|91		|TAXNWRB_Acric.Ferralsols_250m_ll				|WRB 2006 class															|	|
|92		|TAXNWRB_Acric.Plinthosols_250m_ll				|WRB 2006 class															|	|
|93		|TAXNWRB_Albic.Arenosols_250m_ll				|WRB 2006 class															|	|
|94		|TAXNWRB_Albic.Luvisols_250m_ll					|WRB 2006 class															|	|
|95		|TAXNWRB_Alic.Nitisols_250m_ll					|WRB 2006 class															|	|
|96		|TAXNWRB_Aluandic.Andosols_250m_ll				|WRB 2006 class															|	|
|97		|TAXNWRB_Aric.Regosols_250m_ll					|WRB 2006 class															|	|
|98		|TAXNWRB_Calcaric.Regosols_250m_ll				|WRB 2006 class															|	|
|99		|TAXNWRB_Calcic.Chernozems_250m_ll				|WRB 2006 class															|	|
|100	|TAXNWRB_Calcic.Gleysols_250m_ll				|WRB 2006 class															|	|
|101	|TAXNWRB_Calcic.Gypsisols_250m_ll				|WRB 2006 class															|	|
|102	|TAXNWRB_Calcic.Histosols_250m_ll				|WRB 2006 class															|	|
|103	|TAXNWRB_Calcic.Kastanozems_250m_ll				|WRB 2006 class															|	|
|104	|TAXNWRB_Calcic.Luvisols_250m_ll				|WRB 2006 class															|	|
|105	|TAXNWRB_Calcic.Solonetz_250m_ll				|WRB 2006 class															|	|
|106	|TAXNWRB_Calcic.Vertisols_250m_ll				|WRB 2006 class															|	|
|107	|TAXNWRB_Cryic.Histosols_250m_ll				|WRB 2006 class															|	|
|108	|TAXNWRB_Cutanic.Alisols_250m_ll				|WRB 2006 class															|	|
|109	|TAXNWRB_Endogleyic.Cambisols_250m_ll			|WRB 2006 class															|	|
|110	|TAXNWRB_Endogleyic.Planosols_250m_ll			|WRB 2006 class															|	|
|111	|TAXNWRB_Ferralic.Arenosols_250m_ll				|WRB 2006 class															|	|
|112	|TAXNWRB_Ferralic.Cambisols_250m_ll				|WRB 2006 class															|	|
|113	|TAXNWRB_Fibric.Histosols_250m_ll				|WRB 2006 class															|	|
|114	|TAXNWRB_Gleyic.Luvisols_250m_ll				|WRB 2006 class															|	|
|115	|TAXNWRB_Gleyic.Podzols_250m_ll					|WRB 2006 class															|	|
|116	|TAXNWRB_Gleyic.Solonetz_250m_ll				|WRB 2006 class															|	|
|117	|TAXNWRB_Gypsic.Solonchaks_250m_ll				|WRB 2006 class															|	|
|118	|TAXNWRB_Haplic.Acrisols_250m_ll				|WRB 2006 class															|	|
|119	|TAXNWRB_Haplic.Acrisols..Alumic._250m_ll		|WRB 2006 class															|	|
|120	|TAXNWRB_Haplic.Acrisols..Ferric._250m_ll		|WRB 2006 class															|	|
|121	|TAXNWRB_Haplic.Acrisols..Humic._250m_ll		|WRB 2006 class															|	|
|122	|TAXNWRB_Haplic.Albeluvisols_250m_ll			|WRB 2006 class															|	|
|123	|TAXNWRB_Haplic.Alisols_250m_ll					|WRB 2006 class															|	|
|124	|TAXNWRB_Haplic.Andosols_250m_ll				|WRB 2006 class															|	|
|125	|TAXNWRB_Haplic.Arenosols_250m_ll				|WRB 2006 class															|	|
|126	|TAXNWRB_Haplic.Arenosols..Calcaric._250m_ll	|WRB 2006 class															|	|
|127	|TAXNWRB_Haplic.Calcisols_250m_ll				|WRB 2006 class															|	|
|128	|TAXNWRB_Haplic.Calcisols..Sodic._250m_ll		|WRB 2006 class															|	|
|129	|TAXNWRB_Haplic.Cambisols_250m_ll				|WRB 2006 class															|	|
|130	|TAXNWRB_Haplic.Cambisols..Calcaric._250m_ll	|WRB 2006 class															|	|
|131	|TAXNWRB_Haplic.Cambisols..Chromic._250m_ll		|WRB 2006 class															|	|
|132	|TAXNWRB_Haplic.Cambisols..Dystric._250m_ll		|WRB 2006 class															|	|
|133	|TAXNWRB_Haplic.Cambisols..Eutric._250m_ll		|WRB 2006 class															|	|
|134	|TAXNWRB_Haplic.Cambisols..Humic._250m_ll		|WRB 2006 class															|	|
|135	|TAXNWRB_Haplic.Cambisols..Sodic._250m_ll		|WRB 2006 class															|	|
|136	|TAXNWRB_Haplic.Chernozems_250m_ll				|WRB 2006 class															|	|
|137	|TAXNWRB_Haplic.Cryosols_250m_ll				|WRB 2006 class															|	|
|138	|TAXNWRB_Haplic.Ferralsols_250m_ll				|WRB 2006 class															|	|
|139	|TAXNWRB_Haplic.Ferralsols..Rhodic._250m_ll		|WRB 2006 class															|	|
|140	|TAXNWRB_Haplic.Ferralsols..Xanthic._250m_ll	|WRB 2006 class															|	|
|141	|TAXNWRB_Haplic.Fluvisols_250m_ll				|WRB 2006 class															|	|
|142	|TAXNWRB_Haplic.Fluvisols..Arenic._250m_ll		|WRB 2006 class															|	|
|143	|TAXNWRB_Haplic.Fluvisols..Calcaric._250m_ll	|WRB 2006 class															|	|
|144	|TAXNWRB_Haplic.Fluvisols..Dystric._250m_ll		|WRB 2006 class															|	|
|145	|TAXNWRB_Haplic.Fluvisols..Eutric._250m_ll		|WRB 2006 class															|	|
|146	|TAXNWRB_Haplic.Gleysols_250m_ll				|WRB 2006 class															|	|
|147	|TAXNWRB_Haplic.Gleysols..Dystric._250m_ll		|WRB 2006 class															|	|
|148	|TAXNWRB_Haplic.Gleysols..Eutric._250m_ll		|WRB 2006 class															|	|
|149	|TAXNWRB_Haplic.Gypsisols_250m_ll				|WRB 2006 class															|	|
|150	|TAXNWRB_Haplic.Kastanozems_250m_ll				|WRB 2006 class															|	|
|151	|TAXNWRB_Haplic.Leptosols_250m_ll				|WRB 2006 class															|	|
|152	|TAXNWRB_Haplic.Leptosols..Eutric._250m_ll		|WRB 2006 class															|	|
|153	|TAXNWRB_Haplic.Lixisols_250m_ll				|WRB 2006 class															|	|
|154	|TAXNWRB_Haplic.Lixisols..Chromic._250m_ll		|WRB 2006 class															|	|
|155	|TAXNWRB_Haplic.Lixisols..Ferric._250m_ll		|WRB 2006 class															|	|
|156	|TAXNWRB_Haplic.Luvisols_250m_ll				|WRB 2006 class															|	|
|157	|TAXNWRB_Haplic.Luvisols..Chromic._250m_ll		|WRB 2006 class															|	|
|158	|TAXNWRB_Haplic.Luvisols..Ferric._250m_ll		|WRB 2006 class															|	|
|159	|TAXNWRB_Haplic.Nitisols..Rhodic._250m_ll		|WRB 2006 class															|	|
|160	|TAXNWRB_Haplic.Phaeozems_250m_ll				|WRB 2006 class															|	|
|161	|TAXNWRB_Haplic.Planosols..Dystric._250m_ll		|WRB 2006 class															|	|
|162	|TAXNWRB_Haplic.Planosols..Eutric._250m_ll		|WRB 2006 class															|	|
|163	|TAXNWRB_Haplic.Podzols_250m_ll					|WRB 2006 class															|	|
|164	|TAXNWRB_Haplic.Regosols..Dystric._250m_ll		|WRB 2006 class															|	|
|165	|TAXNWRB_Haplic.Regosols..Eutric._250m_ll		|WRB 2006 class															|	|
|166	|TAXNWRB_Haplic.Regosols..Sodic._250m_ll		|WRB 2006 class															|	|
|167	|TAXNWRB_Haplic.Solonchaks_250m_ll				|WRB 2006 class															|	|
|168	|TAXNWRB_Haplic.Solonchaks..Sodic._250m_ll		|WRB 2006 class															|	|
|169	|TAXNWRB_Haplic.Solonetz_250m_ll				|WRB 2006 class															|	|
|170	|TAXNWRB_Haplic.Umbrisols_250m_ll				|WRB 2006 class															|	|
|171	|TAXNWRB_Haplic.Vertisols_250m_ll				|WRB 2006 class															|	|
|172	|TAXNWRB_Haplic.Vertisols..Eutric._250m_ll		|WRB 2006 class															|	|
|173	|TAXNWRB_Hemic.Histosols_250m_ll				|WRB 2006 class															|	|
|174	|TAXNWRB_Histic.Albeluvisols_250m_ll			|WRB 2006 class															|	|
|175	|TAXNWRB_Hypoluvic.Arenosols_250m_ll			|WRB 2006 class															|	|
|176	|TAXNWRB_Leptic.Cambisols_250m_ll				|WRB 2006 class															|	|
|177	|TAXNWRB_Leptic.Luvisols_250m_ll				|WRB 2006 class															|	|
|178	|TAXNWRB_Leptic.Phaeozems_250m_ll				|WRB 2006 class															|	|
|179	|TAXNWRB_Leptic.Regosols_250m_ll				|WRB 2006 class															|	|
|180	|TAXNWRB_Leptic.Umbrisols_250m_ll				|WRB 2006 class															|	|
|181	|TAXNWRB_Lithic.Leptosols_250m_ll				|WRB 2006 class															|	|
|182	|TAXNWRB_Lixic.Plinthosols_250m_ll				|WRB 2006 class															|	|
|183	|TAXNWRB_Luvic.Calcisols_250m_ll				|WRB 2006 class															|	|
|184	|TAXNWRB_Luvic.Chernozems_250m_ll				|WRB 2006 class															|	|
|185	|TAXNWRB_Luvic.Phaeozems_250m_ll				|WRB 2006 class															|	|
|186	|TAXNWRB_Luvic.Planosols_250m_ll				|WRB 2006 class															|	|
|187	|TAXNWRB_Luvic.Stagnosols_250m_ll				|WRB 2006 class															|	|
|188	|TAXNWRB_Mollic.Gleysols_250m_ll				|WRB 2006 class															|	|
|189	|TAXNWRB_Mollic.Leptosols_250m_ll				|WRB 2006 class															|	|
|190	|TAXNWRB_Mollic.Solonetz_250m_ll				|WRB 2006 class															|	|
|191	|TAXNWRB_Mollic.Vertisols_250m_ll				|WRB 2006 class															|	|
|192	|TAXNWRB_Petric.Calcisols_250m_ll				|WRB 2006 class															|	|
|193	|TAXNWRB_Petric.Durisols_250m_ll				|WRB 2006 class															|	|
|194	|TAXNWRB_Plinthic.Acrisols_250m_ll				|WRB 2006 class															|	|
|195	|TAXNWRB_Protic.Arenosols_250m_ll				|WRB 2006 class															|	|
|196	|TAXNWRB_Rendzic.Leptosols_250m_ll				|WRB 2006 class															|	|
|197	|TAXNWRB_Sapric.Histosols_250m_ll				|WRB 2006 class															|	|
|198	|TAXNWRB_Solodic.Planosols_250m_ll				|WRB 2006 class															|	|
|199	|TAXNWRB_Stagnic.Luvisols_250m_ll				|WRB 2006 class															|	|
|200	|TAXNWRB_Turbic.Cryosols_250m_ll				|WRB 2006 class															|	|
|201	|TAXNWRB_Umbric.Albeluvisols_250m_ll			|WRB 2006 class															|	|
|202	|TAXNWRB_Umbric.Ferralsols_250m_ll				|WRB 2006 class															|	|
|203	|TAXNWRB_Umbric.Gleysols_250m_ll				|WRB 2006 class															|	|
|204	|TAXNWRB_Vertic.Cambisols_250m_ll				|WRB 2006 class															|	|
|205	|TAXNWRB_Vertic.Luvisols_250m_ll				|WRB 2006 class															|	|
|206	|TAXNWRB_Vetic.Acrisols_250m_ll					|WRB 2006 class															|	|
|207	|TAXNWRB_Vitric.Andosols_250m_ll				|WRB 2006 class															|	|
|208	|TAXNWRB_Vitric.Cryosols_250m_ll				|WRB 2006 class															|	|
|209	|TAXOUSDA_250m_ll			 					|USDA 2014 class														|	|
|210	|TAXOUSDA_Albolls_250m_ll			 			|USDA 2014 class														|	|
|211	|TAXOUSDA_Aqualfs_250m_ll			 			|USDA 2014 class														|	|
|212	|TAXOUSDA_Aquands_250m_ll			 			|USDA 2014 class														|	|
|213	|TAXOUSDA_Aquents_250m_ll			 			|USDA 2014 class														|	|
|214	|TAXOUSDA_Aquepts_250m_ll			 			|USDA 2014 class														|	|
|215	|TAXOUSDA_Aquerts_250m_ll			 			|USDA 2014 class														|	|
|216	|TAXOUSDA_Aquods_250m_ll			 			|USDA 2014 class														|	|
|217	|TAXOUSDA_Aquolls_250m_ll			 			|USDA 2014 class														|	|
|218	|TAXOUSDA_Aquox_250m_ll			 				|USDA 2014 class														|	|
|219	|TAXOUSDA_Aquults_250m_ll			 			|USDA 2014 class														|	|
|220	|TAXOUSDA_Arents_250m_ll			 			|USDA 2014 class														|	|
|221	|TAXOUSDA_Argids_250m_ll			 			|USDA 2014 class														|	|
|222	|TAXOUSDA_Borolls_250m_ll			 			|USDA 2014 class														|	|
|223	|TAXOUSDA_Calcids_250m_ll			 			|USDA 2014 class														|	|
|224	|TAXOUSDA_Cambids_250m_ll			 			|USDA 2014 class														|	|
|225	|TAXOUSDA_Cryalfs_250m_ll			 			|USDA 2014 class														|	|
|226	|TAXOUSDA_Cryands_250m_ll			 			|USDA 2014 class														|	|
|227	|TAXOUSDA_Cryepts_250m_ll			 			|USDA 2014 class														|	|
|228	|TAXOUSDA_Cryids_250m_ll			 			|USDA 2014 class														|	|
|229	|TAXOUSDA_Cryods_250m_ll			 			|USDA 2014 class														|	|
|230	|TAXOUSDA_Cryolls_250m_ll			 			|USDA 2014 class														|	|
|231	|TAXOUSDA_Durids_250m_ll			 			|USDA 2014 class														|	|
|232	|TAXOUSDA_Fibrists_250m_ll			 			|USDA 2014 class														|	|
|233	|TAXOUSDA_Fluvents_250m_ll			 			|USDA 2014 class														|	|
|234	|TAXOUSDA_Folists_250m_ll			 			|USDA 2014 class														|	|
|235	|TAXOUSDA_Gelands_250m_ll			 			|USDA 2014 class														|	|
|236	|TAXOUSDA_Gelepts_250m_ll			 			|USDA 2014 class														|	|
|237	|TAXOUSDA_Gelods_250m_ll			 			|USDA 2014 class														|	|
|238	|TAXOUSDA_Gypsids_250m_ll			 			|USDA 2014 class														|	|
|239	|TAXOUSDA_Hemists_250m_ll			 			|USDA 2014 class														|	|
|240	|TAXOUSDA_Histels_250m_ll			 			|USDA 2014 class														|	|
|241	|TAXOUSDA_Humods_250m_ll			 			|USDA 2014 class														|	|
|242	|TAXOUSDA_Humults_250m_ll			 			|USDA 2014 class														|	|
|243	|TAXOUSDA_Ochrepts_250m_ll			 			|USDA 2014 class														|	|
|244	|TAXOUSDA_Orthels_250m_ll			 			|USDA 2014 class														|	|
|245	|TAXOUSDA_Orthents_250m_ll			 			|USDA 2014 class														|	|
|246	|TAXOUSDA_Orthods_250m_ll			 			|USDA 2014 class														|	|
|247	|TAXOUSDA_Perox_250m_ll			 				|USDA 2014 class														|	|
|248	|TAXOUSDA_Psamments_250m_ll						|USDA 2014 class														|	|
|249	|TAXOUSDA_Rendolls_250m_ll			 			|USDA 2014 class														|	|
|250	|TAXOUSDA_Salids_250m_ll			 			|USDA 2014 class														|	|
|251	|TAXOUSDA_Saprists_250m_ll			 			|USDA 2014 class														|	|
|252	|TAXOUSDA_Torrands_250m_ll			 			|USDA 2014 class														|	|
|253	|TAXOUSDA_Torrerts_250m_ll			 			|USDA 2014 class														|	|
|254	|TAXOUSDA_Torrox_250m_ll			 			|USDA 2014 class														|	|
|255	|TAXOUSDA_Turbels_250m_ll			 			|USDA 2014 class														|	|
|256	|TAXOUSDA_Udalfs_250m_ll			 			|USDA 2014 class														|	|
|257	|TAXOUSDA_Udands_250m_ll			 			|USDA 2014 class														|	|
|258	|TAXOUSDA_Udepts_250m_ll			 			|USDA 2014 class														|	|
|259	|TAXOUSDA_Uderts_250m_ll			 			|USDA 2014 class														|	|
|260	|TAXOUSDA_Udolls_250m_ll			 			|USDA 2014 class														|	|
|261	|TAXOUSDA_Udox_250m_ll			 				|USDA 2014 class														|	|
|262	|TAXOUSDA_Udults_250m_ll			 			|USDA 2014 class														|	|
|263	|TAXOUSDA_Ustalfs_250m_ll			 			|USDA 2014 class														|	|
|264	|TAXOUSDA_Ustands_250m_ll			 			|USDA 2014 class														|	|
|265	|TAXOUSDA_Ustepts_250m_ll			 			|USDA 2014 class														|	|
|266	|TAXOUSDA_Usterts_250m_ll			 			|USDA 2014 class														|	|
|267	|TAXOUSDA_Ustolls_250m_ll			 			|USDA 2014 class														|	|
|268	|TAXOUSDA_Ustox_250m_ll			 				|USDA 2014 class														|	|
|269	|TAXOUSDA_Ustults_250m_ll			 			|USDA 2014 class														|	|
|270	|TAXOUSDA_Vitrands_250m_ll			 			|USDA 2014 class														|	|
|271	|TAXOUSDA_Xeralfs_250m_ll			 			|USDA 2014 class														|	|
|272	|TAXOUSDA_Xerands_250m_ll			 			|USDA 2014 class														|	|
|273	|TAXOUSDA_Xerepts_250m_ll			 			|USDA 2014 class														|	|
|274	|TAXOUSDA_Xererts_250m_ll			 			|USDA 2014 class														|	|
|275	|TAXOUSDA_Xerolls_250m_ll			 			|USDA 2014 class														|	|
|276	|TAXOUSDA_Xerults_250m_ll			 			|USDA 2014 class														|	|
|277	|AWCh1_M_sl1_250m_ll			 				|Available soil water capacity (volumetric fraction) for h1				|	|
|278	|AWCh1_M_sl2_250m_ll			 				|Available soil water capacity (volumetric fraction) for h1				|	|
|279	|AWCh1_M_sl3_250m_ll			 				|Available soil water capacity (volumetric fraction) for h1				|	|
|280	|AWCh1_M_sl4_250m_ll			 				|Available soil water capacity (volumetric fraction) for h1				|	|
|281	|AWCh1_M_sl5_250m_ll			 				|Available soil water capacity (volumetric fraction) for h1				|	|
|282	|AWCh1_M_sl6_250m_ll			 				|Available soil water capacity (volumetric fraction) for h1				|	|
|283	|AWCh1_M_sl7_250m_ll			 				|Available soil water capacity (volumetric fraction) for h1				|	|
|284	|AWCh2_M_sl1_250m_ll			 				|Available soil water capacity (volumetric fraction) for h2				|	|
|285	|AWCh2_M_sl2_250m_ll			 				|Available soil water capacity (volumetric fraction) for h2				|	|
|286	|AWCh2_M_sl3_250m_ll			 				|Available soil water capacity (volumetric fraction) for h2				|	|
|287	|AWCh2_M_sl4_250m_ll			 				|Available soil water capacity (volumetric fraction) for h2				|	|
|288	|AWCh2_M_sl5_250m_ll			 				|Available soil water capacity (volumetric fraction) for h2				|	|
|289	|AWCh2_M_sl6_250m_ll			 				|Available soil water capacity (volumetric fraction) for h2				|	|
|290	|AWCh2_M_sl7_250m_ll			 				|Available soil water capacity (volumetric fraction) for h2				|	|
|291	|AWCh3_M_sl1_250m_ll			 				|Available soil water capacity (volumetric fraction) for h3				|	|
|292	|AWCh3_M_sl2_250m_ll			 				|Available soil water capacity (volumetric fraction) for h3				|	|
|293	|AWCh3_M_sl3_250m_ll			 				|Available soil water capacity (volumetric fraction) for h3				|	|
|294	|AWCh3_M_sl4_250m_ll			 				|Available soil water capacity (volumetric fraction) for h3				|	|
|295	|AWCh3_M_sl5_250m_ll			 				|Available soil water capacity (volumetric fraction) for h3				|	|
|296	|AWCh3_M_sl6_250m_ll			 				|Available soil water capacity (volumetric fraction) for h3				|	|
|297	|AWCh3_M_sl7_250m_ll			 				|Available soil water capacity (volumetric fraction) for h3				|	|
|298	|WWP_M_sl1_250m_ll			 					|Available soil water capacity (volumetric fraction) until wilting point|	|
|299	|WWP_M_sl2_250m_ll			 					|Available soil water capacity (volumetric fraction) until wilting point|	|
|300	|WWP_M_sl3_250m_ll			 					|Available soil water capacity (volumetric fraction) until wilting point|	|
|301	|WWP_M_sl4_250m_ll			 					|Available soil water capacity (volumetric fraction) until wilting point|	|
|302	|WWP_M_sl5_250m_ll			 					|Available soil water capacity (volumetric fraction) until wilting point|	|
|303	|WWP_M_sl6_250m_ll			 					|Available soil water capacity (volumetric fraction) until wilting point|	|
|304	|WWP_M_sl7_250m_ll			 					|Available soil water capacity (volumetric fraction) until wilting point|	|
|305	|AWCtS_M_sl1_250m_ll			 				|Saturated water content (volumetric fraction) for tS					|	|
|306	|AWCtS_M_sl2_250m_ll			 				|Saturated water content (volumetric fraction) for tS					|	|
|307	|AWCtS_M_sl3_250m_ll			 				|Saturated water content (volumetric fraction) for tS					|	|
|308	|AWCtS_M_sl4_250m_ll			 				|Saturated water content (volumetric fraction) for tS					|	|
|309	|AWCtS_M_sl5_250m_ll			 				|Saturated water content (volumetric fraction) for tS					|	|
|310	|AWCtS_M_sl6_250m_ll			 				|Saturated water content (volumetric fraction) for tS					|	|
|311	|AWCtS_M_sl7_250m_ll			 				|Saturated water content (volumetric fraction) for tS					|	|
|312	|HISTPR_250m_ll			 						|Histosols probability cumulative										|	|
|313	|SLGWRB_250m_ll			 						|Sodic soil grade														|	|
|314	|ACDWRB_M_ss_250m_ll			 				|Acid sub-soils grade													|	|

Apart from the table above, a complete description of the variables are found in the following files available on the hosting `Soil Grids V1` website: 1) [`META_GEOTIFF_1B.csv`](https://files.isric.org/soilgrids/former/2017-03-10/data/META_GEOTIFF_1B.csv) that details all the files included in the dataset, 2) [`TAXNWRB_250m_ll.tif.csv`](https://files.isric.org/soilgrids/former/2017-03-10/data/TAXNWRB_250m_ll.tif.csv), and 3) [`TAXOUSDA_250m_ll.tif.csv`](https://files.isric.org/soilgrids/former/2017-03-10/data/TAXOUSDA_250m_ll.tif.csv).
