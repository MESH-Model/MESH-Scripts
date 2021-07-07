Extract_GSDE <- function(nrow, ncol, res, xmin, ymin, indir, var, texture) {
  # Description  
  #
  #  The purpose of this script is to read soil datasets (8 layers) from
  #  GSDE soil dataset and convert them to four layers required for MESH
  #  The geographical boundary is cliped to region of interest. When soil  
  #  parameters are extracted for the basin of interested, then these input 
  #  files can be used for the generation of MESH parameters.
  #
  # Input         
  #               nrow              number of rows of a domain of interest
  #               ncol              number of columns of a domain of interest
  #               res               resolution of grids 
  #               xmin              longitudinal lower corner coordinates of domain 
  #               ymin              latitudinal  lower corner coordinates of domain
  #               indir             input directory of datasets 
  #               var               variable of interest
  #               texture           a flag indicating whether the texture data is read or d
  #                                 depth to bed rock
  #
  # Output        extracted Soil layers for the domain of interest
  #
  #
  # Reference     
  #
  #
  # See also: 
  #
  # Author: Ala Bahrami       
  #
  # Created Date: 03/09/2021
  # 
  # Copyright (C) 2021 Ala Bahrami  
  #
  ### loading libs ------------------------------
  library(ncdf4)
  library(raster)
  library(rgdal) 
  library(raster)
  library(shapefiles)
  source("maskgenerate.R")
  
  ### setting dir -------------------------------
  rm(list=ls(all=TRUE))
  setwd("D:/programing/R/GSDE")
  stdir <- paste(indir, var, "/", sep = "")
  
  ### generates mask for a region of interest 
  mask <- create_mask(nrow, ncol, res, xmin, ymin)
  
  if (texture){
      ### reading inputs info
      GSDE1 <- paste(stdir, var, "1.nc", sep = "")
      GSDE2 <- paste(stdir, var, "2.nc", sep = "")
      
      #### 
      nc_data1   <- nc_open(GSDE1, write=FALSE)
      {
        sink(paste(stdir, var, "1_metadata.txt", sep = ""))
        print(nc_data1)
        sink()
      }
      
      nc_data2   <- nc_open(GSDE2, write=FALSE)
      {
        sink(paste(stdir, var, "2_metadata.txt", sep = ""))
        print(nc_data2)
        sink()
      }
      
      ### reading input nc file
      # geographical info
      lon             <- ncvar_get(nc_data1, "lon")
      lat             <- ncvar_get(nc_data1, "lat")
      
      # data array
      # use as an input 
      gsde1.array     <- ncvar_get(nc_data1, var)
      gsde2.array     <- ncvar_get(nc_data2, var)
      
      # verify fill value is used 
      # here also 
      fillvalue1  <- ncatt_get(nc_data1, var, "_FillValue")
      fillvalue2  <- ncatt_get(nc_data2, var, "_FillValue")
      
      # close Netcdf files
      nc_close(nc_data1)
      nc_close(nc_data2)
      
      ### get GSDE1,2 LAYERs for a desired basin 
      row         <- dim(gsde1.array)[1] 
      col         <- dim(gsde1.array)[2] 
      reso        <- lon[2] - lon[1]
      
      # loop over four layers of gsde 1,2
      for (i in 1:4){
        
        # gsde 1
        gsde1_arr        <- gsde1.array[ , , i]
        if (var == "OC"){
          gsde1_arr <- gsde1_arr/(100 * 0.58) 
        }
        rgsde1           <- raster(t(gsde1_arr))
        extent(rgsde1)   <- c(xmn=min(lon), xmx=max(lon),ymn=min(lat), ymx=max(lat))
        dim(rgsde1)      <- c(col,row)
        crs(rgsde1)      <- crs(mask)
        
        # gsde 2
        gsde2_arr       <- gsde2.array[ , , i]
        if (var == "OC"){
          gsde2_arr <- gsde2_arr/(100 * 0.58) 
        }
        
        rgsde2          <- raster(t(gsde2_arr))
        extent(rgsde2)  <- c(xmn=min(lon), xmx=max(lon),ymn=min(lat), ymx=max(lat))
        dim(rgsde2)     <- c(col,row)
        crs(rgsde2)     <- crs(mask)
        
        plot(rgsde1)
        plot(rgsde2)
        
        # saving to geotiff 
        # writeRaster(rclay1, "rclay11", "GTiff", overwrite = TRUE)
        
        # crop and save raster clay layer 
        rgsde1_crop <- crop(rgsde1, mask)
        # need to be set
        plot(rgsde1_crop, main = paste("Cropped", var, "Layer", "1"))
        
        # crop and save raster clay layer 
        rgsde2_crop <- crop(rgsde2, mask)
        plot(rgsde2_crop, main = paste("Cropped", var, "Layer", "2"))
        
        # need to be set 
        str1   <- paste(stdir, "Fraser_", var, "1_ly", sep = "")
        str2   <- paste(stdir, "Fraser_", var, "2_ly", sep = "")
        str3   <- sprintf("%d", i)
        str_1  <- paste(str1, str3, sep = "")
        str_2  <- paste(str2, str3, sep = "")
        
        writeRaster(rgsde1_crop, str_1, "GTiff", overwrite = TRUE)
        writeRaster(rgsde2_crop, str_2, "GTiff", overwrite = TRUE)
      }
      
  }
  else {
       # reading SDEP data 
      st1       <- paste(stdir, "BDTICM_M_250m_ll.tif", sep = "")
      sdep      <- raster(st1)
      crs(sdep) <- crs(mask)
      # clipping and converting to m 
      sdep_crop <- crop(sdep, mask)
      sdep_crop <- sdep_crop/100
      # exporting data
      st2       <- paste(stdir, "Fraser_","MESH_", var, sep = "")
      writeRaster(sdep_crop, st2, "GTiff", overwrite = TRUE)
      
    }
  
  }