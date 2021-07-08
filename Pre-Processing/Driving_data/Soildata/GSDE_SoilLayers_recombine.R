# Description  
#
#  The purpose of this script is to read extracted soil layers (e.g., CLAY, SAND, Organic) 
#  of the domain of interest. Then, based on GSDE soil layer depths, 4 or 3 soil layers 
#  required for running MESH model are generated. These layers thereafter can be resampled
#  to MESH grid resolution which can be used for generating MESH parameters.  
#
# Input         
#               Soil texture layers (e.g., Fraser_gsde_ly1.tif) of domain 
#
#
# Output        extracted Soil layers required for MESH
#
#
# Reference     
#             Shangguan, W., Dai, Y., Duan, Q., Liu, B., Yuan, H., 2014. A global soil data set for 
#             earth system modeling. J. Adv. Model. Earth Syst. 6, 249–263.
#             https://doi.org/10.1002/2013MS000293
#
#             http://globalchange.bnu.edu.cn/research/soilw
#
#             Shangguan, W., Hengl, T., Mendes de Jesus, J., Yuan, H., Dai, Y., 2017. Mapping the global 
#             depth to bedrock for land surface modeling. J. Adv. Model. Earth Syst. 9, 65–88.
#             https://doi.org/10.1002/2016MS000686
#
#             http://globalchange.bnu.edu.cn/research/dtb.jsp
#
# See also: 
#
# Author: Ala Bahrami       
#
# Created Date: 05/21/2021
#                          
# Copyright (C) 2021 Ala Bahrami  
#
# loading libs ------------------------------
library(ncdf4)
library(raster)
library(rgdal) 
library(raster)
library(shapefiles)
library(progress)
source("Extract_GSDE.R")
source("maskgenerate.R")
# Setting inputs -------------
nrow    <- 64 +2
ncol    <- 81 +2 
res     <- 0.125
xmin    <- -128.125 - 1*res
ymin    <- 48.5 - 1*res
texture <- TRUE
indir  <- "D:/Data/SoilData/"
varlist <- c("CLAY","SAND","OC", "SDEP")

#GSDE Soil Layer depths
depth    <- c(0.045, 0.046, 0.075, 0.123, 0.204, 0.336, 0.554, 0.913) 

### Extracting Clay layers --------
var    <-  varlist[1]
Extract_GSDE(nrow , ncol , res ,
             xmin , ymin , 
             indir, var, texture)

### Extracting SAND layers --------
var   <-  varlist[2]
Extract_GSDE(nrow , ncol , res ,
             xmin , ymin , 
             indir, var, texture)

### Extracting Organic layers --------
var   <-  varlist[3]
Extract_GSDE(nrow , ncol , res ,
             xmin , ymin , 
             indir, var, texture)

### Extracting and construct MESH-SDEP layer --------
texture <- FALSE
var     <-  varlist[4]
Extract_GSDE(nrow , ncol , res ,
             xmin , ymin , 
             indir, var, texture)

### construct 4 MESH-CLAY/SAND/OC layers for domain of interest -----
pb <- progress_bar$new(
      format = "  Constructing MESH Soil layers [:bar] :percent eta: :eta",
      total = 3, clear = FALSE, width= 70)

for (j in 1:3) {
  
      pb$tick()
      Sys.sleep(1 / 3)    
      var     <-  varlist[j]
      
      stdir   <- paste(indir, var, "/", sep = "") 
      str1    <- paste(stdir, "Fraser_", var, "1_ly", sep = "")
      str2    <- paste(stdir, "Fraser_", var, "2_ly", sep = "")
      
      for (i in 1:4){
        
        str3    <- sprintf("%d", i)
        str_1   <- paste(str1, str3,".tif" ,sep = "")
        str_2   <- paste(str2, str3,".tif" ,sep = "")
        ly1     <- raster(str_1)
        ly2     <- raster(str_2)
        
        if (i ==1){
          nr <- nrow(ly1)
          nc <- ncol(ly1)
          gsde.array <- array(1 : nr*nc*8, dim = c(nr, nc, 8))
        }
        
        # subsitude NA values with the mean value for gsde if required 
        cl1                 <- values(ly1)
        #cl1_mean            <- mean(cl1, na.rm=TRUE)
        #cl1[is.na(cl1)]     <- cl1_mean
        cl1                 <- t(matrix(cl1, nc, nr))
        gsde.array[ , , i]  <- cl1
        plot(raster(gsde.array[ , , i]), main = paste("Fraser_", var, "1_ly","Layer", i))
        
        # subsitude NA values with mean for clay2 
        cl2             <- values(ly2)
        #cl2_mean        <- mean(cl2, na.rm=TRUE)
        #cl2[is.na(cl2)] <- cl2_mean
        cl2             <- t(matrix(cl2, nc, nr))
        gsde.array[ , , i+4] <- cl2
        plot(raster(gsde.array[ , , i+4]), main = paste("Fraser_", var, "2_ly","Layer", i))
        
      }
      
      if (var == "CLAY"){
        # Obtain 4 clay layers from 8 extracted GSDE layers  
        clay_MESH.array         <- array(1 : nr*nc*4, dim = c(nr, nc, 4))
        clay_MESH.array[ , , 1] <- (gsde.array[ , , 1] * depth[1] + gsde.array[ , , 2] * depth[2])/(depth[1] + depth[2])
        clay_MESH.array[ , , 2] <- (gsde.array[ , , 3] * depth[3] + gsde.array[ , , 4] * depth[4])/(depth[3] + depth[4])
        clay_MESH.array[ , , 3] <- (gsde.array[ , , 5] * depth[5] + gsde.array[ , , 6] * depth[6])/(depth[5] + depth[6])
        clay_MESH.array[ , , 4] <- (gsde.array[ , , 7] * depth[7] + gsde.array[ , , 8] * depth[8])/(depth[7] + depth[8])
      } 
      else if (var == "SAND"){
        # Obtain 4 SAND layers from 8 extracted GSDE layers  
        sand_MESH.array         <- array(1 : nr*nc*4, dim = c(nr, nc, 4))
        sand_MESH.array[ , , 1] <- (gsde.array[ , , 1] * depth[1] + gsde.array[ , , 2] * depth[2])/(depth[1] + depth[2])
        sand_MESH.array[ , , 2] <- (gsde.array[ , , 3] * depth[3] + gsde.array[ , , 4] * depth[4])/(depth[3] + depth[4])
        sand_MESH.array[ , , 3] <- (gsde.array[ , , 5] * depth[5] + gsde.array[ , , 6] * depth[6])/(depth[5] + depth[6])
        sand_MESH.array[ , , 4] <- (gsde.array[ , , 7] * depth[7] + gsde.array[ , , 8] * depth[8])/(depth[7] + depth[8])
      
      } 
      else {
        # Obtain 4 organic layers from 8 extracted GSDE layers  
        organic_MESH.array         <- array(1 : nr*nc*4, dim = c(nr, nc, 4))
        organic_MESH.array[ , , 1] <- (gsde.array[ , , 1] * depth[1] + gsde.array[ , , 2] * depth[2])/(depth[1] + depth[2])
        organic_MESH.array[ , , 2] <- (gsde.array[ , , 3] * depth[3] + gsde.array[ , , 4] * depth[4])/(depth[3] + depth[4])
        organic_MESH.array[ , , 3] <- (gsde.array[ , , 5] * depth[5] + gsde.array[ , , 6] * depth[6])/(depth[5] + depth[6])
        organic_MESH.array[ , , 4] <- (gsde.array[ , , 7] * depth[7] + gsde.array[ , , 8] * depth[8])/(depth[7] + depth[8])
        
      }
}

### export MESH-CLAY/SAND/OC layers--------------
extsoil <- extent(ly1)
dimsoil <- dim(ly1)
crssoil <- crs(ly1)

for (j in 1:3) {
  for (i in 1:4){
    var     <-  varlist[j]
    stdir   <- paste(indir, var, "/", sep = "") 
    str1    <- paste(stdir, "Fraser_MESH_", var, "Layer", sep = "")
    str3    <- sprintf("%d", i)
    str_1   <- paste(str1, str3, sep = "")
    
    if (j == 1){
      gsdesoil1             <- raster(clay_MESH.array[ , , i])
      extent(gsdesoil1)     <- extsoil
      dim(gsdesoil1)        <- dimsoil
      crs(gsdesoil1)        <- crssoil
      
      writeRaster(gsdesoil1, str_1, "GTiff", overwrite = TRUE)
      
    }
    else if (j ==2){
      gsdesoil1             <- raster(sand_MESH.array[ , , i])
      extent(gsdesoil1)     <- extsoil
      dim(gsdesoil1)        <- dimsoil
      crs(gsdesoil1)        <- crssoil
      
      writeRaster(gsdesoil1, str_1, "GTiff", overwrite = TRUE)
      
    }
    else {
      gsdesoil1             <- raster(organic_MESH.array[ , , i])
      extent(gsdesoil1)     <- extsoil
      dim(gsdesoil1)        <- dimsoil
      crs(gsdesoil1)        <- crssoil
      
      writeRaster(gsdesoil1, str_1, "GTiff", overwrite = TRUE)
      
    }
    
  }
  
}
