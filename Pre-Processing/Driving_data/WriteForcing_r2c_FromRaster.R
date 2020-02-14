# This script is used to extract data from a raster file using a shape of points and write the .r2c file for forcing data.
# The example used in this script is for basin precip., but can be adapted for other parameters as well.

# This script was written by Pedro Arboleda and adapted by Roger Guio from the Universidad Nacional de Colombia (National University of Colombia)

rm(list=ls(all=T))
library(raster)
library(ncdf4)
library(maptools)
library(rgdal)
library(doParallel)
library(foreach) 

#-------------------------------------------------------------------------------------
# Define function to write .r2c file
toR2Cformat<-function (filedir,xmin,ymin,csize,dimen,Rain,ini){
  
  rw<-dimen[1]
  cl<-dimen[2]
  
  dater2c<-seq(ISOdate(1980,1,1,12), ISOdate(2012,12,31,21), "3 hour")
  dater2c<-dater2c-12*60*60
  
  idfile<-file(filedir,'w')
  cat('########################################','\n',file=idfile)
  cat(':FileType r2c  ASCII  EnSim 1.0','\n',file=idfile)
  cat('#\n',file=idfile)
  cat('# DataType               2D Rect Cell\n',file=idfile)
  cat('#\n',file=idfile)
  cat(':Application             RSTUDIO\n',file=idfile)
  cat(':Version                 1.0.136\n',file=idfile)
  cat(':WrittenBy               Pedro Arboleda - Unal/UofS\n',file=idfile)
  cat(':CreationDate            ',as.character(Sys.time()),'\n',file=idfile)
  cat('#\n',file=idfile)
  cat('#---------------------------------------\n',file=idfile)
  cat(':AttributeName    Precip_rate\n',file=idfile)
  cat(':AttributeUnit           mm/s',file=idfile)
  cat('#\n',file=idfile)
  cat(':Projection              LATLONG\n',file=idfile)
  cat(':Ellipsoid               WGS84\n',file=idfile)
  cat('#\n',file=idfile)
  cat(':xOrigin                  ',as.character(xmin),'\n',file=idfile)
  cat(':yOrigin                  ',as.character(ymin),'\n',file=idfile)
  cat(':SourceFile              XXXX/           X\n',file=idfile)
  cat('#\n',file=idfile)
  cat(':xCount                  ',as.character(cl),'\n',file=idfile)
  cat(':yCount                  ',as.character(rw),'\n',file=idfile)
  cat(':xDelta                   ',as.character(csize),'\n',file=idfile)
  cat(':yDelta                   ',as.character(csize),'\n',file=idfile)
  cat('#\n',file=idfile)
  cat('#\n',file=idfile)
  cat(':endHeader\n',file=idfile)
  countframe<-1+ini-1
  for (i in 1:dimen[3]){
    print(i)
    datefr<-strftime(dater2c[i+ini-1]+6*60*60,"%Y-%m-%d %H:%M:%S.000")
    cat(':Frame        ',as.character(countframe),'       ',as.character(countframe),' "',datefr,'"\n',file=idfile)
    for (j in 1:dimen[1]){
      dumvar<-as.character(Rain[(dimen[1]+1-j),,i])
      
      dumvar[which(is.na(dumvar))]<-'0.0'
      
      cat(dumvar,'\n',file=idfile)
    }
    cat(':EndFrame','\n',file=idfile)
    countframe<-countframe+1
  }
  close(idfile)
  
  print('.R2C file created')
}

rasterOptions(maxmemory = 1e+11)

startw<-Sys.time()
ini<-1
en<-12054

start<-Sys.time()

# infile<-"D:/Roger/Revision_Info/PruebR/Coello_PP/"
infile <- "C:/Path/To/Folder/Containing/Raster/Files/Of/Driving/Data"
inprecip<-paste(infile,list.files(infile),sep = '')
#intempmin<-stack(file[ini:en],quick=T)


end<-Sys.time()
difftime(end,start)

## Create the dates vector
date<-seq.Date(as.Date("1980/01/01"),as.Date("2012/12/31"),'day')

# Load a raster and read its size characteristics
muestra<-raster(inprecip[1])

## Set the time-step
h<-3
hd<-24/h
ndays<-length(inprecip)

## Output raster stack
## Create the array that will contain the rasters

dimen<-dim(muestra)[1:2]
dimen<-c(dimen,ndays*hd)
Rain<-array(dim = dimen)
print('Calculating intra diurnal values -- Step 3')

for (i in 1:ndays){
  cat("\014") 
  print(Sys.time())
  print(i)
  temp <- raster(inprecip[i]) # Temporary variable
  Rain[,,(i*hd-(hd-1)):(i*hd)]<-as.matrix(temp)/24/3600
}

## Create the .r2c file which will be read by MESH
## Below is the information that will be needed by the file
## and then call the function to create the file (function defined above)

rw<-nrow(muestra)
cl<-ncol(muestra)
xmin<--75.55
ymin<-4.268
xmax<--75.15
ymax<-4.568
spref<-'+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs '
csize<-0.1
print('Passing to new format -- Step 4')

filedir<-'C:/Path/To/Folder/Containing/Forcing/Files/basin_rain.r2c'  ## Define the file path and name for the file

toR2Cformat(filedir,xmin,ymin,csize,dimen,Rain,ini)

endw<-Sys.time()
difftime(endw,startw)