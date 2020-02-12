# This script is for plotting the water balance as a set of two bar plots per scenario and allows for comparison between scenarios

# Written by Roger Guio, Master's Student at the Universidad Nacional de Colombia (National University of Colombia), 2019
# Minor modifications by Haley Brauner, Feb. 2020

#libraries
library("ggplot2")
library("lubridate")
library("dplyr")
library("tidyverse")
library("DataCombine")
library("digest")

rm(list=ls())

# USER INPUTS
pathR <- "C:/Path/To/Parent/Folder/Housing/Subfolder/For/Each/Scenario"
basinArea <- 1530 # km2

#Load files
filesBal <- list.files(pathR, pattern = "water_balance.csv",full.names = TRUE,recursive=TRUE)
filesQ   <- list.files(pathR, pattern = "streamflow.csv",full.names = TRUE,recursive=TRUE)

# Building the table
tplot   <- data.frame()
n <- length(filesBal)
labs <- list.files(pathR, pattern = "_") # labels

# i=4
# filesQ[i]
# Stream <- read.csv(filesQ[i],header = TRUE)
# Stream[Stream<0] <- NA
# Qo <- Stream$QOMEAS1
# Qs <- Stream$QOSIM1
# round(1- sum((Qs-Qo)^2,na.rm=TRUE)/sum((Qo-mean(Qo, na.rm=TRUE))^2, na.rm=TRUE),2)

for (i in 1:n){
  Meteo  <- read.csv(filesBal[i],header = TRUE)
  Stream <- read.csv(filesQ[i],header = TRUE)
  Stream[Stream<0]<- NA
  
  Qo <- Stream$QOMEAS1
  Qs <- Stream$QOSIM1
  NSE <- round(1- sum((Qs-Qo)^2,na.rm=TRUE)/sum((Qo-mean(Qo, na.rm=TRUE))^2, na.rm=TRUE),2)
  NSE <- paste0("NSE=",NSE)
  
  tplot1   <- data.frame(sum(Meteo$PRE)/length(Meteo$PRE)*365,
                         paste0(labs[i]),"P","I m",NSE,filesBal[i])
  tplot2   <- data.frame(sum(Meteo$EVAP)/length(Meteo$EVAP)*365,
                         paste0(labs[i]),"ET","O m",NSE,filesBal[i])
  tplot3   <- data.frame(sum(Meteo$ROFO)/length(Meteo$ROFO)*365,
                         paste0(labs[i]),"OVERLANDFLOW","O m",NSE,filesBal[i])
  tplot4   <- data.frame(sum(Meteo$ROFS)/length(Meteo$ROFS)*365,
                         paste0(labs[i]),"INTERFLOW","O m",NSE,filesBal[i]) 
  tplot5   <- data.frame(sum(Meteo$ROFB)/length(Meteo$ROFB)*365,
                         paste0(labs[i]),"BASEFLOW","O m",NSE,filesBal[i]) 
  for(j in 1:5){
    newcol <- eval(parse(text=paste0("tplot",j)))
    colnames(newcol) <- c("data","Scenery","process","kind","NSE")
    tplot <- rbind(tplot, newcol)
  }
}
AvgQMeas   <- sum(Stream$QOMEAS1, na.rm=TRUE)/length(Stream$QOMEAS1)*365*3600*24/basinArea/1000 # Calculates average annual streamflow in mm using basin area

# plotting
ggplot(tplot, aes(x = kind, y = data, fill = process)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~Scenery+NSE) + ggtitle("Model Balance") +
  xlab("") + ylab("mm/year")+ 
  geom_abline(intercept = AvgQMeas, slope = 0, colour = "blue")

