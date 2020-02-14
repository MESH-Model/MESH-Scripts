### prueba
rm(list=ls())
library("ggplot2")
library("lubridate")
library("dplyr")
library("tidyverse")
library("DataCombine")


#preparing "environment"

pathR <- "C:/Path/To/Parent/Folder/Of/Scenario/Subfolders/" #Specify the parent folder of the scenario subfolders
    # Note: Scenario names are read from the names of the subfolders

name_dir_short <- list.files(pathR, pattern = "_")
name_dir       <- list.files(pathR, pattern = "_",full.names = TRUE)
n_dir          <- length(name_dir)
WB             <- data.frame(3)

#loading every file first for for directories and second for every csv file  
for (i in 1:n_dir) {
  files          <- list.files(name_dir[i], pattern = "*.csv$",full.names = TRUE,recursive=TRUE)
  m              <- length(files)
  
  for (j in 1:m) {
  WBT            <- read.csv(files[j],sep=",")
  WBT$X          <- NULL
   if (j == 1 && i == 1) {
     WB          <- WBT[1:2]
   }
  WBT$YEAR       <- NULL
  WBT$DAY        <- NULL
  colnames(WBT)  <- paste0(name_dir_short[i],"-",names(WBT))
  WB             <- cbind(WB, WBT)
  }
}
namescols    <- colnames(WB)
colnames(WB) <- gsub("-", ".", namescols)

write.csv(WB, file = paste0(pathR,"/plots.csv"))
