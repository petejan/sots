library(RNetCDF)
library(zoo)
library(plyr)
library(ggplot2)
library(suncalc)
library(dplyr)
library(data.table)
library(scales)
library(grDevices)
library(lubridate)
library(tidyr)
library(oce)
library(gtable)
library(grid)
library(LakeMetabolizer)
library(ggpubr)

#Importing data from netcdf file
  allPAR <- read.nc(open.nc("IMOS_ABOS-SOTS_F_20090928_SOFS_FV01_SOFS-1-2010-PAR-SR-SW-DiscreteGeometries_END-20160413_C-20181128.nc"))
  instanceSplit <- strsplit(allPAR$station_name, ":")
  
#Isolating the useful information from the netcdf
  PARandsensor <- data.frame(time = allPAR$TIME, sensor = allPAR$stationIndex, par = allPAR$PAR,par_qc = allPAR$PAR_quality_code,solrad = allPAR$cSR,sw = allPAR$SW)
  
#Index numbers and stations names are offset by 1
  PARandsensor$sensor <- PARandsensor$sensor + 1

#Specifying the mooring of each data point
  PARandsensor$deployment <- unlist(lapply(PARandsensor$sensor,mooringfromsensor))
  
#Specifying the depth of each data point
  PARandsensor$depth <- unlist(lapply(PARandsensor$sensor,depthfromsensor))
  
#Specifying coordinates for each point
  PARandsensor$lat <- sapply(PARandsensor$sensor,latitudefromsensor)
  PARandsensor$lon <- sapply(PARandsensor$sensor,longitudefromsensor)
  
#Removing all out of water data flagged 5 for analysis, will be re added afterwards
  badqcPARandsensor <- subset(PARandsensor,PARandsensor$par_qc == 5)
  PARandsensor <- subset(PARandsensor,PARandsensor$par_qc != 5)

  
#all the good and bad data
  #allthePARdata <- data.frame(time = allPAR$TIME, sensor = allPAR$stationIndex, par = allPAR$PAR,par_qc = allPAR$PAR_quality_code,solrad = allPAR$cSR)
    #allthePARdata$sensor <- allthePARdata$sensor + 1
    #allthePARdata$deployment <- unlist(lapply(allthePARdata$sensor,mooringfromsensor))
    