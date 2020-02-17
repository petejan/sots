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
#library(LakeMetabolizer)

#Importing data from netcdf file
  source_nc_file <- "IMOS_ABOS-SOTS_F_20090928_SOFS_FV01_SOFS-1-2010-PAR-SWR-cSR-DiscreteGeometries_END-20160413_C-20200215.nc"
  allPAR <- read.nc(open.nc(source_nc_file))
  instanceSplit <- strsplit(allPAR$station_name, ":")
  
#Import the SWR data
#  allSW <- read.nc(open.nc("IMOS_ABOS-SOTS_F_20100318_SOFS_FV01_SOFS-1-2010-SW-DiscreteGeometries_END-20171101_C-20180604.nc"))
  
#Isolating the useful information from the netcdf
  PARandsensor <- data.frame(time = allPAR$TIME, 
                             sensor = allPAR$stationIndex, 
                             par = allPAR$PAR, 
                             par_qc = allPAR$PAR_quality_code,
                             solrad = allPAR$cSR)
  
#Index numbers and stations names are offset by 1
  PARandsensor$sensor <- PARandsensor$sensor + 1

#Specifying the mooring of each data point
  PARandsensor$deployment <- unlist(lapply(PARandsensor$sensor, mooringfromsensor))
  
#Specifying the depth of each data point
  PARandsensor$depth <- unlist(lapply(PARandsensor$sensor, depthfromsensor))
  
#Specifying coordinates for each point
  PARandsensor$lat <- sapply(PARandsensor$sensor, latitudefromsensor)
  PARandsensor$lon <- sapply(PARandsensor$sensor, longitudefromsensor)
  
#Removing all out of water data flagged 5 for analysis, will be re added afterwards
  badqcPARandsensor <- subset(PARandsensor, PARandsensor$par_qc == 5)
  PARandsensor <- subset(PARandsensor, PARandsensor$par_qc != 5)

  # add dates, for plotting
  PARandsensor$dates <- as.Date(PARandsensor$time, origin="1950-01-01")
  
#all the good and bad data
  allthePARdata <- data.frame(time=allPAR$TIME, 
                              sensor=allPAR$stationIndex, 
                              par=allPAR$PAR,
                              par_qc=allPAR$PAR_quality_code,
                              solrad=allPAR$cSR)
  
  allthePARdata$sensor <- allthePARdata$sensor + 1
  allthePARdata$deployment <- unlist(lapply(allthePARdata$sensor, mooringfromsensor))

  
  