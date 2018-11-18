R code for QCing the SOTS PAR sensor data

Packages needed

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

source('C:/Users/jan079/ABOS/git/sots/R/PAR/Functions for organising PAR data.R', encoding = 'UTF-8')
source('C:/Users/jan079/ABOS/git/sots/R/PAR/Importing and organising PAR data.R', encoding = 'UTF-8')

