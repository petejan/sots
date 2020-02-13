
PARandsensor$dates <- as.Date(PARandsensor$time,origin = "1950-01-01")

#Gross range test

  #using data from import
  grPARandsensor <- PARandsensor
  
  #Applying gross range test
  grPARandsensor$flags <- sapply(grPARandsensor$par,
                                 grossrangetest,
                                 sensormin = 0,
                                 sensormax = 10000,
                                 opmin = 0,
                                 opmax = 4500)
  
  grPARandsensor$gr_flags <- grPARandsensor$flags # save flags for later debug
  
  #Getting flag counts
  grflagcounts <- data.frame(good =sum(grPARandsensor$flags == 1),
                             fail = sum(grPARandsensor$flags == 4),
                             suspect = sum(grPARandsensor$flags == 3),
                             uneval = sum(grPARandsensor$flags == 2))
  
#Climatology test

  kd490high <- 0.0166 + 0.07242*1^0.68955
  kd490low <- 0.0166 + 0.07242*0.5^0.68955
  
  kdPARhigh <- 0.0864 + 0.884*kd490high - 0.00137*(kd490high)^-1
  kdPARlow <- 0.0864 + 0.884*kd490low - 0.00137*(kd490low)^-1
  
  grPARandsensor$ePARhigherchl <- apply(grPARandsensor, 1, climatologyPARfromKd, kd=kdPARhigh)
  grPARandsensor$ePARhigherchl <- as.numeric(grPARandsensor$ePARhigherchl)
  grPARandsensor$ePARlowerchl <- apply(grPARandsensor, 1, climatologyPARfromKd, kd=kdPARlow)
  grPARandsensor$ePARlowerchl <- as.numeric(grPARandsensor$ePARlowerchl)
  
  #Using output of gross range test
  clPARandsensor <- grPARandsensor[order(grPARandsensor$time),]
  
  #adding estimated PAR to data 
  clPARandsensor$ePAR <- apply(clPARandsensor, 1, climatologyPARfromKd, kd=0.04)
  clPARandsensor$ePAR <- as.numeric(clPARandsensor$ePAR)
  
  #Applying test to generate flags
  clflags <- apply(clPARandsensor, 1, applyclimatologytest)
  
  #Add flags to data
  clPARandsensor$flags <- addflags(clPARandsensor$flags, clflags)
  
  #clPARandsensor$floortimes <- floor(clPARandsensor$time)
  
  #clbaddays <- subset(clPARandsensor,clPARandsensor$flags==3)
  
  #clPARandsensor$flags[clPARandsensor$floortimes %in% clbaddays$floortimes] <- 3
  
  clPARandsensor$cl_flags <- clflags  # save for later debug
  
  #Getting flag counts
  clflagcounts <- data.frame(good = sum(clflags ==1), 
                             fail = sum(clflags == 4),
                             suspect = sum(clflags == 3),
                             uneval = sum(clflags == 2))
  
  #ensuring data is ordered chronologically
  clPARandsensor <- clPARandsensor[order(clPARandsensor$time),]

  
    
#Flat line test
  
    #Using output of flat line test
    flPARandsensor <- clPARandsensor
    
    #Isolating day time data
    dayflPARandsensor <- daydataPAR(flPARandsensor)
    
    #Running the flat line test
    flflags <- by(dayflPARandsensor,dayflPARandsensor$sensor, function(x) 
    {
      c(NA,rollapply(x$par,FUN = flatlinetest, width=5, fill=NA, align='right', tolerance=0))
    })
    
    #adding flags back into day data
    dayflPARandsensor <- separatesensorflags(dayflPARandsensor, flflags)
    
    
    #Merging day time data back into full dataset
    flPARandsensor <- merge(flPARandsensor,dayflPARandsensor, by = c("time",
                                                                     "sensor",
                                                                     "par",
                                                                     "par_qc",
                                                                     "deployment",
                                                                     "depth",
                                                                     "lat",
                                                                     "lon")
                            , all.x = TRUE)
    
    #Replacing unevaluated flags with a value of 2
    flPARandsensor$flags.y[is.na(flPARandsensor$flags.y)] <- 2
    
    flflagslong <- flPARandsensor$flags.y
    
    #Adding daytime flags to entire flag data
    flPARandsensor$flags <- addflags(flPARandsensor$flags.x, flPARandsensor$flags.y)
    
    #removing flag file
    rm(flflags)
    
    flPARandsensor$fl_flags <- flPARandsensor$flags.y  # save for later debug
    
    #Getting flag counts
    flflagcounts <- data.frame(good = sum(flPARandsensor$flags.y == 1),
                               fail = sum(flPARandsensor$flags.y == 4),
                               suspect = sum(flPARandsensor$flags.y == 3),
                               uneval = sum(flPARandsensor$flags.y == 2))
    
    #Removing unnecessary columns from data
    flPARandsensor <- flPARandsensor[,-c(9,11,12,13,14,15,16)]
    
    #ensuring data is ordered chronologically
    flPARandsensor <- flPARandsensor[order(flPARandsensor$time),]

#Neighbour test

  #Isolating daily means to make this test more simple
  dailymeanPAR <- flPARandsensor
  dailymeanPAR$time <- floor(dailymeanPAR$time)
  dailymeanPAR <- ddply(dailymeanPAR,.variables = 'sensor', testdailymeans)
  dailymeanPAR <- daysoftheyear(dailymeanPAR)
  dailymeanPAR$depth <- unlist(lapply(dailymeanPAR$sensor, depthfromsensor))
  dailymeanPAR$sensorname <- sapply(dailymeanPAR$sensor, sensorname)
  
  #neighbour test for pulse moorings
  neighbourtestPULSE <- subset(dailymeanPAR, dailymeanPAR$sensor %in% c(1,2,3,4,7,8,9,10,11,12,13,14,19,23,24,25,26,30,31,32))
  neighbourtestPULSE <- neighbourtestPULSE[order(neighbourtestPULSE$day, neighbourtestPULSE$depth),]
  
  #Comducting the neighbour test, output is in TRUE/FALSE from
  neighbourtestlogicPULSE <- creatingdaylists(neighbourtestPULSE)
  
  
  #Converting logic of test into flags
  neighbourflagsPULSE <- gettingflagsfromlogic(neighbourtestlogicPULSE)
  
  
  #Add flags back to original dataset
  neighbourflagschronPULSE <- unlist(neighbourflagsPULSE)
  neighbourtestPULSE$flags <- neighbourflagschronPULSE
  
  #Need to change flags that are due to two sensors being at the same depth
  neighbourtestPULSE$flags[neighbourtestPULSE$depth == 50 & neighbourtestPULSE$deployment == "Pulse 10"] <- 1
  
  neighbourtestPULSE$floortimes <- neighbourtestPULSE$day
  neighbourtestPULSE$addingmore3 <- NA
  
  #Ensuring that if a sensor drops below one other sensor, both are flagged as suspicious
  for (x in seq(1,length(neighbourflagschronPULSE)))
  {
    if (neighbourtestPULSE$flags[x] == 3)
    {
      
      neighbourtestPULSE$addingmore3[x] <- 3
    }
  }
  
  for (x in seq(2,length(neighbourflagschronPULSE)))
  {
    if (isTRUE(neighbourtestPULSE$flags[x-1] == 3 & neighbourtestPULSE$addingmore3[x-1] == 3 & neighbourtestPULSE$deployment[x] == neighbourtestPULSE$deployment[x-1]))
    {
      neighbourtestPULSE$flags[x] <- 3
    }
  }
  
  #neighbour test for SOFS moorings
  neighbourtestSOFS <- subset(dailymeanPAR,!(dailymeanPAR$sensor %in% c(1,2,3,4,7,8,9,10,11,12,13,14,19,23,24,25,26,30,31,32)))
  neighbourtestSOFS <- neighbourtestSOFS[order(neighbourtestSOFS$day,neighbourtestSOFS$depth),]
  
  #Comducting the neighbour test, output is in TRUE/FALSE from
  neighbourtestlogicSOFS <- creatingdaylists(neighbourtestSOFS)
  
  
  #Converting logic of test into flags
  neighbourflagsSOFS <- gettingflagsfromlogic(neighbourtestlogicSOFS)
  
  
  #Add flags back to original dataset
  neighbourflagschronSOFS <- unlist(neighbourflagsSOFS)
  
  
  neighbourtestSOFS$flags <- neighbourflagschronSOFS
  
  neighbourtestSOFS$floortimes <- neighbourtestSOFS$day
  neighbourtestSOFS$addingmore3 <- NA
  
  #Ensuring that if a sensor drops below one other sensor, both are flagged as suspicious
  for (x in seq(1,length(neighbourflagschronSOFS)))
  {
    if (neighbourtestSOFS$flags[x] == 3)
    {
      
      neighbourtestSOFS$addingmore3[x] <- 3
    }
  }
  
  for (x in seq(2,length(neighbourflagschronSOFS)))
  {
    if (isTRUE(neighbourtestSOFS$flags[x-1] == 3 & neighbourtestSOFS$addingmore3[x-1] == 3 & neighbourtestSOFS$deployment[x] == neighbourtestSOFS$deployment[x-1]))
    {
      neighbourtestSOFS$flags[x] <- 3
    }
  }
  
  
  #recombining separate Pulse and SOFS data
  neighbourtest <- rbind(neighbourtestPULSE,neighbourtestSOFS)
  
  #ensuring data is ordered chronologically
  neighbourtest <- neighbourtest[order(neighbourtest$day),]
  
  neighbourformerge <- data.frame(floortimes = neighbourtest$floortimes,
                                  sensor = neighbourtest$sensor,
                                  flags = neighbourtest$flags)
  
  # Getting flag counts
  neighbourflagcounts <- data.frame(good = sum(neighbourformerge$flags == 1),
                                    fail = sum(neighbourformerge$flags == 4),
                                    suspect = sum(neighbourformerge$flags == 3),
                                    uneval = sum(neighbourformerge$flags == 2))
  
  #Using output from flat line test
  neighbourPARandsensor <- flPARandsensor
  neighbourPARandsensor$floortimes <- floor(neighbourPARandsensor$time)
  
  #Adding flags from mean data back into full dataset
  neighbourPARandsensor <- merge(neighbourPARandsensor,neighbourformerge, by = c('floortimes',"sensor"), all.x = TRUE)
  
  neighbourPARandsensor$flags.y[is.na(neighbourPARandsensor$flags.y)] <- 2
  neighbourflagslong <- neighbourPARandsensor$flags.y
  
  neighbourPARandsensor$nn_flags <- neighbourPARandsensor$flags.y  # save for later debug
  
  neighbourPARandsensor$flags <- addflags(neighbourPARandsensor$flags.x, neighbourPARandsensor$flags.y)
  
  #adding dates for plotting
  neighbourPARandsensor$dates <- as.Date(neighbourPARandsensor$time, origin="1950-01-01")
  neighbourPARandsensor <- neighbourPARandsensor[order(neighbourPARandsensor$time),]
  
  
  #removing unnecessary files
  rm(neighbourtest, neighbourflagsPULSE, neighbourflagsSOFS, neighbourtestlogicPULSE, neighbourtestlogicSOFS, neighbourtestPULSE, neighbourtestSOFS)
  
  #reflag all SOFS 1 surface data as 3
  neighbourPARandsensor$flags[neighbourPARandsensor$sensor==5] <- 3
  neighbourPARandsensor$flags[neighbourPARandsensor$sensor==2] <- 3
  neighbourPARandsensor$flags[neighbourPARandsensor$sensor==22] <- 3
  neighbourPARandsensor <- neighbourPARandsensor[,-c(8,9,10,11)]

#final step, reintroducing 5 flags and creating time/flag file for output

badqcflags <- data.frame(time = badqcPARandsensor$time,flags = badqcPARandsensor$par_qc)
goodqcflags <- data.frame(time = neighbourPARandsensor$time,flags = neighbourPARandsensor$flags)
finalflags <- rbind(goodqcflags, badqcflags)
finalflags2 <- finalflags[order(finalflags$time),]


  #Pete wants format time, mooring, PAR, value, flag
  
  mooringvec <-  unlist(lapply(allthePARdata$sensor, mooringfromsensor))
  PARvec <- rep("PAR", nrow(allthePARdata))
  PARvalues <- allthePARdata$par
  
  #netcdf
  nc <- create.nc("foo.nc")
  dim.def.nc(nc, "TIME", unlim=TRUE)
  var.def.nc(nc, "TIME", "NC_DOUBLE", "TIME")
  var.def.nc(nc, "PAR", "NC_FLOAT", "TIME")
  var.def.nc(nc, "PAR_quality_code", "NC_BYTE", "TIME")
  var.def.nc(nc, "PAR_quality_code_gr", "NC_BYTE", "TIME")
  var.def.nc(nc, "PAR_quality_code_fl", "NC_BYTE", "TIME")
  var.def.nc(nc, "PAR_quality_code_cl", "NC_BYTE", "TIME")
  var.def.nc(nc, "PAR_quality_code_nn", "NC_BYTE", "TIME")
  var.def.nc(nc, "sensor", "NC_BYTE", "TIME")
  var.def.nc(nc, "PAR_solar_climate_high", "NC_FLOAT", "TIME")
  var.def.nc(nc, "PAR_solar_climate_low", "NC_FLOAT", "TIME")
  
  att.put.nc(nc, "TIME", "units", "NC_CHAR", "days since 1950-01-01T00:00:00 UTC")
  
  var.put.nc(nc, "TIME", finalflags2$time)
  var.put.nc(nc, "PAR", PARvalues)
  var.put.nc(nc, "PAR_quality_code", finalflags2$flags)
  var.put.nc(nc, "PAR_quality_code_gr", grPARandsensor$gr_flags)
  var.put.nc(nc, "PAR_quality_code_fl", flPARandsensor$fl_flags)
  var.put.nc(nc, "PAR_quality_code_cl", clPARandsensor$cl_flags)
  var.put.nc(nc, "PAR_quality_code_nn", neighbourPARandsensor$nn_flags)
  var.put.nc(nc, "sensor", allthePARdata$sensor)
  var.put.nc(nc, "PAR_solar_climate_high", grPARandsensor$ePARhigherchl)
  var.put.nc(nc, "PAR_solar_climate_low", grPARandsensor$ePARlowerchl)
  
  close.nc(nc)
  
  #csv
  #finaldataforpete <- cbind(finalflags2$time, mooringvec, PARvec, PARvalues, finalflags2$flags)      
  #colnames(finaldataforpete) <- c("TIME","MOORING","OBSCODE","VALUE","QC FLAG")
  
  #write.csv(finaldataforpete,file = "qcPARdata.csv", row.names = FALSE)

  