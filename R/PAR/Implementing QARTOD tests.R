
printf <- function(...) cat(sprintf(...))

#Gross range test

  test_params <- "gross_range: sensor_min=-1.7, sensor_max=10000, output_min=-1.7, output_max=4500"
  printf(test_params)
  
  #Applying gross range test
  PARandsensor$flags_gr <- sapply(PARandsensor$par,
                                  grossrangetest,
                                  sensormin = -1.7,
                                  sensormax = 10000,
                                  opmin = -1.7,
                                  opmax = 4500)
  
  #Getting flag counts
  grflagcounts <- data.frame(good =sum(PARandsensor$flags_gr == 1),
                             fail = sum(PARandsensor$flags_gr == 4),
                             suspect = sum(PARandsensor$flags_gr == 3),
                             uneval = sum(PARandsensor$flags_gr == 2))
  
#Climatology test
  
  params <- "climatology: offset=3, mult=3, kd=0.04"
  printf(params)
  
  test_params <- paste(test_params, params, sep="\n");
  
  # adding estimated PAR to data 
  PARandsensor$ePAR <- apply(PARandsensor, 1, climatologyPARfromKd, kd=0.04)
  
  #Applying test to generate flags
  PARandsensor$flags_cl <- apply(PARandsensor, 1, applyclimatologytest2, limit=3, mult=3)

  # combine flags
  PARandsensor$flags <- addflags(PARandsensor$flags_gr, PARandsensor$flags_cl)
  
  #Getting flag counts
  clflagcounts <- data.frame(good = sum(PARandsensor$flags_cl ==1), 
                             fail = sum(PARandsensor$flags_cl == 4),
                             suspect = sum(PARandsensor$flags_cl == 3),
                             uneval = sum(PARandsensor$flags_cl == 2))
  
  #ensuring data is ordered chronologically
  #clPARandsensor <- clPARandsensor[order(clPARandsensor$time),]

  
    
#Flat line test
    params <- "flat line: day_data_only, window=5, tolerance=0"
    printf(params)
    
    test_params <- paste(test_params, params, sep="\n");
  
    #Isolating day time data
    day_data <- PARandsensor$solrad > 3

    #Running the flat line test
    flflags <- by(PARandsensor$par[day_data], PARandsensor$sensor[day_data], function(x) 
    {
      c(NA,rollapply(x, FUN = flatlinetest, width=5, fill=NA, align='right', tolerance=0))
    })
    
    flags_fl = rep(0, nrow(PARandsensor))
    for (x in seq(1,nrow(flflags)))
    {
      print(x)
      flags_fl[PARandsensor$sensor == x & day_data] = unlist(flflags[x])[-1]
    }
    PARandsensor$flags_fl = flags_fl
    PARandsensor$flags_fl[is.na(PARandsensor$flags_fl)] <- 0

    #Getting flag counts
    flflagcounts <- data.frame(good = sum(PARandsensor$flags_fl == 1),
                               fail = sum(PARandsensor$flags_fl == 4),
                               suspect = sum(PARandsensor$flags_fl == 3),
                               uneval = sum(PARandsensor$flags_fl == 0))

#Neighbour test
    
  params <- "neighbour test: daily mean"
  printf(params)
  test_params <- paste(test_params, params, sep="\n");

  #Isolating daily means to make this test more simple
  dailymeanPAR <- PARandsensor
  dailymeanPAR$time <- floor(dailymeanPAR$time)
  dailymeanPAR <- ddply(dailymeanPAR,.variables = 'sensor', testdailymeans)
  dailymeanPAR <- daysoftheyear(dailymeanPAR)
  dailymeanPAR$depth <- unlist(lapply(dailymeanPAR$sensor, depthfromsensor))
  
  #neighbour test for pulse moorings
  pulse_mooring <-  grepl("Pulse*", instanceSplit)  ## better way of finding the pulse mooring sensors
  n <- 1:31
  neighbourtestPULSE <- subset(dailymeanPAR, dailymeanPAR$sensor %in% c(n[pulse_mooring]))
  neighbourtestPULSE <- neighbourtestPULSE[order(neighbourtestPULSE$day, neighbourtestPULSE$depth),]
  
  #Conducting the neighbour test, output is in TRUE/FALSE from
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
  neighbourtestSOFS <- subset(dailymeanPAR,(dailymeanPAR$sensor %in% c(n[!pulse_mooring])))
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
  neighbourtest <- rbind(neighbourtestPULSE, neighbourtestSOFS)
  
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
  neighbourPARandsensor <- PARandsensor
  neighbourPARandsensor$floortimes <- floor(neighbourPARandsensor$time)
  
  #Adding flags from mean data back into full dataset
  neighbourPARandsensor <- merge(neighbourPARandsensor, neighbourformerge, by = c('floortimes', 'sensor'), all.x = TRUE)
  
  neighbourPARandsensor$flags.y[is.na(neighbourPARandsensor$flags.y)] <- 2
  neighbourflagslong <- neighbourPARandsensor$flags.y
  
  PARandsensor$flags_nn <- neighbourPARandsensor$flags.y  # save for later debug
  
  neighbourPARandsensor$flags <- addflags(neighbourPARandsensor$flags.x, neighbourPARandsensor$flags.y)
  
  #adding dates for plotting
  #neighbourPARandsensor$dates <- as.Date(neighbourPARandsensor$time, origin="1950-01-01")
  #neighbourPARandsensor <- neighbourPARandsensor[order(neighbourPARandsensor$time),]
  
  
  #removing unnecessary files
  rm(neighbourtest, neighbourflagsPULSE, neighbourflagsSOFS, neighbourtestlogicPULSE, neighbourtestlogicSOFS, neighbourtestPULSE, neighbourtestSOFS)
  
# manual flagging data as 3
  
  params <- "manual: SOFS-1-2010-Licor-Q40966, Pulse-6-2009-Alec-200341, SOFS-4-2013-Licor-Q47470"
  printf(params)
  
  test_params <- paste(test_params, params, sep="\n");
  
  PARandsensor$flags_man <- rep(0, nrow(PARandsensor))
  mooring_sofs1_surface <- n[grepl("SOFS-1-2010.*Q40966", instanceSplit)]
  PARandsensor$flags_man[PARandsensor$sensor==mooring_sofs1_surface] <- 3
  mooring_pulse6_surface <- n[grepl("Pulse-6-2009.*200341", instanceSplit)]
  PARandsensor$flags_man[PARandsensor$sensor==mooring_pulse6_surface] <- 3
  mooring_sofs4_surface <- n[grepl("SOFS-4-2013.*Q47470", instanceSplit)]
  PARandsensor$flags_man[PARandsensor$sensor==mooring_sofs4_surface] <- 3
  
  #PARandsensor <- PARandsensor[,-c(8,9,10,11)]

#final step, reintroducing 5 flags and creating time/flag file for output
  
  printf("recombine flags...")

  badqcflags <- data.frame(time=badqcPARandsensor$time, flags=badqcPARandsensor$par_qc)
  goodqcflags <- data.frame(time=neighbourPARandsensor$time, flags=neighbourPARandsensor$flags)
  finalflags <- rbind(goodqcflags, badqcflags)
  #finalflags2 <- finalflags[order(finalflags$time),]


#netcdf
  
  printf("output netcdf file...\n")  
  
  nc <- create.nc("par-data-qc.nc")
  att.put.nc(nc, "NC_GLOBAL", "source_file", "NC_CHAR", source_nc_file)
  att.put.nc(nc, "NC_GLOBAL", "test_params", "NC_CHAR", test_params)

  dim.def.nc(nc, "TIME", unlim=TRUE)
  
  var.def.nc(nc, "TIME", "NC_DOUBLE", "TIME")
  var.def.nc(nc, "PAR", "NC_FLOAT", "TIME")
  var.def.nc(nc, "ePAR", "NC_FLOAT", "TIME")

  var.def.nc(nc, "sensor", "NC_BYTE", "TIME")
  var.def.nc(nc, "depth", "NC_FLOAT", "TIME")
  
  var.def.nc(nc, "PAR_quality_code", "NC_BYTE", "TIME")
  var.def.nc(nc, "PAR_quality_code_gr", "NC_BYTE", "TIME")
  var.def.nc(nc, "PAR_quality_code_fl", "NC_BYTE", "TIME")
  var.def.nc(nc, "PAR_quality_code_cl", "NC_BYTE", "TIME")
  var.def.nc(nc, "PAR_quality_code_nn", "NC_BYTE", "TIME")
  var.def.nc(nc, "PAR_quality_code_man", "NC_BYTE", "TIME")
  
  att.put.nc(nc, "TIME", "units", "NC_CHAR", "days since 1950-01-01T00:00:00 UTC")
  
  var.put.nc(nc, "TIME", PARandsensor$time)
  var.put.nc(nc, "PAR", PARandsensor$par)
  
  var.put.nc(nc, "PAR_quality_code", PARandsensor$flags)
  var.put.nc(nc, "PAR_quality_code_gr", PARandsensor$flags_gr)
  var.put.nc(nc, "PAR_quality_code_cl", PARandsensor$flags_cl)
  var.put.nc(nc, "PAR_quality_code_fl", PARandsensor$flags_fl)
  var.put.nc(nc, "PAR_quality_code_man", PARandsensor$flags_man)
  var.put.nc(nc, "PAR_quality_code_nn", PARandsensor$flags_nn)
  
  var.put.nc(nc, "sensor", PARandsensor$sensor-1)  # sensors here are 1 numbered, and 0 numbered in the netCDF
  
  var.put.nc(nc, "ePAR", PARandsensor$ePAR)
  var.put.nc(nc, "depth", PARandsensor$depth)
  close.nc(nc)
    
  #csv
  
  #printf("output csv file...")
  
  #Pete wants format time, mooring, PAR, value, flag
  
  #mooringvec <-  unlist(lapply(allthePARdata$sensor, mooringfromsensor))
  #PARvec <- rep("PAR", nrow(allthePARdata))
  #PARvalues <- allthePARdata$par
  
  #finaldataforpete <- cbind(finalflags2$time, mooringvec, PARvec, PARvalues, finalflags2$flags)      
  #colnames(finaldataforpete) <- c("TIME","MOORING","OBSCODE","VALUE","QC FLAG")
  
  #write.csv(finaldataforpete,file = "qcPARdata.csv", row.names = FALSE)
