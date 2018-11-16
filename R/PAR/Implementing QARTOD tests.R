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
  
    #Getting flag counts
    grflagcounts <- data.frame(good =sum(grPARandsensor$flags == 1),
                               fail = sum(grPARandsensor$flags == 4),
                               suspect = sum(grPARandsensor$flags == 3),
                               uneval = sum(grPARandsensor$flags == 2))
    
    

    
    
#Climatology test
    
    #Using output of gross range test
    clPARandsensor <- grPARandsensor[order(grPARandsensor$time),]
    
    
    #Converting solar radiation data from Wm^-2 to micro mol photons m^-2 s^-1
    clPARandsensor$solrad <- sw.to.par.base(clPARandsensor$solrad)
    
    
    #calculate PAR at the depth of the data point based on clear water kd values
    climatologyPARfromKd <- function(cldata)
    {
      if (cldata[7] <= 0)
      {
        ePAR <- cldata[5]
      }
      else
      {
        cldata[5] <- as.numeric(cldata[5])
        ePAR <- 0.80*as.numeric(cldata[5])*exp(-0.04*as.numeric(cldata[7]))
      }
      return(ePAR)
    }
    
    #adding estimated PAR to data 
    clPARandsensor$ePAR <- apply(clPARandsensor,1,climatologyPARfromKd)
    clPARandsensor$ePAR <- as.numeric(clPARandsensor$ePAR)
    
    #Applying test to generate flags
    clflags <- apply(clPARandsensor,1,applyclimatologytest)
    
    #Add flags to data
    clPARandsensor$flags <- addflags(clPARandsensor$flags,clflags)
    
    #Getting flag counts
    clflagcounts <- data.frame(good = sum(clflags ==1), 
                               fail = sum(clflags == 4),
                               suspect = sum(clflags == 3),
                               uneval = sum(clflags == 2))
    
    #ensuring data is ordered chronologically
    clPARandsensor <- clPARandsensor[order(clPARandsensor$time),]

    
    
#Rate of change test  
    
    #Using output from climatology test
    rocPARandsensor <- clPARandsensor
    
    #Isolating night time data
    dayrocdata <- daydataPAR(rocPARandsensor)
    nightrocPARandsensor <- subset(rocPARandsensor,!(rocPARandsensor$time %in% dayrocdata$time))
    
    #Get mean PAR for each night
    nightlymeanrocPAR <- nightrocPARandsensor
    nightlymeanrocPAR$time <- floor(nightlymeanrocPAR$time)
    nightlymeanrocPAR <- ddply(nightlymeanrocPAR,.variables = 'sensor',dailymeans)
    
    
    #add the appropriate SDs
    rocsd <- by(nightlymeanrocPAR,nightlymeanrocPAR$sensor,function(x) {
      c(NA,rollapply(x$meanpar,FUN = sd, width = 7))
    })
    
    nightlymeanrocPAR$sd <- NA
    for (x in as.numeric(rownames(rocsd)))
    {
      nightlymeanrocPAR$sd[nightlymeanrocPAR$sensor == x] <- append(unlist(rocsd[x])[-1],c(NA,NA,NA,NA,NA,NA))
    }
    
    #Run the rate of change test
    rocflags <- roctest(nightlymeanrocPAR,3)
    
    #Replace unevaluated flags with value of 2
    rocflags[is.na(rocflags)] <- 2
    
    #Getting flag counts
    rocflagcounts <- data.frame(good = sum(rocflags == 1),
                                fail = sum(rocflags == 4),
                                suspect = sum(rocflags == 3),
                                uneval = sum(rocflags == 2))
    
    #Adding flags back into data
    rocformerge <- data.frame(floortimes = nightlymeanrocPAR$day,
                              flags = rocflags,
                              sensor = nightlymeanrocPAR$sensor)
    
   
    rocPARandsensor$floortimes <- floor(rocPARandsensor$time)
    rocPARandsensor <- merge(rocPARandsensor,rocformerge, by = c("floortimes","sensor"),all = TRUE)
    rocPARandsensor <- rocPARandsensor[order(rocPARandsensor$time),]
    
    
    rocflagslong <- rocPARandsensor$flags.y
    rocflagslong[is.na(rocflagslong)] <- 2
    
    #ensuring only adding flags to night data
    for (x in seq(1, length(rocPARandsensor[,1])))
    {
      if (rocPARandsensor$time[x] %in% nightrocPARandsensor$time[x])
      {
        rocPARandsensor$flags.x[x] <- addflags(rocPARandsensor$flags.x[x],rocPARandsensor$flags.y[x])
      }
    }
    
    #removing columns that won't be needed any more
    rocPARandsensor <- rocPARandsensor[,-13]
    colnames(rocPARandsensor)[11] <- "flags"
    
    #ensuring data is ordered chronologically 
    rocPARandsensor <- rocPARandsensor[order(rocPARandsensor$time),]
    
    
#Flat line test
    
    #Using output of rate of change test
    flPARandsensor <- rocPARandsensor
    

    #Isolating day time data
    dayflPARandsensor <- daydataPAR(flPARandsensor)
    
    #Running the flat line test
    flflags <- by(dayflPARandsensor,dayflPARandsensor$sensor,function(x) {
      c(NA,rollapply(x$par,FUN = flatlinetest, width = 5, fill = NA, align = 'right',tolerance = 0.1))
    })
    
    #adding flags back into day data
    dayflPARandsensor <- separatesensorflags(dayflPARandsensor,flflags)
    
    
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
    flPARandsensor$flags <- addflags(flPARandsensor$flags.x,flPARandsensor$flags.y)

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
    dailymeanPAR <- ddply(dailymeanPAR,.variables = 'sensor',dailymeans)
    dailymeanPAR <- daysoftheyear(dailymeanPAR)
    dailymeanPAR$depth <- unlist(lapply(dailymeanPAR$sensor,depthfromsensor))
    dailymeanPAR$sensorname <- sapply(dailymeanPAR$sensor,sensorname)
    
    
    
    
    #neighbour test for pulse moorings
    
    neighbourtestPULSE <- subset(dailymeanPAR,dailymeanPAR$sensor %in% c(1,2,3,4,7,8,9,10,11,12,13,14,19,23,24,25,26,30,31,32))
    neighbourtestPULSE <- neighbourtestPULSE[order(neighbourtestPULSE$day,neighbourtestPULSE$depth),]
    
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
    
    #Getting flag counts
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
    
    
    neighbourPARandsensor$flags <- addflags(neighbourPARandsensor$flags.x,neighbourPARandsensor$flags.y)
    
    #adding dates for plotting
    neighbourPARandsensor$dates <- as.Date(neighbourPARandsensor$time,origin = "1950-01-01")
    neighbourPARandsensor <- neighbourPARandsensor[order(neighbourPARandsensor$time),]
    
    
    
      

      
#final step, reintroducing 5 flags and creating time/flag file for output
      
      badqcflags <- data.frame(time = badqcPARandsensor$time,flags = badqcPARandsensor$par_qc)
      goodqcflags <- data.frame(time = neighbourPARandsensor$time,flags = neighbourPARandsensor$flags)
      finalflags <- rbind(goodqcflags,badqcflags)
      
      
      