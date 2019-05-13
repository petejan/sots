#Gross range test
    
    #using data from import
    grPARandsensor <- PARandsensor
    
    #Converting solar radiation data from Wm^-2 to micro mol photons m^-2 s^-1
    grPARandsensor$solrad <- sw.to.par.base(grPARandsensor$solrad)
    
    #Applying gross range test
    grPARandsensor$flags <- sapply(grPARandsensor$par,
                                   grossrangetest,
                                   sensormin = -1.7,
                                   sensormax = 10000,
                                   opmin = -1.7,
                                   opmax = 4500)
    
    
    grPARandsensor$dates <- as.Date(grPARandsensor$time,origin = "1950-01-01")
    
    kd490high <- 0.0166 + 0.07242*1^0.68955
    kd490low <- 0.0166 + 0.07242*0.5^0.68955
    
    kdPARhigh <- 0.0864 + 0.884*kd490high - 0.00137*(kd490high)^-1
    kdPARlow <- 0.0864 + 0.884*kd490low - 0.00137*(kd490low)^-1
      
    
    grPARandsensor$ePARhigherchl <- apply(grPARandsensor,1,climatologyPARfromKd,kd = kdPARhigh)
    grPARandsensor$ePARhigherchl <- as.numeric(grPARandsensor$ePARhigherchl)
    grPARandsensor$ePARlowerchl <- apply(grPARandsensor,1,climatologyPARfromKd,kd = kdPARlow)
    grPARandsensor$ePARlowerchl <- as.numeric(grPARandsensor$ePARlowerchl)
    
    
    #Getting flag counts
    grflagcounts <- data.frame(good =sum(grPARandsensor$flags == 1),
                               fail = sum(grPARandsensor$flags == 4),
                               suspect = sum(grPARandsensor$flags == 3),
                               uneval = sum(grPARandsensor$flags == 2))
    
    

    
    
#Climatology test
    
    #Using output of gross range test
    clPARandsensor <- grPARandsensor[order(grPARandsensor$time),]
    
    


    
    #adding estimated PAR to data 
    clPARandsensor$ePAR <- apply(clPARandsensor,1,climatologyPARfromKd,kd = 0.04)
    clPARandsensor$ePAR <- as.numeric(clPARandsensor$ePAR)
    
    #Applying test to generate flags
    clflags <- apply(clPARandsensor,1,applyclimatologytest)
    
    #Add flags to data
    clPARandsensor$flags <- addflags(clPARandsensor$flags,clflags)
    
    
    
        #clPARandsensor$floortimes <- floor(clPARandsensor$time)
        
        #clbaddays <- subset(clPARandsensor,clPARandsensor$flags==3)
        
        #clPARandsensor$flags[clPARandsensor$floortimes %in% clbaddays$floortimes] <- 3
    
    #Getting flag counts
    clflagcounts <- data.frame(good = sum(clflags ==1), 
                               fail = sum(clflags == 4),
                               suspect = sum(clflags == 3),
                               uneval = sum(clflags == 2))
    
    #removing flags file
    rm(clflags)
    
    #ensuring data is ordered chronologically
    clPARandsensor <- clPARandsensor[order(clPARandsensor$time),]

    
    

    
#Flat line test
    
    #Using output of rate of change test
    flPARandsensor <- clPARandsensor
    

    #Isolating day time data
    #this has old flags in it, let's try removing them?
    dayflPARandsensor <- daydataPAR(flPARandsensor)
    dayflPARandsensor$flags <- 1
    
    #Running the flat line test
    flflags <- by(dayflPARandsensor,dayflPARandsensor$sensor,function(x) {
      c(NA,rollapply(x$par,FUN = flatlinetest, width = 5, fill = NA, align = 'right',tolerance = 0))
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
                                                           "lon",
                                                           "sw",
                                                           "solrad",
                                                           "ePAR",
                                                           "dates",
                                                           "ePARhigherchl",
                                                           "ePARlowerchl")
                  , all.x = TRUE)
    
    #Replacing unevaluated flags with a value of 2
    flPARandsensor$flags.y[is.na(flPARandsensor$flags.y)] <- 2
    
    flflagslong <- flPARandsensor$flags.y
    
    #Adding daytime flags to entire flag data
    flPARandsensor$flags <- addflags(flPARandsensor$flags.x,flPARandsensor$flags.y)
    
    #removing flag file
    rm(flflags)
    
    #Getting flag counts
    flflagcounts <- data.frame(good = sum(flPARandsensor$flags.y == 1),
                               fail = sum(flPARandsensor$flags.y == 4),
                               suspect = sum(flPARandsensor$flags.y == 3),
                               uneval = sum(flPARandsensor$flags.y == 2))

    
    #ensuring data is ordered chronologically
    flPARandsensor <- flPARandsensor[order(flPARandsensor$time),]
    
    flPARandsensor$flags.x <- NULL
    flPARandsensor$flags.y <- NULL
    
#Neighbour test
    
    #Isolating daily means to make this test more simple
    dailymeanPAR <- flPARandsensor
    dailymeanPAR$time <- floor(dailymeanPAR$time)
    dailymeanPAR <- ddply(dailymeanPAR,.variables = 'sensor',testdailymeans)
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
    neighbourtestPULSE$flags[neighbourtestPULSE$depth == 50 & neighbourtestPULSE$deployment == "Pulse-10-2013"] <- 1
    
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
    
    incompletedays <- subset(dailymeanPAR,is.na(dailymeanPAR$meanpar))
    
    for (x in seq(1,length(incompletedays[,1])))
    {
      neighbourPARandsensor$flags.y[neighbourPARandsensor$floortimes == incompletedays$day[x] & neighbourPARandsensor$sensor == incompletedays$sensor[x]] <- 2
    }
    
    
    neighbourPARandsensor$flags.y[is.na(neighbourPARandsensor$flags.y)] <- 2
    neighbourflagslong <- neighbourPARandsensor$flags.y
    
    
    neighbourPARandsensor$flags <- addflags(neighbourPARandsensor$flags.x,neighbourPARandsensor$flags.y)
    
    #adding dates for plotting
    #neighbourPARandsensor$dates <- as.Date(neighbourPARandsensor$time,origin = "1950-01-01")
    neighbourPARandsensor <- neighbourPARandsensor[order(neighbourPARandsensor$time),]
    
    
    #removing unnecessary files
    rm(neighbourtest,neighbourflagsPULSE,neighbourflagsSOFS,neighbourtestlogicPULSE,neighbourtestlogicSOFS,neighbourtestPULSE,neighbourtestSOFS)
    
#reflag all SOFS 1 surface data as 3
    neighbourPARandsensor$flags[neighbourPARandsensor$sensor==5] <- 3
    neighbourPARandsensor$flags[neighbourPARandsensor$sensor==2] <- 3
    neighbourPARandsensor$flags[neighbourPARandsensor$sensor==22] <- 3
    neighbourPARandsensor <- neighbourPARandsensor[,-c(8,9,10,11)]
    
    
    
      

      
#final step, reintroducing 5 flags and creating time/flag file for output
      
      badqcflags <- data.frame(time = badqcPARandsensor$time,flags = badqcPARandsensor$par_qc)
      goodqcflags <- data.frame(time = neighbourPARandsensor$time,flags = neighbourPARandsensor$flags)
      finalflags <- rbind(goodqcflags,badqcflags)
      finalflags2 <- finalflags[order(finalflags$time),]
      
      
#Pete wants format time, mooring, PAR, value, flag
      
      mooringvec <-  unlist(lapply(allthePARdata$sensor,mooringfromsensor))
      PARvec <- rep("PAR",6135719)
      PARvalues <- allthePARdata$par

      finaldataforpete <- cbind(finalflags2$time,mooringvec,PARvec,PARvalues,finalflags2$flags)      
      colnames(finaldataforpete) <- c("TIME","MOORING","OBSCODE","VALUE","QC FLAG")
      
      
      #csv
      write.csv(finaldataforpete,file = "qcPARdata.csv", row.names = FALSE)
      
      