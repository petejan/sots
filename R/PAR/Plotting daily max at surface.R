#getting data ready
dailymaxPARandsensor <- subset(neighbourPARandsensor,neighbourPARandsensor$flags == 1 | neighbourPARandsensor$flags == 2)
dailymaxPARandsensor$time <- floor(dailymaxPARandsensor$time)
dailymaxPARandsensor$sensorname <- sapply(dailymaxPARandsensor$sensor,sensorname)
dailymaxPARandsensor$depth[dailymaxPARandsensor$sensor == 25] <- 50.01


dailymaxfunction <- function(data,depthinm)
{
  dailymaxdata <- subset(data,data$depth == depthinm)
  dailymaxdata <- as.data.table(dailymaxdata)
  dailymaxdata <- dailymaxdata[dailymaxdata[, .I[par == max(par)], by=list(time,sensor)]$V1]
  dailymaxdata <- data.frame(distinct(dailymaxdata,time,sensor, .keep_all = TRUE))
  times <- timeconv(dailymaxdata$time)
  dailymaxdata <- cbind(dailymaxdata,times[,1])
  colnames(dailymaxdata)[length(dailymaxdata)] <- 'year'
  return(dailymaxdata)
}


#isolating sets of sensors at depths
  #includes -2.8m and -2.7m, must be something I did somewhere else in the code
  dailymaxPARneg3m <- dailymaxfunction(dailymaxPARandsensor,-3)
  dailymaxPARneg1.8m <- dailymaxfunction(dailymaxPARandsensor,-1.8)
  dailymaxPAR0m <- dailymaxfunction(dailymaxPARandsensor,0)
  dailymaxPAR10m <- dailymaxfunction(dailymaxPARandsensor,10)
  dailymaxPAR20m <- dailymaxfunction(dailymaxPARandsensor,20)
  dailymaxPAR27m <- dailymaxfunction(dailymaxPARandsensor,27)
  dailymaxPAR28m <- dailymaxfunction(dailymaxPARandsensor,28)
  dailymaxPAR31.1m <- dailymaxfunction(dailymaxPARandsensor,31.1)
  dailymaxPAR34m <- dailymaxfunction(dailymaxPARandsensor,34)
  dailymaxPAR37.5m <- dailymaxfunction(dailymaxPARandsensor,37.5)
  dailymaxPAR38.5m <- dailymaxfunction(dailymaxPARandsensor,38.5)
  dailymaxPAR40m <- dailymaxfunction(dailymaxPARandsensor,40)
  dailymaxPAR50m <- dailymaxfunction(dailymaxPARandsensor,50)
    #weird stuff to make sure sensors at 50m don't overwrite one another
    dailymaxPAR50.01m <- dailymaxfunction(dailymaxPARandsensor,50.01)
    dailymaxPAR50m <- rbind(dailymaxPAR50m,dailymaxPAR50.01m)
    dailymaxPAR50m <- dailymaxPAR50m[order(dailymaxPAR50m$time),]
    dailymaxPAR50m$depth[dailymaxPAR50m$depth == 50.01] <-50


#getting surface data for sofs1 sofs2 and pulse 6
    dailymaxPARsurfacecomp <- dailymaxPAR0m[dailymaxPAR0m$deployment == 'Pulse 6',]
      dailymaxPARsurfacecomp <- rbind(dailymaxPARsurfacecomp,
                                      dailymaxPARneg3m[dailymaxPARneg3m$deployment == 'SOFS 1',])
      dailymaxPARsurfacecomp <- rbind(dailymaxPARsurfacecomp,
                                      dailymaxPARneg3m[dailymaxPARneg3m$deployment == 'SOFS 2',])
      dailymaxPARsurfacecomp <- rbind(dailymaxPARsurfacecomp,
                                      dailymaxPARneg1.8m[dailymaxPARneg1.8m$deployment == 'SOFS 4',])
      dailymaxPARsurfacecomp <- rbind(dailymaxPARsurfacecomp,
                                      dailymaxPARneg1.8m[dailymaxPARneg1.8m$deployment == 'SOFS 5',])
      
      dailymaxPARsurfacecomp$dates <- as.Date(dailymaxPARsurfacecomp$time, origin = "1950-01-01")
  
      
    #plotting this surface data
      ggplot(dailymaxPARsurfacecomp[dailymaxPARsurfacecomp$deployment =="Pulse 6",],aes(dates,par)) + geom_point(aes(colour = factor(deployment),group = year),size = 1) +
        geom_point(data = dailymaxPARsurfacecomp[dailymaxPARsurfacecomp$deployment =="SOFS 1",],aes(dates,par,colour = factor(deployment),group = year),size = 1) +
        geom_point(data = dailymaxPARsurfacecomp[dailymaxPARsurfacecomp$deployment =="SOFS 2",],aes(dates,par,colour = factor(deployment),group = year),size = 1) +
        geom_point(data = dailymaxPARsurfacecomp[dailymaxPARsurfacecomp$deployment =="SOFS 4",],aes(dates,par,colour = factor(deployment),group = year),size = 1) +
        geom_point(data = dailymaxPARsurfacecomp[dailymaxPARsurfacecomp$deployment =="SOFS 5",],aes(dates,par,colour = factor(deployment),group = year),size = 1) +
        scale_y_log10() + 
        xlab("Date") + 
        ylab(PAR~(mu*mol~m^-2~s^-1)) + 
        guides(colour = guide_legend(override.aes = list(size = 5,
                                                         shape = c(19,19,19,19,19)))) +
        theme(axis.text = element_text(size = 15, colour = 'black'),
                           legend.key=element_blank(),
                           plot.title = element_text(size = 20,hjust = 0.5),
                           axis.title = element_text(size = 17),
                           legend.text = element_text(size = 13),
                           legend.title = element_blank(),
                           panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      #same but yearly cycle
      ggplot(dailymaxPARsurfacecomp[dailymaxPARsurfacecomp$deployment =="Pulse 6",],aes(dayofyear,par)) + geom_point(aes(colour = factor(deployment),group = year)) +
        geom_point(data = dailymaxPARsurfacecomp[dailymaxPARsurfacecomp$deployment =="SOFS 1",],aes(dayofyear,par,colour = factor(deployment),group = year)) +
        geom_point(data = dailymaxPARsurfacecomp[dailymaxPARsurfacecomp$deployment =="SOFS 2",],aes(dayofyear,par,colour = factor(deployment),group = year)) +
        geom_point(data = dailymaxPARsurfacecomp[dailymaxPARsurfacecomp$deployment =="SOFS 3",],aes(dayofyear,par,colour = factor(deployment),group = year)) +
        scale_y_log10() + 
        xlab("Days from Jan 1st") + 
        ylab(PAR~(mu*mol~m^-2~s^-1)) + 
        theme(axis.text = element_text(size = 15, colour = 'black'),
              legend.key=element_blank(),
              plot.title = element_text(size = 20,hjust = 0.5),
              axis.title = element_text(size = 17),
              legend.text = element_text(size = 13),
              legend.title = element_blank(),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
