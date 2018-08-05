#function to give all values of data that happen in the day 
#all functions used are below
daydataPAR <- function(data)
{
  daytimes <- daytime(data)
  daytimesseq <- createdaytimeseq(daytimes)
  #create data file with time values rounded
  roundeddata <- data
  roundeddata$time <- round(roundeddata$time, digits = 2)
  #find indices of day time values in rounded time data
  daytimeindex <- which(roundeddata$time %in% daytimesseq)
  #use these indices to get day time values in non-rounded data
  daytimedata <- data[daytimeindex,]
  
  return(daytimedata)
  
}



#calculates sunrise and sunset times
sunrisesunsetcal <- function(mooringdata)
{
  convstartdate <- timeconv(mooringdata[1,1])
  convenddate <- timeconv(mooringdata[length(mooringdata[,1]),1])
  
  startdate <- paste(toString(convstartdate[1]),toString(convstartdate[2]),toString(convstartdate[3]), sep = '/')
  enddate <- paste(toString(convenddate[1]),toString(convenddate[2]),toString(convenddate[3] + 1), sep = '/')
  
  
  sunriseandsunset <- getSunlightTimes(date = seq.Date(from = as.Date(startdate, tz = "UTC") , to = as.Date(enddate, tz = "UTC"), by = 1), 
                                       keep = c("sunrise","sunset"),
                                       lat = mooringdata$lat[1],
                                       lon = mooringdata$lon[1],
                                       tz = "UTC")
  return(sunriseandsunset)
}


#provides these times in nice decimal format
daytime <- function(mooringdata)
{
  initdaydata <- sunrisesunsetcal(mooringdata)
  initdaydata[,4] <- revtimeconv(initdaydata[,4])
  initdaydata[,5] <- revtimeconv(initdaydata[,5])
  
  return(initdaydata)
}

#creates a single time vector with all day time values to 2 decimal places

createdaytimeseq <- function(daydata)
{
  timeseq <- seq(daydata[1,4],daydata[1,5],by = 0.01)
  for (x in seq(2,length(daydata[,1])))
  {
    timeseq <- c(timeseq,seq(daydata[x,4],daydata[x,5],by = 0.01))
  }
  return(round(timeseq,digits = 2))
}


