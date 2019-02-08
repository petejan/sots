daysoftheyear <- function(data)
{
  #times as dates
  dates <- timeconv(as.numeric(unlist(data["day"])))
  # print(dates)
  
  #times as dates in string format
  timestring <- timeconvstring(as.numeric(unlist(data["day"])))
  
  
  #figure out how many days from 1950-01-01 to the 1st of Jan on the year the mooring was deployed
  timeinfo <- data.frame(time = data["day"], 
                         dates,
                         timetojan = unlist(lapply(dates[,1],timefromjanfunction))) 
  #print(timeinfo)
  
  
  #timeinfo <- cbind(timeinfo,timetojan)
  
  #convert the time into days since july 1st within the year of that data set
  timeinfo[,1] <- timeinfo[,1] - timeinfo[,8]
  
  data <- cbind(data,timeinfo[,2])
  colnames(data)[length(data)] <- "year"
  
  timeinfo[timeinfo > 365] <- timeinfo[timeinfo > 365] - 365
  
  #add this as a column to the original dataset
  data <- cbind(data,timeinfo[,1])
  colnames(data)[length(data)] <- "dayofyear"
  
  
  
  return(data)
}

#need to convert time in days since 00:00:00 Jan 1 1950 to UTC date and time
timeconv <- function(x)
{
  y <- utcal.nc("days since 1950-01-01 00:00:00 +00:00",x)
  return(y)
}


#as above but only returns the year
timeconvyear <- function(x)
{
  y <- utcal.nc("days since 1950-01-01 00:00:00 +00:00",x)
  return(y[1])
}

#as above but outputs date as string
timeconvstring <- function(x)
{
  y <- utcal.nc("days since 1950-01-01 00:00:00 +00:00",x, type ="c")
  return(y)
}

#reverse of timeconv
revtimeconv <- function(x)
{
  return(utinvcal.nc("days since 1950-01-01 00:00:00 +00:00",x))
}

#returns time from start of year in days
timefromjanfunction <- function(data)
{
  timefromjan <- utinvcal.nc("days since 1950-01-01 00:00:00", paste(toString(data),"-01-01 00:00:00", sep = ''))
  return(timefromjan)
}
