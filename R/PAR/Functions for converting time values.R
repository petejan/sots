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
