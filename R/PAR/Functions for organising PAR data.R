#Specifies depth of sensor
depthfromsensor <- function(data)
{
  return (allPAR$NOMINAL_DEPTH[data])
}

#as above but in string format
depthfromsensorforplots <- function(data)
{
  
  if (data %in% c(2,5,8,14,15,18,22,23,29))
  {
    return("Surface")
  }
  
  #10m
  if (data == 16)
  {
    return("10m")
  }
  
  #20m
  if (data %in% c(21,28,31))
  {
    return("20m")
  }
  
  #27m
  if (data %in% c(3,9,12))
  {
    return("27m")
  }
  
  #28
  if (data %in% c(24,30))
  {
    return("28m")
  }
  
  #31.1
  if (data == 7)
  {
    return("31.1m")
  }
  
  #34m
  if (data == 13)
  {
    return("34m")
  }
  
  #37.5m
  if (data == 1)
  {
    return("37.5m")
  }
  
  #38.5m
  if (data == 19)
  {
    return("38.5m")
  }
  
  #40m
  if (data %in% c(6,17,20,27))
  {
    return("40m")
  }
  
  #50m
  else
  {
    return("50m")
  }
}

#Specifies deployment using sensor number
mooringfromsensor <- function(data)
{
  return (instanceSplit[[data]][1])
}

#specifies longitude from sensor
longitudefromsensor <- function(data)
{
  return(allPAR$LONGITUDE[data])
}

#specifies latitude from sensor
latitudefromsensor <- function(data)
{
  return(allPAR$LATITUDE[data])
}

#specifies sensor name from sensor number
sensorname <- function(sensor)
{
  return (paste(instanceSplit[[sensor]][3], instanceSplit[[sensor]][4]))
  
}