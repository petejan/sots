#Specifies depth of sensor
depthfromsensor <- function(data)
{
  return (allPAR$NOMINAL_DEPTH[data])
}

#as above but in string format
depthfromsensorforplots <- function(data)
{
  d = depthfromsensor(data)
  if (d < 0)
  {
    s <- 'surface'
  }
  else
  {
    s <- sprintf("%.0fm", depthfromsensor(data))
  }

  return (s)
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
  return (paste(instanceSplit[[1]][3], instanceSplit[[1]][4]))
}