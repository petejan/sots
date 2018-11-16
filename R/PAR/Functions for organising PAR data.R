#Specifies depth of sensor
depthfromsensor <- function(data)
{
  return (allPAR$NOMINAL_DEPTH[5])

  # #-3m
  # if (data == 5|data == 15|data == 18)
  # {
  #   return(-3)
  # }
  # 
  # #-1.8m
  # if (data == 22|data == 29)
  # {
  #   return(-1.8)
  # }
  # 
  # #0m
  # if (data == 2|data == 8|data == 14|data == 23)
  # {
  #   return(0)
  # }
  # 
  # #10m
  # if (data == 16)
  # {
  #   return(10)
  # }
  # 
  # #20m
  # if (data == 21|data == 28|data == 31)
  # {
  #   return(20)
  # }
  # 
  # #27m
  # if (data == 3|data == 9|data == 12)
  # {
  #   return(27)
  # }
  # 
  # #28
  # if (data == 24|data == 30)
  # {
  #   return(28)
  # }
  # 
  # #31.1
  # if (data == 7)
  # {
  #   return(31.1)
  # }
  # 
  # #34m
  # if (data == 13)
  # {
  #   return(34)
  # }
  # 
  # #37.5m
  # if (data == 1)
  # {
  #   return(37.5)
  # }
  # 
  # #38.5m
  # if (data == 19)
  # {
  #   return(38.5)
  # }
  # 
  # #40m
  # if (data == 6|data == 17|data == 20|data == 27)
  # {
  #   return(40)
  # }
  # 
  # #50m
  # if (data == 4|data == 10|data == 11|data == 25|data == 26|data == 32)
  # {
  #   return(50)
  # }
}

#as above but in string format
depthfromsensorforplots <- function(data)
{
  #-3m
  if (data == 5|data == 15|data == 18)
  {
    return("Surface - in air")
  }
  
  #-1.8m
  if (data == 22|data == 29)
  {
    return("Surface - in air")
  }
  
  #0m
  if (data == 2|data == 8|data == 14|data == 23)
  {
    return("Surface")
  }
  
  #10m
  if (data == 16)
  {
    return("10m")
  }
  
  #20m
  if (data == 21|data == 28|data == 31)
  {
    return("20m")
  }
  
  #27m
  if (data == 3|data == 9|data == 12)
  {
    return("27m")
  }
  
  #28
  if (data == 24|data == 30)
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
  if (data == 6|data == 17|data == 20|data == 27)
  {
    return("40m")
  }
  
  #50m
  if (data == 4|data == 10|data == 11|data == 25|data == 26|data == 32)
  {
    return("50m")
  }
}

#Specifies deployment using sensor number
mooringfromsensor <- function(data)
{
  return (instanceSplit[[data]][1])
  # #pulse 6
  # if (data <= 4)
  # {
  #   return("Pulse 6")
  # }
  # #pulse 7
  # if (data >= 7 & data <= 10)
  # {
  #   return("Pulse 7")
  # }
  # #pulse 8
  # if (data >= 11 & data <= 14)
  # {
  #   return("Pulse 8")
  # }
  # #pulse9
  # if (data == 19)
  # {
  #   return("Pulse 9")
  # }
  # #pulse 10
  # if (data >= 23 & data <= 26)
  # {
  #   return("Pulse 10")
  # }
  # #pulse 11 
  # if (data >= 30)
  # {
  #   return("Pulse 11")
  # }
  # #sofs 1
  # if (data >= 5 & data <= 6)
  # {
  #   return("SOFS 1")
  # }
  # #sofs 2
  # if (data >= 15 & data <= 17)
  # {
  #   return("SOFS 2")
  # }
  # #sofs 3
  # if (data == 18)
  # {
  #   return("SOFS 3")
  # }
  # #sofs 4
  # if (data >= 20 & data <= 22)
  # {
  #   return("SOFS 4")
  # }
  # #sofs 5
  # if (data >= 27 & data <= 29)
  # {
  #   return("SOFS 5")
  # }
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
  
  # if(sensor == 1)
  # {return("ECO PAR 135")}
  # if(sensor == 2)
  # {return("MDSMKVL 200341")}
  # if(sensor == 3)
  # {return("MDSMKVL 200664")}
  # if(sensor == 4)
  # {return("MDSMKVL 200665")}
  # if(sensor == 5)
  # {return("LI190SA Q40966")}
  # if(sensor == 6)
  # {return("MDSMKVL 201318")}
  # if(sensor == 7)
  # {return("ECO PAR 135")}
  # if(sensor == 8)
  # {return("MDSMKVL 200341")}
  # if(sensor == 9)
  # {return("MDSMKVL 200664")}
  # if(sensor == 10)
  # {return("MDSMKVL 200665")}
  # if(sensor == 11)
  # {return("MDSMKVL 200341")}
  # if(sensor == 12)
  # {return("MDSMKVL 200664")}
  # if(sensor == 13)
  # {return("ECO PAR 134")}
  # if(sensor == 14)
  # {return("MDSMKVL 200665")}
  # if(sensor == 15)
  # {return("LI190SA Q40966")}
  # if(sensor == 16)
  # {return("MDSMKVL 201319")}
  # if(sensor == 17)
  # {return("MDSMKVL 201318")}
  # if(sensor == 18)
  # {return("LI190SA Q40966")}
  # if(sensor == 19)
  # {return("ECO PAR 135")}
  # if(sensor == 20)
  # {return("MDSMKVL 201319")}
  # if(sensor == 21)
  # {return("MDSMKVL 201318")}
  # if(sensor == 22)
  # {return("LI190 Q47470")}
  # if(sensor == 23)
  # {return("MDSMKVL 200341")}
  # if(sensor == 24)
  # {return("ECO PAR 134")}
  # if(sensor == 25)
  # {return("MDSMKVL 200665")}
  # if(sensor == 26)
  # {return("DEFI L 082V023")}
  # if(sensor == 27)
  # {return("MDSMKVL 201319")}
  # if(sensor == 28)
  # {return("MDSMKVL 201318")}
  # if(sensor == 29)
  # {return("LI190 Q47470")}
  # if(sensor == 30)
  # {return("ECO PAR 134")}
  # if(sensor == 31)
  # {return("MDSMKVL 200665")}
  # if(sensor == 32)
  # {return("DEFI L 082V023")}
  # 
  
}