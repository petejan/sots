testdailymeans <- function(data)
{
  data$time <- floor(data$time)
  
  means <- data.frame(day = NA,meanpar = NA,deployment = NA)
  for (x in seq(min(data$time),max(data$time)))
  {
    #1440
    if (data$sensor %in% c(2,3,4,8,9,10,11,12,14,20,21,23,25,26,27,28,31,32))
    {
      parmean <- meaninput(data,1439,x)
      means <- rbind(means,data.frame(day = x, 
                                      meanpar = parmean, 
                                      deployment = data$deployment[data$time == x][1]))
    }
    #720
    if (data$sensor %in% c(6,16,17))
    {
      parmean <- meaninput(data,719,x)
      means <- rbind(means,data.frame(day = x, 
                                      meanpar = parmean, 
                                      deployment = data$deployment[data$time == x][1]))
    }
    #144
    if (data$sensor == 13)
    {
      parmean <- meaninput(data,142,x)
      means <- rbind(means,data.frame(day = x, 
                                      meanpar = parmean, 
                                      deployment = data$deployment[data$time == x][1]))
    }
    #120
    if (data$sensor %in% c(19,24))
    {
      parmean <- meaninput(data,118,x)
      means <- rbind(means,data.frame(day = x, 
                                      meanpar = parmean, 
                                      deployment = data$deployment[data$time == x][1]))
    }
    #36
    if (data$sensor == 5)
    {
      parmean <- meaninput(data,35,x)
      means <- rbind(means,data.frame(day = x, 
                                      meanpar = parmean, 
                                      deployment = data$deployment[data$time == x][1]))
    }
    #24
    if (data$sensor %in% c(1,7,15,18,22,29,30))
    {
      parmean <- meaninput(data,23,x)
      means <- rbind(means,data.frame(day = x, 
                                      meanpar = parmean, 
                                      deployment = data$deployment[data$time == x][1]))
    }
    
  }
  means <- means[-1,]
  rownames(means) <- seq(1,length(means[,1]))
  means$dates <- as.Date(means$day, origin = "1950-01-01")
  
  return(means)
}


testdailymeansePAR <- function(data,type)
{
  data$time <- floor(data$time)
  
  means <- data.frame(day = NA,meanepar = NA,deployment = NA)
  for (x in seq(min(data$time),max(data$time)))
  {
    #1440
    if (data$sensor %in% c(2,3,4,8,9,10,11,12,14,20,21,23,25,26,27,28,31,32))
    {
      parmean <- meaninputePAR(data,1439,x,type)
      means <- rbind(means,data.frame(day = x, 
                                      meanepar = parmean, 
                                      deployment = data$deployment[data$time == x][1]))
    }
    #720
    if (data$sensor %in% c(6,16,17))
    {
      parmean <- meaninputePAR(data,719,x,type)
      means <- rbind(means,data.frame(day = x, 
                                      meanepar = parmean, 
                                      deployment = data$deployment[data$time == x][1]))
    }
    #144
    if (data$sensor == 13)
    {
      parmean <- meaninputePAR(data,142,x,type)
      means <- rbind(means,data.frame(day = x, 
                                      meanepar = parmean, 
                                      deployment = data$deployment[data$time == x][1]))
    }
    #120
    if (data$sensor %in% c(19,24))
    {
      parmean <- meaninputePAR(data,118,x,type)
      means <- rbind(means,data.frame(day = x, 
                                      meanepar = parmean, 
                                      deployment = data$deployment[data$time == x][1]))
    }
    #36
    if (data$sensor == 5)
    {
      parmean <- meaninputePAR(data,35,x,type)
      means <- rbind(means,data.frame(day = x, 
                                      meanepar = parmean, 
                                      deployment = data$deployment[data$time == x][1]))
    }
    #24
    if (data$sensor %in% c(1,7,15,18,22,29,30))
    {
      parmean <- meaninputePAR(data,23,x,type)
      means <- rbind(means,data.frame(day = x, 
                                      meanepar = parmean, 
                                      deployment = data$deployment[data$time == x][1]))
    }
    
  }
  rownames(means) <- seq(1,length(means[,1]))
  means$dates <- as.Date(means$day, origin = "1950-01-01")
  return(means)
}





meaninput <- function(data,y,x)
{
  if (length(subset(data,data$time == x)[,1]) > y)
  {
    parmean <- mean(subset(data$par,data$time == x & data$flags != 4 & data$flags != 3))
  }
  else
  {
    parmean <- NA
  }
  
  return(parmean)
}



meaninputePAR <- function(data,y,x,type)
{
  if (type == "high")
  {
    if (length(subset(data,data$time == x)[,1]) > y)
    {
      parmean <- mean(subset(data$ePARhigherchl,data$time == x))
    }
    else
    {
      parmean <- NA
    }
    
  }
  
  if (type == "low")
  {
    if (length(subset(data,data$time == x)[,1]) > y)
    {
      parmean <- mean(subset(data$ePARlowerchl,data$time == x))
    }
    else
    {
      parmean <- NA
    }
    
  }
  
  if (type == "reg")
  {
    if (length(subset(data,data$time == x)[,1]) > y)
    {
      parmean <- mean(subset(data$ePAR,data$time == x))
    }
    else
    {
      parmean <- NA
    }
    
  }
  
  return(parmean)
}