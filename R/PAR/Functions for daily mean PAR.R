#calculating daily means

dailymeans <- function(data)
{
  data$time <- floor(data$time)
  
  means <- data.frame(day = NA,meanpar = NA,deployment = NA)
  for (x in seq(min(data$time),max(data$time)))
  {
    
    parmean <- mean(subset(data$par,data$time == x & data$flags != 4 & data$flags != 3))

    means <- rbind(means,data.frame(day = x, 
                                    meanpar = parmean, 
                                    deployment = data$deployment[data$time == x][1]))
  }
  means <- na.omit(means)
  rownames(means) <- seq(1,length(means[,1]))
  means$dates <- as.Date(means$day, origin = "1950-01-01")
  return(means)
}

#as above but for estimated par values
dailymeansePAR <- function(data)
{
  data$time <- floor(data$time)
  
  means <- data.frame(day = NA,meanepar = NA,deployment = NA)
  for (x in seq(min(data$time),max(data$time)))
  {
    
    eparmean <- mean(subset(data$ePAR,data$time == x))

    
    means <- rbind(means,data.frame(day = x, 
                                    meanepar = eparmean, 
                                    deployment = data$deployment[data$time == x][1]))
  }
  means <- na.omit(means)
  rownames(means) <- seq(1,length(means[,1]))
  means$dates <- as.Date(means$day, origin = "1950-01-01")
  return(means)
}


#as above but for solar radiation values
dailymeanssolrad <- function(data)
{
  data$time <- floor(data$time)
  
  means <- data.frame(day = NA,meanrad = NA,deployment = NA)
  for (x in seq(min(data$time),max(data$time)))
  {
    
    radmean <- mean(subset(data$solrad,data$time == x))
    
    means <- rbind(means,data.frame(day = x, 
                                    meanrad = radmean, 
                                    deployment = data$deployment[data$time == x][1]))
  }
  means <- na.omit(means)
  rownames(means) <- seq(1,length(means[,1]))
  means$dates <- as.Date(means$day, origin = "1950-01-01")
  return(means)
}
  

