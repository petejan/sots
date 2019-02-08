#Gross range test
    #no need for 2 flag here
    grossrangetest <- function(data,sensormin,sensormax,opmin,opmax)
    {
      if (data > sensormax | data < sensormin)
      {
        return(4)
      }
      if (data > opmax | data < opmin)
      {
        return(3)
      }
      else
      {
        return(1)
      }
    }


#Climatology test
    #function for estimating ePAR based on clear water kd values
    climatologyPARfromKd <- function(cldata)
    {
      if (cldata["depth"] <= 0)
      {
        ePAR <- cldata["solrad"]
      }
      else
      {
        cldata["solrad"] <- as.numeric(cldata["solrad"])
        ePAR <- as.numeric(cldata["solrad"])*exp(-0.04*as.numeric(cldata["depth"]))
      }
      return(ePAR)
    }
    
    
    
    applyclimatologytest <- function(data)
    {
      #check if PAR value is above 3
      if (as.numeric(data[3]) > 3)
      {
        #check if PAR value fails test
        if (as.numeric(data[3]) > (as.numeric(data[12])*3))
        {
          return(3)
        }
        else
        {
          return(1)
        }
      }
      else
      {
        return(2)
      }
    }
    

    
#rate of change test
    roctest <- function(data, numberofsd)
    {
      #flags
      flags <- rep(NA,length(data$meanpar))
      for (x in seq(1,length(data$meanpar) - 1))
      {
        #check if data has an associated sd value
        if (!is.na(data$sd[x]))
        {
          #check if difference between two data points is greater than 3 sd
          if (abs(data$meanpar[x] - data$meanpar[x+1]) > numberofsd*data$sd[x])
          {
            flags[x] <- 3
          }
          else
          {
            flags[x] <- 1
          }
        }
      }
      return(flags)
    }
    
    

#flat line test
    flatlinetest <- function(data,tolerance)
    {
      #check if difference between subsequent data points falls below a tolerance value
      if (abs(max(data) - min(data)) < tolerance)
      {
        return(4)
      }
      else
      {
        return(1)
      }
    }
    

    
    

#Neighbour test
    creatingdaylists <- function(data)
    {
      daylists <- list()
      #looping through each day
      y = 0
      for (x in unique(data$day))
      {
        y <- y + 1
        #getting all the data from that day and putting it in sensor order
        day <- data[data$day == x,]
        day <- day[order(day$depth),]
        
        #ensuring test is only run on days with more than one sensor
        if (length(day[,1]) > 1)
        {
          daylists[[y]] <- list(creatinglists(day))
          
        }
        else
        {
          daylists[[y]] <- NA
        }
        
      }
      
      
      return(daylists)
    }
          
    
          #checks if each datapoint in depth order is lower than the one underneath it
          creatinglists <- function(daydata)
          {
            islower <- vector("list",length(daydata[,1]))
            for (y in seq(1,length(daydata[,1])-1))
            {
              if (length(daydata$meanpar > 2))
              {
                islower[y] <- list(daydata$meanpar[y] < daydata$meanpar[y+1:length(daydata$meanpar)])
              }
              else
              {
                islower[y] <- list(daydata$meanpar[y] < daydata$meanpar[y+1])
              }
            }
            return(islower)
          }
          
          
          #converts the logic of the above test into a flag value
          gettingflagsfromlogic <- function(data)
          {
            flags <- list()
            
            for (x in seq(1,length(data)))
            {
              #look at each day
              daytest <- data[[x]][[1]]
              
              dayflags <- sapply(daytest,countingtrues)
              
              flags[[x]] <- dayflags
            }
            return(flags)
          }
          
                  
                  #as above
                  countingtrues <- function(daydata)
                  {
                    daydata <- unlist(daydata)
                    
                    if (sum(daydata,na.rm = TRUE) >= 2)
                    {
                      return(4)
                    }
                    
                    if (sum(daydata, na.rm = TRUE) == 1)
                    {
                      return(3)
                    }
                    else
                    {
                      return(1)
                    }
                  }
    
    




