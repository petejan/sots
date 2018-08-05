#Adds flags to data without lower flags overwriting higher flags
addflags <- function(data,flags)
{
  for (x in seq(1,length(data)))
  {
    if (!is.na(flags[x]))
    {
      if (data[x] < flags[x])
      {
        data[x] <- flags[x]
      }
    }
  }
  return(data)
}


#function for reattaching flags when test is conducted separately on each sensor
separatesensorflags <- function(data,flags)
{
  for (x in seq(1,length(flags)))
  {
    data$flags[data$sensor == x] <- addflags(data$flags[data$sensor == x],unlist(flags[x])[-1])
  }
  return(data)
}
