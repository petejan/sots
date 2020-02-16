#Adds flags to data without lower flags overwriting higher flags
addflags <- function(flag, new_flag)
{
  for (x in seq(1,length(flag)))
  {
    if (!is.na(new_flag[x]))
    {
      if (flag[x] < new_flag[x])
      {
        flag[x] <- new_flag[x]
      }
    }
  }
  return(flag)
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
