
  mooringflagcounts <- function(data)
  {
    flagcounts <- list()
    for (x in unique(data$dep))
    {
      print(x)
      mooringdata <- subset(data,data$dep == x)
      print(length(mooringdata[,2]))
      mooringdata$flags[is.na(mooringdata$flags)] <- 2
      mooringflagcounts <- c(sum(mooringdata$flags == 1),
                             sum(mooringdata$flags == 2),
                             sum(mooringdata$flags == 3),
                             sum(mooringdata$flags == 4))
      print(mooringflagcounts)
      flagcounts[[x]] <- mooringflagcounts
    }
    return(flagcounts)
  }
  

  
  #GROSS RANGE
  grflagsanddep <- data.frame(flags = grPARandsensor$flags, dep = grPARandsensor$deployment)
  grossrangeflagcounts <- mooringflagcounts(grflagsanddep)

  #CLIMATOLOGY
  clflagsanddep <- data.frame(flags = clflags, dep = clPARandsensor$deployment)
  clflagcounts <- mooringflagcounts(clflagsanddep)

  #FLAT LINE
  flflagsanddep <- data.frame(flags = flflagslong, dep = flPARandsensor$deployment)
  flflagcounts <- mooringflagcounts(flflagsanddep)

  #NEIGHBOUR
  neighbourflagsanddep <- data.frame(flags = neighbourflagslong, dep = neighbourPARandsensor$deployment)
  neighbourflagcounts <- mooringflagcounts(neighbourflagsanddep)
  
  
  creatingflagtable <- function(data)
  {
    table <- matrix(ncol = 4)
    for (x in seq(1,length(data)))
    {
      table <- rbind(table,data[[x]])
    }
    table <- table[-1,]
    rownames(table) <- unique(clflagsanddep$dep)
    table <- table[order(rownames(table)),]
    table <- rbind(table,c(sum(table[,1]),sum(table[,2]),sum(table[,3]),sum(table[,4])))
    return(table)
  }
  
  grflagtable <- creatingflagtable(grossrangeflagcounts)
  
  clflagtable <- creatingflagtable(clflagcounts)
  
  
  flflagtable <- creatingflagtable(flflagcounts)
  
  neighbourflagtable <- creatingflagtable(neighbourflagcounts)
  
  flagtotaltable <- grflagtable + clflagtable + flflagtable + neighbourflagtable
