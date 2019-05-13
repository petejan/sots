#plotting flags with estimated PAR values
plottingflags <- function(x,data)
{
  #getting data
  data$dates <- as.Date(data$time, origin = "1950-01-01")
  pardata <- subset(data,data$sensor == x)
  
  suspardata <- subset(pardata,pardata$flags == 3)
  failpardata <- subset(pardata,pardata$flags == 4)
  
  
  #organising mean data
    #check that there is good data to make a mean
    if (sum(pardata$flags == 1) != 0)
    {
    meanpardata <- testdailymeans(pardata)
      
      #check that there is enough data to cut off the first and last day
      if (length(meanpardata$dates) > 4)
        {
        meanpardata <- meanpardata[meanpardata$day != max(meanpardata$day),]
        meanpardata <- meanpardata[meanpardata$day != min(meanpardata$day),]
        }
    meandates <- data.frame(dates = seq.Date(min(na.omit(meanpardata$dates)),max(na.omit(meanpardata$dates)),by = "day"))
    meanpardates <- merge(meandates,meanpardata,by = "dates", all.x = TRUE)
    }

 
  #organising mean ePAR
    meanePARdata <- testdailymeansePAR(pardata,"reg")
    meanePARdata <- na.omit(meanePARdata)
    #check that there is enough data to cut off the first and last day
    if (length(meanePARdata$dates) > 4)
    {
      print("reg")
      meanePARdata <- meanePARdata[meanePARdata$day != max(meanePARdata$day),]
      meanePARdata <- meanePARdata[meanePARdata$day != min(meanePARdata$day),]
    }
    meanepardates <- data.frame(dates = seq.Date(min(na.omit(meanePARdata$dates)),max(na.omit(meanePARdata$dates)),by = "day"))
    meanePARdata <- merge(meanepardates,meanePARdata,by = "dates", all.x = TRUE)
    
    
    meanhighePARdata <- testdailymeansePAR(pardata,"high")
    meanhighePARdata <- na.omit(meanhighePARdata)
    #check that there is enough data to cut off the first and last day
    if (length(meanhighePARdata$dates) > 4)
    {
      print("high")
      meanhighePARdata <- meanhighePARdata[meanhighePARdata$day != max(meanhighePARdata$day),]
      meanhighePARdata <- meanhighePARdata[meanhighePARdata$day != min(meanhighePARdata$day),]
    }
    meanepardates <- data.frame(dates = seq.Date(min(na.omit(meanhighePARdata$dates)),max(na.omit(meanhighePARdata$dates)),by = "day"))
    meanhighePARdata <- merge(meanepardates,meanhighePARdata,by = "dates", all.x = TRUE)
    
    
    
    meanlowePARdata <- testdailymeansePAR(pardata,"low")
    meanlowePARdata <- na.omit(meanlowePARdata)
    #check that there is enough data to cut off the first and last day
    if (length(meanlowePARdata$dates) > 4)
    {
      print("low")
      meanlowePARdata <- meanlowePARdata[meanlowePARdata$day != max(meanlowePARdata$day),]
      meanlowePARdata <- meanlowePARdata[meanlowePARdata$day != min(meanlowePARdata$day),]
    }
    meanepardates <- data.frame(dates = seq.Date(min(na.omit(meanlowePARdata$dates)),max(na.omit(meanlowePARdata$dates)),by = "day"))
    meanlowePARdata <- merge(meanepardates,meanlowePARdata,by = "dates", all.x = TRUE)
    
    

  
  #if there isn't any good data
  if (sum(pardata$flags == 1) == 0)
  {
        #suspect flags absent
        if (length(suspardata[,1]) == 0)
        {
          print("nogoodnosus")
          plot <- ggplot()
          plot <- plot + geom_point(data = failpardata,aes(dates,par,colour = "Fail"),size = 0.25)
          plot <- plot + geom_line(data = meanePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR'),linetype = "dashed")
          plot <- plot + geom_line(data = meanhighePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR (Chl = 1)'),linetype = "dashed")
          plot <- plot + geom_line(data = meanlowePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR (Chl = 0.5'),linetype = "dashed")
          
          plot <- plot + scale_colour_manual(breaks = c("Fail","Mean Estimated PAR",'Mean Estimated PAR (Chl = 1)','Mean Estimated PAR (Chl = 0.5'),
                                             name = "",
                                             values = c("red","#0000FF","dark green","dark purple"))
          plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                          shape = c(22,22,22,22))))
          
          plot <- plot + ggtitle(paste(sensors$name[x]," (",gsub("-"," ",sensors$deployment[x])," - ",sensors$depth[x],")", sep = ""))
          plot <- plot + scale_x_date(labels = date_format("%b"))
          
          plot <- plot + xlab("Date")
          plot <- plot + ylab(PAR~(mu*mol~m^-2~s^-1))
          plot <- plot + theme(axis.text = element_text(size = 15, colour = 'black'),
                               legend.key=element_blank(),
                               plot.title = element_text(size = 20,hjust = 0.5),
                               axis.title = element_text(size = 17),
                               legend.text = element_text(size = 13),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               panel.background = element_blank(), axis.line = element_line(colour = "black"))
          
          plot <- plot + scale_y_continuous(expand = c(0, 0))
          
          return(plot)
        }
    
        #fail flags absent
        if (length(failpardata[,1]) == 0)
        {
          print("nogoodnofail")
          plot <- ggplot()
          plot <- plot + geom_point(data = suspardata,aes(dates,par,colour = "Suspect"),size = 0.25)
          plot <- plot + geom_line(data = meanePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR'),linetype = "dashed")
          plot <- plot + geom_line(data = meanhighePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR (Chl = 1)'),linetype = "dashed")
          plot <- plot + geom_line(data = meanlowePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR (Chl = 0.5'),linetype = "dashed")
          
          plot <- plot + scale_colour_manual(breaks = c("Suspect","Mean Estimated PAR",'Mean Estimated PAR (Chl = 1)','Mean Estimated PAR (Chl = 0.5)'),
                                             name = "",
                                             values = c("#0000FF","dark green","purple","orange"))
          plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                          shape = c(22,22,22,22)
                                                                          )))
          
          plot <- plot + ggtitle(paste(sensors$name[x]," (",gsub("-"," ",sensors$deployment[x])," - ",sensors$depth[x],")", sep = ""))
          plot <- plot + scale_x_date(labels = date_format("%b"))
          
          plot <- plot + xlab("Date")
          plot <- plot + ylab(PAR~(mu*mol~m^-2~s^-1))
          plot <- plot + theme(axis.text = element_text(size = 15, colour = 'black'),
                               legend.key=element_blank(),
                               plot.title = element_text(size = 20,hjust = 0.5),
                               axis.title = element_text(size = 17),
                               legend.text = element_text(size = 13),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               panel.background = element_blank(), axis.line = element_line(colour = "black"))
          
          plot <- plot + scale_y_continuous(expand = c(0, 0))
          
          
          
          return(plot)
        }
    
        #both flag types present
        else
        {
          print("nogood")
          plot <- ggplot()
          plot <- plot + geom_point(data = suspardata,aes(dates,par,colour = "Suspect"),size = 0.25)
          plot <- plot + geom_point(data = failpardata,aes(dates,par,colour = "Fail"),size = 0.25)
          plot <- plot + geom_line(data = meanePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR'),linetype = "dashed")
          plot <- plot + geom_line(data = meanhighePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR (Chl = 1)'),linetype = "dashed")
          plot <- plot + geom_line(data = meanlowePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR (Chl = 0.5'),linetype = "dashed")
          
          plot <- plot + scale_colour_manual(breaks = c( "Suspect", "Fail","Mean Estimated PAR",'Mean Estimated PAR (Chl = 1)','Mean Estimated PAR (Chl = 0.5)'),
                                             name = "",
                                             values = c("red","#0000FF","dark green","dark purple","orange"))
          plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                          shape = c(22,22,22,22,22))))
          
          plot <- plot + ggtitle(paste(sensors$name[x]," (",gsub("-"," ",sensors$deployment[x])," - ",sensors$depth[x],")", sep = ""))
          plot <- plot + scale_x_date(labels = date_format("%b"))
          
          plot <- plot + xlab("Date")
          plot <- plot + ylab(PAR~(mu*mol~m^-2~s^-1))
          plot <- plot + theme(axis.text = element_text(size = 15, colour = 'black'),
                               legend.key=element_blank(),
                               plot.title = element_text(size = 20,hjust = 0.5),
                               axis.title = element_text(size = 17),
                               legend.text = element_text(size = 13),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               panel.background = element_blank(), axis.line = element_line(colour = "black"))
          
          plot <- plot + scale_y_continuous(expand = c(0, 0))
          
          
          return(plot)
        }
  }
  
  

  #if there is good data
  else
  {
    #neither fail nor sus present
        if (length(failpardata[,1]) == 0 & length(suspardata[,1]) == 0)
        {
          print("nofailnosus")
          plot <- ggplot()
          plot <- plot + geom_path(data = meanpardates,aes(dates,meanpar, colour = "Mean PAR"))
          plot <- plot + geom_path(data = meanePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR'),linetype = "dashed",size = 1)
          plot <- plot + geom_line(data = meanhighePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR (Chl = 1)'),linetype = "dashed")
          plot <- plot + geom_line(data = meanlowePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR (Chl = 0.5'),linetype = "dashed")
          
          
          
          
          plot <- plot + scale_colour_manual(breaks = c("Mean Estimated PAR","Mean PAR",'Mean Estimated PAR (Chl = 1)','Mean Estimated PAR (Chl = 0.5)'),
                                             name = "",
                                             values = c("#0000FF","dark green","dark purple","black"))
          plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                          shape = c(22,22,22,22))))
          
          print(paste(sensors$name[x]," (",gsub("-"," ",sensors$deployment[x])," - ",sensors$depth[x],")", sep = ""))
          plot <- plot + ggtitle(paste(sensors$name[x]," (",gsub("-"," ",sensors$deployment[x])," - ",sensors$depth[x],")", sep = ""))
          plot <- plot + scale_x_date(labels = date_format("%b"))
          
          plot <- plot + xlab("Date")
          plot <- plot + ylab(PAR~(mu*mol~m^-2~s^-1))
          plot <- plot + theme(axis.text = element_text(size = 15, colour = 'black'),
                               legend.key=element_blank(),
                               plot.title = element_text(size = 20,hjust = 0.5),
                               axis.title = element_text(size = 17),
                               legend.text = element_text(size = 13),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               panel.background = element_blank(), axis.line = element_line(colour = "black"))
          
          plot <- plot + scale_y_continuous(expand = c(0, 0))
          
          
          return(plot)
        }
        #suspect flags absent
        if (length(suspardata[,1]) == 0)
        {
          print("nosus")
          plot <- ggplot()
          plot <- plot + geom_point(data = failpardata,aes(dates,par,colour = "Fail"),size = 0.25)
          plot <- plot + geom_path(data = meanpardates,aes(dates,meanpar, colour = "Mean PAR"))
          plot <- plot + geom_line(data = meanePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR'),linetype = "dashed")
          plot <- plot + geom_line(data = meanhighePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR (Chl = 1)'),linetype = "dashed")
          plot <- plot + geom_line(data = meanlowePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR (Chl = 0.5'),linetype = "dashed")
          
          
          plot <- plot + scale_colour_manual(breaks = c("Mean Estimated PAR","Mean PAR", "Fail",'Mean Estimated PAR (Chl = 1)','Mean Estimated PAR (Chl = 0.5)'),
                                             name = "",
                                             values = c("red","#0000FF","dark green","dark purple","black"))
          plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                          shape = c(22,22,22,22,22))))
          
          plot <- plot + ggtitle(paste(sensors$name[x]," (",gsub("-"," ",sensors$deployment[x])," - ",sensors$depth[x],")", sep = ""))
          plot <- plot + scale_x_date(labels = date_format("%b"))
          
          plot <- plot + xlab("Date")
          plot <- plot + ylab(PAR~(mu*mol~m^-2~s^-1))
          plot <- plot + theme(axis.text = element_text(size = 15, colour = 'black'),
                               legend.key=element_blank(),
                               plot.title = element_text(size = 20,hjust = 0.5),
                               axis.title = element_text(size = 17),
                               legend.text = element_text(size = 13),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               panel.background = element_blank(), axis.line = element_line(colour = "black"))
          
          plot <- plot + scale_y_continuous(expand = c(0, 0))
          
          
          return(plot)
        }
        
        #fail flags absent
        if (length(failpardata[,1]) == 0)
        {
          print("nofail")
          plot <- ggplot()
          plot <- plot + geom_point(data = suspardata,aes(dates,par,colour = "Suspect"),size = 0.25)
          plot <- plot + geom_path(data = meanpardates,aes(dates,meanpar, colour = "Mean PAR"))
          plot <- plot + geom_path(data = meanePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR'),linetype = "dashed")
          plot <- plot + geom_line(data = meanhighePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR (Chl = 1)'),linetype = "dashed")
          plot <- plot + geom_line(data = meanlowePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR (Chl = 0.5'),linetype = "dashed")
          
          
          plot <- plot + scale_colour_manual(breaks = c("Mean Estimated PAR","Mean PAR", "Suspect",'Mean Estimated PAR (Chl = 1)','Mean Estimated PAR (Chl = 0.5'),
                                             name = "",
                                             values = c("#0000FF","dark green","purple","black","orange"))
          plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                          shape = c(22,22,22,22,22))))
          
          plot <- plot + ggtitle(paste(sensors$name[x]," (",gsub("-"," ",sensors$deployment[x])," - ",sensors$depth[x],")", sep = ""))
          plot <- plot + scale_x_date(labels = date_format("%b"))
          
          plot <- plot + xlab("Date")
          plot <- plot + ylab(PAR~(mu*mol~m^-2~s^-1))
          plot <- plot + theme(axis.text = element_text(size = 15, colour = 'black'),
                               legend.key=element_blank(),
                               plot.title = element_text(size = 20,hjust = 0.5),
                               axis.title = element_text(size = 17),
                               legend.text = element_text(size = 13),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               panel.background = element_blank(), axis.line = element_line(colour = "black"))
          
          plot <- plot + scale_y_continuous(expand = c(0, 0))
          
          
          return(plot)
        }
      
    
        
        
      
      
        #both flag types present
        if (length(failpardata[,1]) != 0 & length(suspardata[,1]) != 0)
        {
          print("allflags")
          plot <- ggplot()
          plot <- plot + geom_point(data = suspardata,aes(dates,par,colour = "Suspect"),size = 0.25)
          plot <- plot + geom_point(data = failpardata,aes(dates,par,colour = "Fail"),size = 0.25)
          plot <- plot + geom_path(data = meanpardates,aes(dates,meanpar, colour = "Mean PAR"))
          plot <- plot + geom_line(data = meanePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR'),linetype = "dashed",size = 1)
          plot <- plot + geom_line(data = meanhighePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR (Chl = 1)'),linetype = "dashed")
          plot <- plot + geom_line(data = meanlowePARdata,aes(dates,meanepar,colour = 'Mean Estimated PAR (Chl = 0.5'),linetype = "dashed")
          
          
          
          
          plot <- plot + scale_colour_manual(breaks = c("Mean PAR", "Suspect", "Fail","Mean Estimated PAR",'Mean Estimated PAR (Chl = 1)','Mean Estimated PAR (Chl = 0.5)'),
                                             name = "",
                                             values = c("red","#0000FF","dark purple","dark green","black","orange"))
          plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                          shape = c(22,22,22,22,22,22))))
          
          plot <- plot + ggtitle(paste(sensors$name[x]," (",gsub("-"," ",sensors$deployment[x])," - ",sensors$depth[x],")", sep = ""))
          plot <- plot + scale_x_date(labels = date_format("%b"))
          
          plot <- plot + xlab("Date")
          plot <- plot + ylab(PAR~(mu*mol~m^-2~s^-1))
          plot <- plot + theme(axis.text = element_text(size = 15, colour = 'black'),
                               legend.key=element_blank(),
                               plot.title = element_text(size = 20,hjust = 0.5),
                               axis.title = element_text(size = 17),
                               legend.text = element_text(size = 13),
                               panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               panel.background = element_blank(), axis.line = element_line(colour = "black"))
          
          plot <- plot + scale_y_continuous(expand = c(0, 0))
          
          
          return(plot)
        }
    

    }
    
    
    
}