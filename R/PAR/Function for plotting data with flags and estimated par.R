#plotting flags with estimated PAR values
testplottingflags <- function(x,data)
{
  data$dates <- as.Date(data$time, origin = "1950-01-01")
  pardata <- subset(data,data$sensor == x)
  ePARdata <- subset(clPARandsensor,clPARandsensor$sensor == x)
  
  meanePARdata <- dailymeansePAR(ePARdata)
  meanepardates <- data.frame(dates = seq.Date(min(meanePARdata$dates),max(meanePARdata$dates),by = "day"))
  meanePARdata <- merge(meanepardates,meanePARdata,by = "dates", all.x = TRUE)
  
  length <- length(meanePARdata[,1])
  print(length)
  length2 <- length - 20
  print(length2)
  
  meanePARdata <- meanePARdata[-(length2:length),]
  
  suspardata <- subset(pardata,pardata$flags == 3)
  
  
  failpardata <- subset(pardata,pardata$flags == 4)
  
  #if there isn't any good data
  if (sum(pardata$flags == 1) == 0)
  {
    print("oh")
    #suspect flags absent
    if (length(suspardata[,1]) == 0)
    {
      print("nosus")
      plot <- ggplot()
      plot <- plot + geom_point(data = failpardata,aes(dates,par,colour = "Fail"),size = 0.25)
      plot <- plot + geom_line(data = meanePARdata[2:nrow(meanePARdata),],aes(dates,meanepar,colour = 'Mean Estimated PAR'),linetype = "dashed")
      
      plot <- plot + scale_colour_manual(breaks = c("Fail","Mean Estimated PAR"),
                                         name = "",
                                         values = c("red","#0000FF"))
      plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                      shape = c(22,22),
                                                                      fill = "red","dark green")))
      
      plot <- plot + ggtitle(paste(sensors$name[x]," (",sensors$deployment[x]," - ",sensors$depth[x],")", sep = ""))
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
      
      #ggsave(paste("plot",x,".png", sep = ''),plot = plot)
      
      return(plot)
    }
    
    #fail flags absent
    if (length(failpardata[,1]) == 0)
    {
      print("nofail")
      plot <- ggplot()
      plot <- plot + geom_point(data = suspardata,aes(dates,par,colour = "Suspect"),size = 0.25)
      plot <- plot + geom_line(data = meanePARdata[2:nrow(meanePARdata),],aes(dates,meanepar,colour = 'Mean Estimated PAR'),linetype = "dashed")
      
      plot <- plot + scale_colour_manual(breaks = c("Suspect","Mean Estimated PAR"),
                                         name = "",
                                         values = c("orange","#0000FF"))
      plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                      shape = c(22,22),
                                                                      fill = "orange","dark green")))
      
      plot <- plot + ggtitle(paste(sensors$name[x]," (",sensors$deployment[x]," - ",sensors$depth[x],")", sep = ""))
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
      
      #ggsave(paste("plot",x,".png", sep = ''),plot = plot)
      
      
      return(plot)
    }
    
    #both flag types present
    else
    {
      print("allg")
      plot <- ggplot()
      plot <- plot + geom_point(data = suspardata,aes(dates,par,colour = "Suspect"),size = 0.25)
      plot <- plot + geom_point(data = failpardata,aes(dates,par,colour = "Fail"),size = 0.25)
      plot <- plot + geom_line(data = meanePARdata[2:nrow(meanePARdata),],aes(dates,meanepar,colour = 'Mean Estimated PAR'),linetype = "dashed")
      
      plot <- plot + scale_colour_manual(breaks = c( "Suspect", "Fail","Mean Estimated PAR"),
                                         name = "",
                                         values = c("red","#0000FF","orange"))
      plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                      shape = c(22,22,22),
                                                                      fill = c("orange","red","dark green"))))
      
      plot <- plot + ggtitle(paste(sensors$name[x]," (",sensors$deployment[x]," - ",sensors$depth[x],")", sep = ""))
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
      
      #ggsave(paste("plot",x,".png", sep = ''),plot = plot)
      
      return(plot)
    }
  }
  
  else
  {
    meanpardata <- dailymeans(pardata)
    meandates <- data.frame(dates = seq.Date(min(meanpardata$dates),max(meanpardata$dates),by = "day"))
    meanpardates <- merge(meandates,meanpardata,by = "dates", all.x = TRUE)
    
    
    
    
    #suspect flags absent
    if (length(suspardata[,1]) == 0)
    {
      print("nosus")
      plot <- ggplot()
      plot <- plot + geom_point(data = failpardata,aes(dates,par,colour = "Fail"),size = 0.25)
      plot <- plot + geom_line(data = meanpardates,aes(dates,meanpar, colour = "Mean PAR"))
      plot <- plot + geom_line(data = meanePARdata[2:nrow(meanePARdata),],aes(dates,meanepar,colour = 'Mean Estimated PAR'),linetype = "dashed")
      
      
      
      plot <- plot + scale_colour_manual(breaks = c("Mean Estimated PAR","Mean PAR", "Fail"),
                                         name = "",
                                         values = c("red","#0000FF","black"))
      plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                      shape = c(22,22,22))))
      
      plot <- plot + ggtitle(paste(sensors$name[x]," (",sensors$deployment[x]," - ",sensors$depth[x],")", sep = ""))
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
      
      #ggsave(paste("plot",x,".png", sep = ''),plot = plot)
      
      return(plot)
    }
    
    #fail flags absent
    if (length(failpardata[,1]) == 0)
    {
      print("nofail")
      plot <- ggplot()
      plot <- plot + geom_point(data = suspardata,aes(dates,par,colour = "Suspect"),size = 0.25)
      plot <- plot + geom_line(data = meanpardates,aes(dates,meanpar, colour = "Mean PAR"))
      plot <- plot + geom_line(data = meanePARdata[2:nrow(meanePARdata),],aes(dates,meanepar,colour = 'Mean Estimated PAR'),linetype = "dashed")
      
      
      
      plot <- plot + scale_colour_manual(breaks = c("Mean Estimated PAR","Mean PAR", "Suspect"),
                                         name = "",
                                         values = c("#0000FF","black","orange"))
      plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                      shape = c(22,22,22))))
      
      plot <- plot + ggtitle(paste(sensors$name[x]," (",sensors$deployment[x]," - ",sensors$depth[x],")", sep = ""))
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
      
      #ggsave(paste("plot",x,".png", sep = ''),plot = plot)
      
      return(plot)
    }
    
    #both flag types present
    else
    {
      print("allg")
      plot <- ggplot()
      plot <- plot + geom_point(data = suspardata,aes(dates,par,colour = "Suspect"),size = 0.25)
      plot <- plot + geom_point(data = failpardata,aes(dates,par,colour = "Fail"),size = 0.25)
      plot <- plot + geom_line(data = meanpardates,aes(dates,meanpar, colour = "Mean PAR"))
      plot <- plot + geom_line(data = meanePARdata[2:nrow(meanePARdata),],aes(dates,meanepar,colour = 'Mean Estimated PAR'),linetype = "dashed",size = 1)
      
      
      
      
      
      plot <- plot + scale_colour_manual(breaks = c("Mean PAR", "Suspect", "Fail","Mean Estimated PAR"),
                                         name = "",
                                         values = c("red","#0000FF","black","orange"))
      plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                      shape = c(22,22,22,22))))
      
      plot <- plot + ggtitle(paste(sensors$name[x]," (",sensors$deployment[x]," - ",sensors$depth[x],")", sep = ""))
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
      
      #ggsave(paste("plot",x,".png", sep = ''),plot = plot)

      return(plot)
    }
  }
}
