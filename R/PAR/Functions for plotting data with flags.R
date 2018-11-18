
plottingflags <- function(sensor,data)
{
  data$dates <- as.Date(data$time, origin = "1950-01-01")
  pardata <- subset(data,data$sensor == sensor)
  print(sum(pardata$flags == 1))

  
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
                
            
                plot <- plot + scale_colour_manual(breaks = c("Fail"),
                                                   name = "",
                                                   values = c("red"))
                plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                                shape = c(22),
                                                                                fill = "red")))
                
                plot <- plot + ggtitle(paste(sensors$name[sensor]," (",sensors$deployment[sensor]," - ",sensors$depth[sensor],")", sep = ""))
                
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

                
                plot <- plot + scale_colour_manual(breaks = c("Suspect"),
                                                   name = "",
                                                   values = c("orange"))
                plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                                shape = c(22),
                                                                                fill = "orange")))
                
                plot <- plot + ggtitle(paste(sensors$name[sensor]," (",sensors$deployment[sensor]," - ",sensors$depth[sensor],")", sep = ""))
                
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

                
                
                plot <- plot + scale_colour_manual(breaks = c( "Suspect", "Fail"),
                                                   name = "",
                                                   values = c("red","orange"))
                plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                                shape = c(22,22),
                                                                                fill = c("orange","red"))))
                
                plot <- plot + ggtitle(paste(sensors$name[sensor]," (",sensors$deployment[sensor]," - ",sensors$depth[sensor],")", sep = ""))
                
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
        
        
        
        
        plot <- plot + scale_colour_manual(breaks = c("Mean PAR", "Fail"),
                                           name = "",
                                           values = c("red","black"))
        plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                        shape = c(22,22))))
        
        plot <- plot + ggtitle(paste(sensors$name[sensor]," (",sensors$deployment[sensor]," - ",sensors$depth[sensor],")", sep = ""))
        
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
        
        
        
        
        plot <- plot + scale_colour_manual(breaks = c("Mean PAR", "Suspect"),
                                           name = "",
                                           values = c("black","orange"))
        plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                        shape = c(22,22))))
        
        plot <- plot + ggtitle(paste(sensors$name[sensor]," (",sensors$deployment[sensor]," - ",sensors$depth[sensor],")", sep = ""))
        
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
       
        
        
        
        
        
        plot <- plot + scale_colour_manual(breaks = c("Mean PAR", "Suspect", "Fail"),
                                           name = "",
                                           values = c("red","black","orange"))
        plot <- plot + guides(colour = guide_legend(override.aes = list(size = 5,
                                                                        shape = c(22,22,22))))
        
        plot <- plot + ggtitle(paste(sensors$name[sensor]," (",sensors$deployment[sensor]," - ",sensors$depth[sensor],")", sep = ""))
        
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
