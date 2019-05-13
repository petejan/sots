sensors <- seq(1,32)
sensors <- data.frame(num = sensors,name = sapply(sensors,sensorname))
sensors$depth <- sapply(sensors$num,depthfromsensorforplots)
sensors$deployment <- sapply(sensors$num,mooringfromsensor)

sensor <- seq(1,32)

neighbourPARandsensor$ePAR <- clPARandsensor$ePAR



#neighbour
#this is the important one as it has the final flags
neighbourplots <- lapply(sensor,plottingflags,data = neighbourPARandsensor)


#manually fixing x axis range of plots that only have data for part of a deployment

#pulse 8 50m
neighbourplots[[11]] <- neighbourplots[[11]] + scale_x_date(labels = date_format("%b"),
                                                            limits = c(min(neighbourPARandsensor$dates[neighbourPARandsensor$sensor==12]),
                                                            max(neighbourPARandsensor$dates[neighbourPARandsensor$sensor==12])))



#sofs 2 40m
neighbourplots[[17]] <- neighbourplots[[17]] + xlim(min(neighbourPARandsensor$dates[neighbourPARandsensor$sensor==16]),
                                                    max(neighbourPARandsensor$dates[neighbourPARandsensor$sensor==16]))

#sofs 4 20m
neighbourplots[[21]] <- neighbourplots[[21]] + scale_x_date(labels = date_format("%b"),
                                                            limits = c(min(neighbourPARandsensor$dates[neighbourPARandsensor$sensor==22]),
                                                                       max(neighbourPARandsensor$dates[neighbourPARandsensor$sensor==22])))

#sofs 5 20m and 40m
neighbourplots[[28]] <- neighbourplots[[28]] + scale_x_date(labels = date_format("%b"),
                                                            limits = c(min(neighbourPARandsensor$dates[neighbourPARandsensor$sensor==29]),
                                                                       max(neighbourPARandsensor$dates[neighbourPARandsensor$sensor==29])))

neighbourplots[[27]] <- neighbourplots[[27]] + scale_x_date(labels = date_format("%b"),
                                                            limits = c(min(neighbourPARandsensor$dates[neighbourPARandsensor$sensor==29]),
                                                                       max(neighbourPARandsensor$dates[neighbourPARandsensor$sensor==29])))


#making plots into some kind of grid
pulse6plots <- rbind(ggplotGrob(neighbourplots[[2]]),
                     ggplotGrob(neighbourplots[[3]]),
                     ggplotGrob(neighbourplots[[1]]),
                     ggplotGrob(neighbourplots[[4]]),
                     size = "first")

pulse7plots <- rbind(ggplotGrob(neighbourplots[[8]]),
                     ggplotGrob(neighbourplots[[9]]),
                     ggplotGrob(neighbourplots[[7]]),
                     ggplotGrob(neighbourplots[[10]]),
                     size = "first")

pulse8plots <- rbind(ggplotGrob(neighbourplots[[14]]),
                     ggplotGrob(neighbourplots[[12]]),
                     ggplotGrob(neighbourplots[[13]]),
                     ggplotGrob(neighbourplots[[11]]),
                     size = "first")


pulse10plots <- rbind(ggplotGrob(neighbourplots[[23]]),
                      ggplotGrob(neighbourplots[[24]]),
                      ggplotGrob(neighbourplots[[25]]),
                      ggplotGrob(neighbourplots[[26]]),
                      size = "first")

pulse11plots <- rbind(ggplotGrob(neighbourplots[[31]]),
                      ggplotGrob(neighbourplots[[30]]),
                      ggplotGrob(neighbourplots[[32]]),
                      size = "first")

sofs1plots <- rbind(ggplotGrob(neighbourplots[[5]]),
                    ggplotGrob(neighbourplots[[6]]),
                    size = "last")

sofs2plots <- rbind(ggplotGrob(neighbourplots[[15]]),
                    ggplotGrob(neighbourplots[[16]]),
                    ggplotGrob(neighbourplots[[17]]),
                    size = "first")


sofs4plots <- rbind(ggplotGrob(neighbourplots[[22]]),
                    ggplotGrob(neighbourplots[[21]]),
                    ggplotGrob(neighbourplots[[20]]),
                    size = "first")

sofs5plots <- rbind(ggplotGrob(neighbourplots[[29]]),
                    ggplotGrob(neighbourplots[[28]]),
                    ggplotGrob(neighbourplots[[27]]),
                    size = "first")


#generating the plots for export
  #pulse 9 and sofs 3 only 1 sensor, so pulled directly from neighbourplots (sensor nos 18 and 19)
  grid.draw(pulse6plots)

  grid.draw(pulse7plots)
 
  grid.draw(pulse8plots)

  neighbourplots[19]

  grid.draw(pulse10plots)

  grid.draw(pulse11plots)
   
  grid.draw(sofs1plots)
 
  grid.draw(sofs2plots)
  
  neighbourplots[18]

  grid.draw(sofs4plots)
  
  grid.draw(sofs5plots)

  

