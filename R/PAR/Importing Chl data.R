PULSE6FLNTUS <- as.data.frame(read.nc(open.nc("IMOS_ABOS-SOTS_OB_20090930_Pulse_FV01_Pulse-6-2009-FLNTUS-1215-38m_END-20100318_C-20180102.nc")))
PULSE7FLNTUS <- as.data.frame(read.nc(open.nc("IMOS_ABOS-SOTS_OB_20100912_Pulse_FV01_Pulse-7-2010-FLNTUS-1215-31m_END-20110417_C-20180102.nc")))
SOFS1FLNTUS <- as.data.frame(read.nc(open.nc("IMOS_ABOS-SOTS_OBSPT_20100408_SOFS_FV01_SOFS-1-2010-FLNTUS-1186-1m_END-20100624_C-20180102.nc")))
PULSE8FLNTUS <- as.data.frame(read.nc(open.nc("IMOS_ABOS-SOTS_OB_20110803_Pulse_FV01_Pulse-8-2011-FLNTUS-1215-34m_END-20120224_C-20180102.nc")))
SOFS2FLNTUS <- as.data.frame(read.nc(open.nc("IMOS_ABOS-SOTS_OBSPT_20111125_SOFS_FV01_SOFS-2-2011-FLNTUS-1186-2m_END-20111229_C-20180102.nc")))
PULSE9FLNTUS <- as.data.frame(read.nc(open.nc("IMOS_ABOS-SOTS_OB_20120717_Pulse_FV01_Pulse-9-2012-FLNTUS-1596-39m_END-20130103_C-20180102.nc")))
SOFS3FLNTUS <- as.data.frame(read.nc(open.nc("IMOS_ABOS-SOTS_OBSPT_20120714_SOFS_FV01_SOFS-3-2012-FLNTUS-1802-2m_END-20121214_C-20180102.nc")))
PULSE10FLNTUS <- as.data.frame(read.nc(open.nc("IMOS_ABOS-SOTS_OB_20130507_Pulse_FV01_Pulse-10-2013-FLNTUS-1186-28m_END-20131013_C-20180102.nc")))
SOFS4FLNTUS <- as.data.frame(read.nc(open.nc("IMOS_ABOS-SOTS_OBSPT_20130501_SOFS_FV01_SOFS-4-2013-FLNTUS-1802-1m_END-20131014_C-20180102.nc")))
PULSE11FLNTUS <- as.data.frame(read.nc(open.nc("IMOS_ABOS-SOTS_OB_20150327_Pulse_FV01_Pulse-11-2015-FLNTUS-1186-28m_END-20160319_C-20180102.nc")))
SOFS5FLNTUS <- as.data.frame(read.nc(open.nc("IMOS_ABOS-SOTS_OBSPT_20150324_SOFS_FV01_SOFS-5-2015-FLNTUS-1802-1m_END-20151124_C-20180102.nc")))



cphldataPULSE <-rbind(PULSE6FLNTUS[,c("TIME","CPHL","CPHL_quality_control","station_name")],
                       PULSE7FLNTUS[,c("TIME","CPHL","CPHL_quality_control","station_name")],
                       PULSE8FLNTUS[,c("TIME","CPHL","CPHL_quality_control","station_name")],
                       PULSE9FLNTUS[,c("TIME","CPHL","CPHL_quality_control","station_name")],
                       PULSE10FLNTUS[,c("TIME","CPHL","CPHL_quality_control","station_name")],
                       PULSE11FLNTUS[,c("TIME","CPHL","CPHL_quality_control","station_name")])
cphldataSOFS <- rbind(SOFS1FLNTUS[,c("TIME","CPHL","CPHL_quality_control","station_name")],
                      SOFS2FLNTUS[,c("TIME","CPHL","CPHL_quality_control","station_name")],
                      SOFS3FLNTUS[,c("TIME","CPHL","CPHL_quality_control","station_name")],
                      SOFS4FLNTUS[,c("TIME","CPHL","CPHL_quality_control","station_name")],
                      SOFS5FLNTUS[,c("TIME","CPHL","CPHL_quality_control","station_name")])

cphldataPULSE$station_name <- substring(cphldataPULSE$station_name,first = 7)
cphldataSOFS$station_name <- substring(cphldataSOFS$station_name,first = 6)

cphldata <- rbind(cphldataPULSE,cphldataSOFS)
