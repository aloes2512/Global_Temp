# Total Solar Irradiance (TSI)
#Solar Radiation and Climate Experiment (SORCE) is a NASA-sponsored satellite mission 
# compare Wikipedia radiative forcing "https://en.wikipedia.org/wiki/Radiative_forcing"
browseURL("https://en.wikipedia.org/wiki/Radiative_forcing")
SORCE_url<-"https://ceres.larc.nasa.gov/documents/TSIdata/CERES_EBAF_Ed2.8_DailyTSI.txt"

dt<-read.delim(SORCE_url,  sep = "", quote = "\"",
           dec = ".", fill = TRUE)#col.names = c("year","month","day","TSI"),
colnames(dt )
head(dt)
require(lubridate)
tsi_dat<-dt%>% mutate(datetm= ymd(paste(year,mon,day)))%>%
  dplyr::select(datetm,tsi.1au)
tsi_dat%>% ggplot(aes(x= datetm,y=tsi.1au))+
  geom_point(size= 0.1)+
  geom_smooth(col="red")
