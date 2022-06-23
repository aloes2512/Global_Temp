nmdt<-function(x){
  x<-mutate(x,dttm=as.numeric(datetime))
}
CO2_data.dfr<-CO2_data.lst%>% 
  map(nmdt)%>%map_dfr(function(x){ x<- x%>%dplyr::select(dttm,site,CO2, lat,lon,alt)})
require(lubridate)
require(stringr)
CO2_data.yr<-CO2_data.dfr%>%mutate(datetime=as.POSIXct(dttm,origin="1970-01-01",tz="UTC"))%>%
  mutate(yr = format(datetime,format= "%Y")%>% 
           as.integer())%>%group_by(yr,site)%>%
  summarise(site=first(site),
            CO2_yr= mean(CO2,na.rm=T),
            lat=first(lat),lon=first(lon),alt=first(alt))
CO2_data.yr%>%subset(site=="MLO")
#
sel_yr<- function(x){res<- mutate(x,yr = format(datetime,format= "%Y")%>%
                                  as.integer())%>% group_by(yr)%>%
                              summarise(site=first(site),
                              CO2_yr= mean(CO2,na.rm=T),
                              lat=first(lat),lon=first(lon),alt=first(alt))
return(res)
}
CO2_data.Yr<-CO2_data.lst%>% map_dfr(sel_yr)

CO2_data.Yr%>% ggplot(aes(x=yr,y=CO2_yr,col=site))+
  geom_smooth(method = "auto")
