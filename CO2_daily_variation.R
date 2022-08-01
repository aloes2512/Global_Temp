library(tidyverse)
library(lubridate)
#station parameters are listed by NOAA
station_sites<-readRDS("data/station_sites.rds")
station_sites%>%head()

NOAA_CO2<-readRDS("data/NOAA_CO2.rds")
head(NOAA_CO2)
NOAA_CO2<-NOAA_CO2%>% mutate(HR= format(datetime,"%H"),HR=as.numeric(HR))
NOAA_CO2$lat%>% summary
NOAA_CO2%>%mutate(LT=cut(lat,12,labels=LETTERS[1:12]))%>%
  ggplot(aes(x=LT,col= Site))+
  geom_histogram(stat = "count")

Site_lat<-NOAA_CO2%>% mutate(LT=cut_width(lat,width=15))%>%
  group_by(LT,Site)%>%summarise(Observ= n())
Site_lat%>% group_by(LT)%>% summarise(Nr.sites= n())%>% 
  ggplot(aes(x=LT,y=Nr.sites))+ 
  labs(x= "Latitude", y = "Number of Sites")+
  geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
#
Site_lat%>% group_by(LT)%>%summarise(Nr.sites= n())
#NOAA grouped by latitude of site 15°intervall width
NOAA_CO2<-NOAA_CO2%>% mutate(Hr= format(datetime,"%H"),Wk= format(datetime,"%W"))
NOAA_CO2%>% subset((lat>(+37.5))&(lat<=(+52.5)))%>% 
  group_by(Site,Hr)%>%
  summarise(CO2= mean(CO2, na.rm=T))%>%
  ggplot(aes(x=as.numeric(Hr),y=CO2,col=Site))+
  geom_smooth()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
NOAA_CO2%>% subset((lat>(+52.5))&(lat<=(+67.5)))%>% 
  group_by(Site,Hr)%>%
  summarise(CO2= mean(CO2, na.rm=T))%>%
  ggplot(aes(x=as.numeric(Hr),y=CO2,col=Site))+
  geom_smooth()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
NOAA_CO2%>% subset((lat>(+67.5)))%>% 
  group_by(Site,Hr)%>%
  summarise(CO2= mean(CO2, na.rm=T))%>%
  ggplot(aes(x=as.numeric(Hr),y=CO2,col=Site))+
  geom_smooth()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
# NOAA Baseline-Stations: BRW,MLO,SMO,SPO
noaa_base<-c("BRW","MLO","SMO","SPO")
noaa_bs.prms<-NOAA_CO2%>% subset(Site %in% noaa_base)%>%
  group_by(Site)%>% summarise(Site=first(Site),Lat=first(lat),Alt= first(alt),CO2=mean(CO2))
# Overview CO2 trends
library(mgcv)
subsetList <- function(myList, elementNames) {
  map(elementNames, function(x) myList[[x]])
}
NOAA_base.Sites_CO2<-subsetList(myList=CO2_data.lst,elementNames = noaa_base)
summary(NOAA_base.Sites_CO2)
names(NOAA_base.Sites_CO2)<-noaa_base
 map_df(NOAA_base.Sites_CO2,bind_rows)%>% ggplot(aes(x=datetime,y= CO2,col=site))+
   geom_point(size= 0.1)
 summary(NOAA_base.Sites_CO2[[1]])
BRW_annual_plt<-NOAA_base.Sites_CO2[[1]]%>% mutate(WK= as.numeric(format(datetime,"%W")))%>%
  group_by(WK)%>% summarise(CO2= mean(CO2,na.rm=T))%>% 
  ggplot(aes(x=WK,y=CO2))+
  geom_point(size= 0.2)+
  geom_smooth()+
  labs(x = "week of year",y="CO2mean [ppm]")
BRW_annual_plt+ ggtitle("CO2-mean Immissions per week @ Barrow ",
                        subtitle= "46 years average 1973 to 2020") 
WK_mean<- function(my_site){
  WK_data<- my_site%>% mutate(WK=as.numeric(format(datetime,"%W")))%>%
    group_by(WK)%>%
    summarise(CO2=mean(CO2,na.rm = T),Site=first(site),Lat=first(lat),Alt=first(alt))
    return(WK_data)
}
WK_mean(NOAA_base.Sites_CO2[[2]])
NOAA_base_wk.mean_CO2<-NOAA_base.Sites_CO2%>% map_df(WK_mean)
NOAA_base_wk.mean_CO2%>% head(1)
NOAA_base_wk.plt<-NOAA_base_wk.mean_CO2%>% ggplot(aes(x=WK,y=CO2,col= Site))+
  geom_smooth(method="gam",se= F)+
  geom_point(size= 0.3)
#=============================
# monthly variation
mnth_mean<- function(my_site){
  Mth_data<- my_site%>% mutate(d=as.numeric(format(datetime,"%d")))%>%
    group_by(d)%>%
    summarise(CO2=mean(CO2,na.rm = T),Site=first(site),Lat=first(lat),Alt=first(alt))
  return(Mth_data)
}
mnth_mean(NOAA_base.Sites_CO2[[1]])%>% ggplot(aes(x=d,y= CO2))+
  geom_point()+
  geom_smooth(method="gam",formula= y ~ s(x,k=24))
# daily variation
day_data<-function(my_site){
  Day_data<- my_site%>% mutate(H=as.numeric(format(datetime,"%H")))%>%
    group_by(H)%>%
    summarise(CO2=mean(CO2,na.rm = T),Site=first(site),Lat=first(lat),Alt=first(alt))
  return(Day_data)
}
day_data(NOAA_base.Sites_CO2[[3]])%>% ggplot(aes(x=H,y= CO2))+
  geom_point()+
  geom_smooth()
NOAA_CO2<-readRDS("data/NOAA_CO2.rds")
summary(NOAA_CO2)
x<-NOAA_CO2%>% group_by(Site)%>% nest()
WK_mean.<- function(my_site){
  WK_data<- my_site%>% mutate(WK=as.numeric(format(datetime,"%W")))%>%
    group_by(WK)%>%
    summarise(CO2=mean(CO2,na.rm = T),Site=first(Site),Lat=first(lat),Alt=first(alt))
  return(WK_data)
}
NOAA_base.Sites_CO2%>%summary()
NOAA_all.Sites_CO2<-subsetList(myList=CO2_data.lst,elementNames = names(CO2_data.lst))
names(NOAA_all.Sites_CO2)<-names(CO2_data.lst)
NOAA_all_wk.mean_CO2<-NOAA_all.Sites_CO2%>%map_df(WK_mean)
#all sites Changes per Calendarweek
NOAA_all_wk.mean_CO2%>% ggplot(aes(x=WK, y= CO2,col=Site ))+
  geom_line()
