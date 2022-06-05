# Evaluate CO2 time series
library(tidyverse)
library(mgcv)
library(broom)
station_sites<-readRDS("~/projects/Global_Temp/data/station_sites.rds")
names(station_sites)<- c("Code","Name","Country","Latitude","Longitude","Elevation","GMTime","Project") 
station_sites<-station_sites%>% mutate(Code=as.factor(Code),Name=as.factor(Name),Country=as.factor(Country))
summary(station_sites)
summary(station_sites$Country)
station_sites%>% subset(Country=="United Kingdom")# TAC lat 52.5 lon 1.14
station_sites%>% subset(Country=="Ireland")# MHD lat 53.3 lon -9.90

CO2_data.lst<- readRDS("~/projects/Global_Temp/data/NOAA_data.rds")
summary(CO2_data.lst)
CO2_data.lst$MHD # site in ireland
CO2_data.lst$TAC # site located east england
unique(station_sites$Code)
head(station_sites)
NROW(station_sites)
station_sites[216,5]<-7.898#,1284,"+1")
station_sites[216,6]<-1284
station_sites[216,7]<-"+1"
station_sites[216,]
station_sites[217,]<-c("DEUB044","Zugspitze","Germany",19.53,10.99,2656,NA,NA)
zugs<-tibble(Code="DEUB044",
             Name="Zugspitze",
             Country= "Germany",
             Latitude= 47.42,
             Longitude= 10.985,
             Elevation= 2656,
             GMTime= "+1",
             Project= NA)
station_sites<-station_sites%>% bind_rows(zugs)
saveRDS(station_sites,file = "~/projects/Global_Temp/data/station_sites.rds")
saveRDS(CO2_data.lst,"~/projects/Global_Temp/data/NOAA_data.rds")
site_nms<- names(CO2_data.lst)# 20 stations

# linear regression model
CO2_regr_mdl<-CO2_data.lst%>%map(~lm(.$CO2~.$datetime,data = .))
names(CO2_regr_mdl)
summary(CO2_regr_mdl)
CO2_regr<-CO2_regr_mdl%>%map_df(~.$coefficients)
CO2_regr$site<-site_nms
CO2_regr<-CO2_regr%>% rename("intercpt"=`(Intercept)`,"slope"=`.$datetime`)
CO2_regr<-CO2_regr%>%arrange(slope)%>% mutate(rate_yr=slope*365.25*24*3600)
# Lampedusa (LMP) was calculated from daily means
CO2_regr%>% subset(site=="LMP")%>% mutate(rate_yr=rate_yr/365.25^2,slope=rate_yr/(365.25*24*3600)) 
CO2_regr[19,2]<-0.0000000467#,rate_yr=1.47)
CO2_regr[19,4]<-1.47
# arrange by rate_yr
CO2_regr<-CO2_regr%>% arrange(rate_yr)
# Mauna Loa
loc_pms <- station_sites%>% filter(Code == "MLO")
CO2_data.lst$MLO%>% ggplot(aes(x=datetime, y= CO2))+
  ggtitle(paste("CO2-Immisssion Trend",loc_pms[2]),
          subtitle = paste("Latitude:",loc_pms[4],"Longitude:",loc_pms[5]))+
  geom_point(size =0.1,alpha = 0.1)+
  geom_abline(intercept = 318. ,slope = 0.0000000566,col ="red")
# calculate residuals from linear regression
CO2_regr_mdl$MLO %>% fitted()
require(broom)
CO2_regr_mdl%>% map(augment)
