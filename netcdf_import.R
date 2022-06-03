# modelling datasets
#importing html data
require(tidyverse)
require(rvest)
url<-"https://en.wikipedia.org/wiki/Category:Murder_in_the_United_States"
h <- read_html(url)

 tab<-h%>%html_node("table")
   class(h)
browseURL("https://pcmdi.llnl.gov/CMIP6/Guide/dataUsers.html")
#netcdf
browseURL("https://cran.r-project.org/web/packages/futureheatwaves/vignettes/starting_from_netcdf.html")
browseURL("https://pcmdi.llnl.gov")
library(ncdf4)
library(ncdf4.helpers)
library(PCICt)
library(tidyverse)
Url <-"https://esgf-node.llnl.gov/snc_day_CanESM2_historicalGHG_r1i1p1_18500101-20121231.nc"
browseURL(Url)# Öffnet die Website
# ARCT data https://docs.google.com/spreadsheets/d/1AAYqDZKVfv-RKekPw328bWJInEGcFBrpJdCQiPQp7N0/edit#gid=0
CRV_CO2_data<-nc_open("~/downloads/co2_crv_tower-insitu_1_ccgg_HourlyData.nc")
# metadata CRV_CO2_data
CRV_CO2_data
CRV_CO2_data# metadata
summary(CRV_CO2_data)
#site_code year month day hour minute second time_decimal value value_std_dev value_unc nvalue latitude longitude altitude elevation intake_height instrument inst_repeatability qcflag
names(CRV_CO2_data$var)
CRV_CO2_data$dim$obs$len#121649
CO2_value<-ncvar_get(CRV_CO2_data, "value" )
time<-ncvar_get(CRV_CO2_data,"time")##sample_time_in_seconds_since_january_1_1970
utc_time<- as.POSIXct(time,origin="1970-01-01",tz="UTC") 
CRV_dat<-tibble(utc_time,
            CO2=ncvar_get(CRV_CO2_data,"value"))%>%
  group_by(utc_time)%>% summarise(CO2=mean(CO2,na.rm=TRUE))

lat<-ncvar_get(CRV_CO2_data,"latitude")#64.983
lon<-ncvar_get(CRV_CO2_data,"longitude")#-147.598
alt<-ncvar_get(CRV_CO2_data,"altitude")%>% range()#[1] 616.33 643.13
CRV_dat$latitude<-first(lat)
CRV_dat$longitude<-first(lon)
CRV_dat$altitude<- first(alt)
CRV_plt<-CRV_dat%>%ggplot(aes(x=utc_time,y=CO2))+
  ggtitle("CO2-Immissions Arctic-Experiment",
          subtitle = paste("latitude",round(lat,digits = 2),"longitude",round(lon,digits=2),"altitude",round(alt),"m"))
plt1<-CRV_plt+  geom_point(size=0.1, alpha=0.2)
CRV_plt+geom_smooth(method = "lm",col = "red")
plt1+geom_smooth(method = "lm",col = "red")
summary(CRV_dat)
head(CRV_dat)
CO2_data.lst<-readRDS()
NOAA_CO2<-readRDS("~/projects/Global_Temp/data/NOAA_data.rds")
summary(NOAA_CO2)
NOAA_CO2$CRV<-CRV_dat
saveRDS(NOAA_CO2,file = "~/projects/Global_Temp/data/NOAA_data.rds")
CRV_dat%>% head()
