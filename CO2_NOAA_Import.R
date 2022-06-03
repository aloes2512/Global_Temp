url_global<-"https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwjokvXeoKL3AhXk8LsIHfE5DTwQFnoECDMQAQ&url=https%3A%2F%2Fgml.noaa.gov%2Fccgg%2Ftrends%2F&usg=AOvVaw3H2TToZQAzOivodOwlQXDs"
# mauna loa data published by noaa: Pieter Tans (303 497 6678; pieter.tans@noaa.gov)
# file created:Thu May  5 05:00:13 2022
url_data<-"https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_weekly_mlo.csv"
library(readr)
library(readxl)
library(tidyverse)
library(lubridate)
library(rvest)
browseURL(url_global)
CO2_ts<-  read_delim("~/Desktop/Klima_Energiewende/Daten/CO2_Zeitreihe_NOAA .csv",
delim = ";", escape_double = FALSE, col_types = cols(molfrac = col_skip(),
`(-999.99=` = col_skip(), no = col_skip()),
trim_ws = TRUE)%>% subset(CO2>0)
CO2_ts<-unite(CO2_ts,datetime,yr,month,day,sep = "-")%>%
  dplyr::select(-`data)`)%>% mutate(datetime=ymd(datetime))
summary(CO2_ts)
head(CO2_ts)
tail(CO2_ts)
CO2_ts<-CO2_ts%>% 
  mutate(yr=floor_date(datetime,unit = "years"))%>% dplyr::select(-dec.yr)
#plot NOAA data()
Mauna_loa_plt<-CO2_ts%>% ggplot(aes(x=datetime,y=CO2))+
  geom_line()+
  ggtitle("CO2-concentration  @ MAUNA LOA",
          subtitle = " published P.Tans:: pieter.tans@noaa.gov")+
  labs(x="",y="CO2[ppm]")
require(lubridate)
#plot average per yr
CO2_yr.mean<-CO2_ts%>% group_by(yr)%>%
  summarise(CO2=mean(CO2,na.rm=T))

CO2_yr.mean%>% ggplot(aes(x=yr,y=CO2))+
  geom_line(col="red")+
  geom_line(data=CO2_ts,aes(x=datetime,y=CO2))+
  ggtitle("CO2-concentration  @ MAUNA LOA",
          subtitle = " published P.Tans:: pieter.tans@noaa.gov")+
  labs(x="",y="CO2[ppm]")

summary(CO2_ts)
# plot weekly means
CO2_ts%>% mutate(wk= floor_date(datetime, unit ="week"))
head(CO2_ts,10)
CO2_ts%>% ggplot(aes(x=week.date,y=CO2))+
  geom_line()
#CO2 statista
CO2.yr<-read_xls("~/desktop/Klima_Energiewende/Daten/CO2_Jahreswerte_statista.xls",col_names = F)
colnames(CO2.yr)<- c("yr","CO2")
CO2.yr<-CO2.yr%>% mutate(date= ymd(paste0(yr,"-01-01")))
#CO2.yr<-CO2.yr%>% mutate(yr=as_factor(yr))
summary(CO2.yr)
CO2.yr%>% left_join(CO2_yr.mean)
Co2.yr_plt<-CO2.yr%>% ggplot(aes(x=date,y=CO2))+
  geom_line()
Co2.yr_plt+  geom_line(aes(x=week.date,y=CO2),data=CO2_ts,col="red")+
  ggtitle("CO2-Concentrations @ Mauna Loa, Hawai",
          subtitle = "Data Source:NOAA/Statista")+
  labs(y= "ppm")
# import brasilian data from 
 url_data<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_abp_surface-flask_1_ccgg_event.txt"
session<- session(url_data)
browseURL("https://gml.noaa.gov/dv/data/")
browseURL(url_data)
# Lampedusa
require(readxl)
require(lubridate)
LMP_CO2_dat<-read_excel("~/downloads/LMP_CO2_samples.xlsx",col_names = T)%>%
  dplyr::select(c(2:6,12))
summary(LMP_CO2_dat)
LMP_CO2_dat<-LMP_CO2_dat%>% mutate(date= paste0(sample_year,"-",sample_month,"-",sample_day," ",sample_hour,":",sample_minute),datetime=ymd_hm(date))%>%
                        group_by(datetime)%>%summarise(CO2= mean(analysis_value,na.rm =T)/1000)
#LMP_CO2_dat$date%>% format("%Y-%m-%d %H:%M")
head(LMP_CO2_dat)
CO2_plt<-LMP_CO2_dat%>%ggplot(aes(x=datetime,y = CO2))+
  geom_point(size= 0.5, shape= 2)
CO2_plt+ ggtitle(" CO2-Immission Lampedusa ",
                 subtitle = " N35.5°,E12.6,alt = 50m")+
  labs(x="",y = "CO2 [ppm]")
LMP_CO2_dat$site<-"LMP"
LMP_CO2_dat$lat<- 35.5
LMP_CO2_dat$lon<-12.6
LMP_CO2_dat$alt<-50
CO2_data.lst$LMP<-LMP_CO2_dat
saveRDS(CO2_data.lst,file="~/projects/Global_Temp/data/NOAA_data.rds")
#=============
ABP_CO2_dat<-read_excel("~/downloads/ABP_CO2_samples.xlsx",col_names = T)%>%
  dplyr::select(c(2:6,9))
ABP_CO2_dat<-ABP_CO2_dat%>% mutate(date= paste0(sample_year,"-",month,"-",day),date=ymd(date))%>%
  group_by(date)%>%summarise(CO2= mean(analysis_value,na.rm =T)/1000)
ABP_CO2_dat%>% ggplot(aes(x=date,y= CO2))+
  geom_point(size=0.5)
CO2_plt+ geom_point(data = ABP_CO2_dat,aes(x=date,y=CO2),size = 0.5,col ="red")
# Hohenpeissenberg Germany
HPB_CO2_samples<-ABP_CO2_dat<-read_excel("~/downloads/ABP_CO2_samples.xlsx",col_names = T)%>%
dplyr::select(c(2:6,9))
HPB_CO2_samples <-HPB_CO2_samples%>% mutate(date= paste0(sample_year,"-",month,"-",day),date=ymd(date))%>%
  group_by(date)%>%summarise(CO2= mean(analysis_value,na.rm =T)/1000)
HPB_CO2_samples%>%ggplot(aes(x=date,y= CO2))+
  geom_point(size=0.5,col ="blue")+
  ggtitle("CO2_Immission @ Hohenpeissenberg",
          subtitle="N 47.8°, E 11.0°")+
  labs(x="", y="CO2[ppm]")
CO2_plt+geom_point(data=HPB_CO2_samples,aes(x=date,y= CO2),size=0.5,col= "blue")+
  ggtitle("CO2-Immission ",
         subtitle = "Hohenpeissenberg,Lampedusa" )+
  labs(y="CO2[ppm]")
#=============================
imprt_data<-function(lnk){
  path<- paste0("~/Downloads/",lnk,"_CO2_samples.xlsx")
  dtfr<-read_excel(path,col_names = TRUE)%>%
    dplyr::select(c(2:6,analysis_value))
  dtfr<-dtfr%>% 
    mutate(date= paste0(sample_year,"-",sample_month,"-",sample_day),date=ymd(date))%>%
    group_by(date)%>%summarise(CO2=mean(analysis_value,na.rm = TRUE)/1000)
}
ABP_CO2_samples <- read_excel("~/Downloads/ABP_CO2_samples.xlsx")

HPB_CO2_samples<-imprt_data("HPB")# Hohenpleissenberg
ABP_CO2_samples<- imprt_data("APB")#Brasilen'
LMP_CO2_samples<-imprt_data("LMP")# Lampedusa
ACR_CO2_samples<-imprt_data("ACR")# Terceira (Azoren)N38.8°W 11.0°
# ALT N82,4508	W -62,507
ALT_CO2_samples<-imprt_data("ALT")%>% subset(CO2>200)
summary(ALT_CO2_samples)
ALT_CO2_samples%>% ggplot(aes(x=date,y=CO2))+
  geom_point(size = 0.5,shape=4)
summary(ACR_CO2_samples)
# Terzeira Island (Azoren) N 38.722222°, W -27.206944
ACR_CO2_samples%>% subset(CO2>200)%>%ggplot(aes(x=date,y=CO2))+
  geom_point(size = 0.5)
#Baltic sea
BAL_CO2_samples<-imprt_data("BAL")%>% subset(CO2>100)
summary(BAL_CO2_samples)
BAL_CO2_samples%>% ggplot(aes(x=date,y=CO2))+
  geom_point(size = 0.5, shape=5)
# Mexico highland
MEX_CO2_samples<-imprt_data("MEX")
summary(MEX_CO2_samples)
MEX_CO2_samples%>% ggplot(aes(x=date,y=CO2))+
  geom_point(size =0.5,shape=0)
#Amsterdam Island, France
AMS_CO2_samples<-imprt_data("AMS")%>% subset(CO2>250&CO2<900)
summary(AMS_CO2_samples)
AMS_CO2_samples%>% ggplot(aes(x=date,y=CO2))+
  geom_point(size = 0.5, shape=5,alpha= 0.5)+
  ggtitle("CO2-Immission @ Amsterdam Island,Fr",
          subtitle = "Indian Ocean @ lat = -37,95°	 long=77,53°")+
  labs(x= "",y= "CO2 ")+
  coord_cartesian(ylim = c(330,360))
plt_CO2<- function(ts,site,lat,lon){
  ts%>% ggplot(aes(x=date,y=CO2))+
    geom_point(size = 0.5, shape=5,alpha= 0.5)+
    ggtitle(paste("CO2-Immission @",site),
            subtitle = paste("latitude=",lat,"longitude=",lon) )+
    labs(x= "",y= "CO2 [ppm]")
}
plt_CO2(AMS_CO2_samples,"AMS",lat= "-37,95°",lon= "77,53°")
# Anmyeondo AMY Korea lat=36,539	long=126,330
require(tidyverse)

AMY_CO2_samples<-imprt_data("AMY")%>% subset(CO2>250&CO2<470)
summary(AMY_CO2_samples)
plt_CO2(AMY_CO2_samples,"AMY",lat= "36,539°",lon= "126,33°")
# UBA Schauinsland 1h Daten N 47.911944, E 7.898611 alt =1284m
UB2019SCO2_SiL<- read_delim("~/Desktop/Klima_Energiewende/CO2_Schauinsland/UB2019SCO2_inv1SMW_20220414.csv",
delim = ";",quote = "'", escape_double = FALSE, trim_ws = TRUE)%>% subset(Wert > 0)
summary(UB2019SCO2_SiL)
head(UB2019SCO2_SiL,2)
UB2019SCO2_SiL<- UB2019SCO2_SiL%>% mutate(datetime= paste(Datum,Uhrzeit),
   datetime = ymd_hms(datetime),datetime=floor_date(datetime,unit = "hour"))
UB2019SCO2_SiL%>%ggplot(aes(x=datetime,y=Wert))+
  geom_point(size = 0.2)
# UB2017SCO2_inv1SMW_20220414
require(lubridate)
UB2017SCO2_SiL<- read_delim("~/Desktop/Klima_Energiewende/CO2_Schauinsland/UB2017SCO2_inv1SMW_20220414.csv",
                            delim = ";",quote = "'", escape_double = FALSE, trim_ws = TRUE)%>% subset(Wert > 0)
summary(UB2017SCO2_SiL)
UB2017SCO2_SiL<- UB2017SCO2_SiL%>% mutate(datetime= paste(Datum,Uhrzeit),
                                          datetime = ymd_hms(datetime),datetime=floor_date(datetime,unit = "hour"))

UB2017SCO2_SiL%>%ggplot(aes(x=datetime,y=Wert))+
  geom_point(size = 0.2)
CO2_SiL <-bind_rows(UB2017SCO2_SiL,UB2019SCO2_SiL)
CO2_SiL%>% ggplot(aes(x=datetime,y=Wert))+
  geom_point(size= 0.1,alpha = 0.1)
#UB2018SCO2_inv1SMW_20220414
UB2018SCO2_SiL<- read_delim("~/Desktop/Klima_Energiewende/CO2_Schauinsland/UB2018SCO2_inv1SMW_20220414.csv",
                            delim = ";",quote = "'", escape_double = FALSE, trim_ws = TRUE)%>% subset(Wert > 0)
summary(UB2018SCO2_SiL)
UB2018SCO2_SiL<- UB2018SCO2_SiL%>% mutate(datetime= paste(Datum,Uhrzeit),
                                          datetime = ymd_hms(datetime),datetime=floor_date(datetime,unit = "hour"))
UB2018SCO2_SiL%>% ggplot(aes(x=datetime,y=Wert))+
  geom_point(size= 0.1,alpha = 0.1)
CO2_SiL<-CO2_SiL%>% bind_rows(UB2018SCO2_SiL)
#UB2010SCO2_inv1SMW_20220414.csv
UB2010SCO2_SiL<- read_delim("~/Desktop/Klima_Energiewende/CO2_Schauinsland/UB2010SCO2_inv1SMW_20220414.csv",
                            delim = ";",quote = "'", escape_double = FALSE, trim_ws = TRUE)%>% subset(Wert > 0)
summary(UB2010SCO2_SiL)
UB2010SCO2_SiL<- UB2010SCO2_SiL%>% mutate(datetime= paste(Datum,Uhrzeit),
                                          datetime = ymd_hms(datetime),datetime=floor_date(datetime,unit = "hour"))
UB2010SCO2_SiL%>% ggplot(aes(x=datetime,y=Wert))+
  geom_point(size= 0.1,alpha = 0.1)
CO2_SiL<-CO2_SiL%>% bind_rows(UB2010SCO2_SiL)
#UB1986SCO2_inv1SMW_20220414.csv
UB1986SCO2_SiL<- read_delim("~/Desktop/Klima_Energiewende/CO2_Schauinsland/UB1986SCO2_inv1SMW_20220414.csv",
                            delim = ";",quote = "'", escape_double = FALSE, trim_ws = TRUE)%>% subset(Wert > 0)
summary(UB1986SCO2_SiL)
UB1986SCO2_SiL<- UB1986SCO2_SiL%>% mutate(datetime= paste(Datum,Uhrzeit),
                                          datetime = ymd_hms(datetime),datetime=floor_date(datetime,unit = "hour"))
UB1986SCO2_SiL%>% ggplot(aes(x=datetime,y=Wert))+
  geom_point(size= 0.1,alpha = 0.1)
CO2_SiL<-CO2_SiL%>% bind_rows(UB1986SCO2_SiL)

CO2_Sil_plt<-CO2_SiL%>% ggplot(aes(x=datetime,y=Wert))+
  geom_point(size= 0.1,alpha = 0.1)+
  ggtitle("CO2-Immission @ Schauinsland",
          subtitle = "lat=N 47.911944;lon=E 7.898611, alt = 1284m")+
  labs(x="",y= "CO2 [ppm]")
CO2_Sil_plt+  geom_smooth(col="red")
#N 47.911944, E 7.898611
CO2_SiL%>%summary()
CO2_Sil<-CO2_SiL%>% mutate(Hr=as.numeric(format(datetime,"%H")))
CO2_h_mtl<-CO2_Sil%>% group_by(Hr)%>%
  summarise(CO2_h.mittel=mean(Wert,na.rm=T)) 
CO2_h_mtl%>%as_tibble%>%ggplot(aes(x=Hr,y= CO2_h_mtl))+
  geom_point()
CO2_h<- tibble (Hr=CO2_h_mtl$Hr,
                CO2=  as.numeric(CO2_h_mtl$CO2_h.mittel))
CO2_h%>% ggplot(aes(x=Hr,y=CO2))+
  geom_point()+
  geom_smooth(col="red")+
  ggtitle("Schauinsland Mean CO2-Immission", 
     subtitle = "per day hour 2017-2019")+
  labs(x = "[00:23h]", y= "CO2 [ppm]")
# CO2 monthly means
CO2_mnth<-CO2_SiL %>% dplyr::select(datetime,CO2=Wert)%>% 
  mutate(Mnth= as.numeric(format(datetime,"%m")))%>% 
  group_by(Mnth)%>% summarise(CO2.mnth= mean(CO2))
CO2_mnth%>% as_tibble%>% ggplot(aes(x=Mnth,y=CO2_mnth))+
  geom_point()
CO2_m <- tibble(mnth=CO2_mnth$Mnth,
                CO2.m=CO2_mnth$CO2.mnth)
CO2_m%>% ggplot(aes(x=mnth,y=CO2.m))+
  geom_point()
# update Schauinsland data
CO2_data.lst$DEUB004<-CO2_data.lst$SiLand
save(CO2_data.lst,file.path(dat_path,"NOAA_data.rds"))
CO2_data.lst<-readRDS(file.path("data","NOAA_data.rds"))
#SPO site data corrrect
SPO_alt<-2810
SPO_lat<--89.98
SPO_lon<--24.8
#Latitude: 89.98° South
#Longitude: 24.8° West
CO2_data.lst$SPO%>%head(2)
CO2_data.lst$SPO<-CO2_data.lst$SPO%>% mutate(lat= -89.98,lon=-24.8)
saveRDS(CO2_data.lst,file = file.path("data","NOAA_data.rds"))

