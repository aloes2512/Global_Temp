
ncdf4-package {ncdf4}
#R for Earth System Science
browseURL("https://pjbartlein.github.io/REarthSysSci/netCDF.html#convert-the-time-variable")
# vignette on package ncdf4 
ncdf4_vignette<-"http://dwpierce.com/software"
browseURL(ncdf4_vignette)
# data source: NOAA ESRL global monitoring laboratory 
ESL_sites<-"https://gml.noaa.gov/dv/site/"
browseURL(ESL_sites)
link<-"https://gml.noaa.gov/aftp/data/trace_gases/ch4/in-situ/tower/nc/ch4_crv_tower-insitu_1_ccgg_HourlyData.nc"
CO2_link<- "https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc/co2_crv_tower-insitu_1_ccgg_HourlyData.nc"
browseURL(CO2_link)# loads data to download
download.file(CO2_link,"co2_crv_tower-insitu_1_ccgg_HourlyData.nc")# file is @ ~/projects/Global_Temp/
ch4_crv <-nc_open("ch4_crv_tower-insitu_1_ccgg_HourlyData.nc")
co2_crv <-nc_open("co2_crv_tower-insitu_1_ccgg_HourlyData.nc")
time<-ncvar_get(co2_crv,"time")
utc_time_co2<- as.POSIXct(time,origin="1970-01-01",tz="UTC")
co2_data<-ncvar_get(co2_crv,"value")

names(co2_crv$var)
crv_lat<-ncvar_get(co2_crv,"latitude")
crv_lon<-ncvar_get(co2_crv,"longitude")
crv_alt<- ncvar_get(co2_crv,"altitude")

CO2_crv<- tibble(site= "CRV",
                datetime=utc_time_co2,
                 CO2=co2_data,
                 lat= crv_lat,
                 lon=crv_lon,
                 alt=crv_alt)
CO2_crv%>%ggplot(aes(x=datetime,y=CO2))+geom_point(size=0.1)+
  geom_smooth(col = "red")+
# select data set  and download
# file in downloads directory named: "ch4_crv_tower-insitu_1_ccgg_HourlyData.nc"
#alternativ
require(rvest)
session<-html_session(link)
session
require(rvest)
s<- html_session(ESL_sites)
page <- s %>%       #html document
  read_html()
tabl<- page%>%html_table()
summary(tabl)
station_sites<-tabl[[1]]
require(ncdf4)
f1<-nc_open('~/downloads/co2_amt_tower-insitu_1_ccgg_HourlyData.nc')
f1 #dataset_start_date: 2003-09-19T00:00:00Z
  #dataset_stop_date: 2019-07-24T23:00:00Z
  #dataset frequency = 1 hour
names(f1$var)
time<-ncvar_get(f1,"time")
utc_time<- as.POSIXct(time,origin="1970-01-01",tz="UTC")
head(utc_time)# two measurments per hour
dat<-tibble(utc_time,
            time,
            CO2=ncvar_get(f1,"value"))
summary(dat)
# use only mean value per hour
dat<-dat%>% group_by(utc_time)%>% summarise(CO2=mean(CO2))
NROW(dat)#123354
dat%>%ggplot(aes(x=utc_time,y=CO2))+
  geom_point(size= 0.2,alpha= 0.2)+
  ggtitle("CO2-Immission @ Argyle, Maine",
  subtitle = "site_latitude: 45.0345,site_longitude: -68.6821")+
  labs(y= "CO2 [ppm]")
# second example air temperatures
file<- "ta_mon_AIRS-1-0_BE_gn_200209-201105.nc"
path ="~/downloads"  
path2<- file.path(path,file)
f2<-nc_open(path2)
names(f2$var)
time_bnd <-ncvar_get(f2,"time_bnds")# defines time intervall
time<-ncvar_get(f2,"time")# dim time = day
tim_lim<- vector(length = length(time)/2)
tim_lim<- time_bnd[2,]%>% as.Date(origin="2000-01-01")
time.posix<-as.Date(time,origin= "2000-01-01")
range(time.posix)#[1] "2002-09-01" "2011-06-01"
f2$dim$time$name# "time"
f2$dim$time$len#105
length(time)#210
t_atm<- ncvar_get(f2,"ta")
length(t_atm)#115668000
summary(t_atm)
f2$dim$time$units# days since 2000-01-01
f2$dim$plev$units# "Pa"
f2$var$ta$size# [1] 360 180  17 105
f2%>%summary()
f2$format
netcdf_url<-"https://pjbartlein.github.io/REarthSysSci/netCDF.html"
browseURL(netcdf_url)
tustr <- f2$dim$time$units%>%strsplit( " ")
Orig<-tustr[[1]][3]
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])%>%as.character()
tday <- as.integer(unlist(tdstr)[3])%>%as.numeric()
tyear <- as.integer(unlist(tdstr)[1])%>%as.numeric()
tm<-time%>% as_tibble()
tm<-tm%>% mutate(date=as.integer(value),date=as.Date(date, origin= Orig))
lon<-ncvar_get(f2,"lon")
nlon<- dim(lon)#360
lat <- ncvar_get(f2,"lat")
nlat <- dim(lat)
head(lat)
print(c(nlon,nlat))
tunits <- ncatt_get(f2,"time","units")
nt <- dim(time)
nt
# get temperature
names(f2$var)
require(ncdf4.helpers)
tmp_array <- names(f2$var)

dlname <- ncatt_get(f2,"ta","long_name")
dunits <- ncatt_get(f2,"ta","units")
fillvalue <- ncatt_get(f2,"ta","_FillValue")
ls()# current workspace
