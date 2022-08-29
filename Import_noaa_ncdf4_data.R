#"The Global Monitoring Laboratory (GML) of the National Oceanic and Atmospheric Administration
#conducts research that addresses three major challenges: 
##1.greenhouse gas and 
##2.carbon cycle feedbacks, 
##3.changes in clouds, aerosols, and surface radiation, and 
##4.recovery of stratospheric ozone."
browseURL("https://gml.noaa.gov")
browseURL("https://pjbartlein.github.io/REarthSysSci/overview.html")
# various data formats for earth system science
browseURL("https://pjbartlein.github.io/REarthSysSci/ESSdata.html")
# vignette on package ncdf4 
browseURL("https://pjbartlein.github.io/REarthSysSci/netCDF.html#convert-the-time-variable")
#nc special format for gridded earth science data
#ncdf4-package {ncdf4}
install.packages("ncdf4")
library(ncdf4)
#R for Earth System Science
# ESRL (Earth System Research Laboratories )
# data source: NOAA ESRL global monitoring laboratory 
ESL_sites<-"https://gml.noaa.gov/dv/site/"
browseURL(ESL_sites)
ESL_page<-read_html(ESL_sites)
all_sites<-ESL_page%>%html_table() %>% {.[[1]]}%>% dplyr::select(-c(7))
all_sites<-all_sites%>%rename("alt"="Elevation (meters)")

nc_sites<- all_sites%>% subset(Code %in% c("AMT","BAO*","CRV", "LEF", "MBO", "SCT", "SNP", "WBI", "WGC", "WKT"))
solar_rad_sites<-all_sites[all_sites$Project%>%as_vector(.type = "character")%>% str_detect("Solar"), ]

# NOAA Dateien "https://gml.noaa.gov/dv/data/index.php?category=Greenhouse%2BGases&parameter_name=Carbon%2BDioxide&type=Insitu"
url_noaa<-("https://gml.noaa.gov/dv/data/index.php?category=Greenhouse%2BGases&parameter_name=Carbon%2BDioxide&type=Insitu")
browseURL(url_noaa)# loads files to downloads//co2_tower-insitu_1_ccgg_netCDF
noaa_co2_sites<-paste0("co2_",c("amt","bao","crv", "lef", "mbo", "sct", "snp", "wbi", "wgc", "wkt"))

# =====================================
# Link to data of specific site
url_co2_path<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc"
browseURL(url_co2_path)#lists 10 files amt bao crv lef mbo sct snp wgi wgc wkt 
noaa_page <- read_html(url_co2_path)
noaa_nc_files<- noaa_page %>% html_table() %>% {.[[1]]} %>% # selects first element of list: "html_table(noaa_page)"
  {.[["Name"]]} %>% # selects elements of a list by name
  stringr::str_subset("\\.nc$")
# link zur ersten Datei
co2_amt_link<-file.path(url_co2_path,noaa_nc_files[1])
browseURL(co2_amt_link) # file in download
download.file(co2_amt_link,destfile = file.path("data",noaa_nc_files[1]))
#2. Datei
co2_bao_link<- file.path(url_co2_path,noaa_nc_files[2])
browseURL(co2_bao_link)
download.file(co2_bao_link,destfile = file.path("data",noaa_nc_files[2]))
#3.Datei
co2_crv_link<-file.path(url_co2_path,noaa_nc_files[3])# "https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc/co2_crv_tower-insitu_1_ccgg_HourlyData.nc"
browseURL(co2_crv_link)
download.file(co2_crv_link,destfile = file.path("data",noaa_nc_files[3]))
# 4. Datei
co2_lef_link<- file.path(url_co2_path,noaa_nc_files[4])#"https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc/co2_lef_tower-insitu_1_ccgg_HourlyData.nc"
download.file(co2_lef_link,destfile = file.path("data",noaa_nc_files[4]))
# 5.Datei
co2_mbo_link<-file.path(url_co2_path,noaa_nc_files[5])# "https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc/co2_mbo_surface-insitu_1_ccgg_HourlyData.nc"
browseURL(co2_mbo_link)
download.file(co2_mbo_link,destfile = file.path("data",noaa_nc_files[5]))
# 6. Datei
co2_sct_link<-file.path(url_co2_path,noaa_nc_files[6]) #"https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc/co2_sct_tower-insitu_1_ccgg_HourlyData.nc"
download.file(co2_sct_link,destfile = file.path("data",noaa_nc_files[6]))
# 7.Datei
co2_snp_link<-file.path(url_co2_path,noaa_nc_files[7]) #"https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc/co2_snp_surface-insitu_1_ccgg_HourlyData.nc"
download.file(co2_snp_link,destfile = file.path("data",noaa_nc_files[7]))
# 8. Datei
co2_wbi_link<-file.path(url_co2_path,noaa_nc_files[8]) #"https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc/co2_wbi_tower-insitu_1_ccgg_HourlyData.nc"
download.file(co2_wbi_link,destfile = file.path("data",noaa_nc_files[8]))
# 9. Datei
co2_wgc_link<- file.path(url_co2_path,noaa_nc_files[9])#"https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc/co2_wgc_tower-insitu_1_ccgg_HourlyData.nc"
download.file(co2_wgc_link,destfile = file.path("data",noaa_nc_files[9]))
# 10. Datei
co2_wkt_link<-file.path(url_co2_path,noaa_nc_files[10]) #"https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc/co2_wkt_tower-insitu_1_ccgg_HourlyData.nc"
download.file(co2_wkt_link,destfile = file.path("data",noaa_nc_files[10]))
list.files("~/projects/Global_Temp/data/", pattern = ".nc")# 10 timeseries

close.connection(con = url(co2_crv_link))
#==============================================
list.files("~/projects/Global_Temp/data/", pattern = ".nc")==noaa_nc_files
# more general

nc_list
nc_list%>% map(function(x){browseURL(file.path(url_co2_path,x))})# loads the file to downloads
# reads data into R
str_extract("co2_crv_tower-insitu_1_ccgg_HourlyData.nc","^.{7}")
site_nms<-nc_list%>% map_chr(str_extract,"^.{7}")
co2_crv <-nc_open("Data/co2_crv_tower-insitu_1_ccgg_HourlyData.nc")
CO2_data<-vector("list",length=7)
names(CO2_data)<-site_nms
CO2_data<-nc_list%>%map(function(x)nc_open(file.path("~/downloads",x)))
CO2_data%>%str(2)
time_data<-CO2_data%>%map(ncvar_get,"time")
time_data%>%class()
time<-ncvar_get(co2_crv,"time")
time%>%class()#array
time<-time%>% as.vector()
utc_time_co2<- as.POSIXct(time,origin="1970-01-01",tz="UTC")
utc_time_data<-time_data%>% map(~as.POSIXct(time,origin="1970-01-01",tz="UTC"))
co2_data<-ncvar_get(co2_crv,"value")
CO2_values<-CO2_data%>% map(ncvar_get,"value")
names(co2_crv$var)# list includes variables
crv_lat<-ncvar_get(co2_crv,"latitude")
CO2_lat<-CO2_data%>% map(ncvar_get,"latitude")
crv_lon<-ncvar_get(co2_crv,"longitude")
CO2_lon<-CO2_data%>% map(ncvar_get,"longitude")
crv_alt<- ncvar_get(co2_crv,"altitude")
CO2_alt<-CO2_data%>% map(ncvar_get,"altitude")
CO2_nc_sites<- tibble(site= site_nms,
                datetime=utc_time_data,
                 CO2=CO2_values,
                 lat= CO2_lat,
                 lon=CO2_lon,
                 alt=CO2_alt)
names(CO2_nc_sites)#"site"     "datetime" "CO2"      "lat"      "lon"      "alt" 
site_nms#"co2_amt" "co2_bao" "co2_lef" "co2_sct" "co2_wbi" "co2_wgc" "co2_wkt"
site_data_nms<- paste0(site_nms,"_data")
length(site_data_nms)
CO2_nc_sites[1,2]%>%pull(datetime)%>% length()#1

CO2_nc_sites[1,3]$CO2%>% as_vector()%>% length()
amt_co2_data<-tibble(site=site_nms[1],
  datetime=CO2_nc_sites[1,2]$datetime%>%as_vector(),
  CO2= CO2_nc_sites[1,3]$CO2%>%as_vector(),
  lat= CO2_nc_sites[1,4]$lat[[1]],
  lon=CO2_nc_sites[1,5]$lon[[1]],
  alt=CO2_nc_sites[1,6]$alt[[1]])
amt_co2_data<-amt_co2_data%>%mutate(datetime=as.POSIXct(datetime,origin="1970-01-01"))
CO2_crv%>%ggplot(aes(x=datetime,y=CO2))+geom_point(size=0.1)+
  geom_smooth(col = "red")
# select data set  and download
# file in downloads directory named: "ch4_crv_tower-insitu_1_ccgg_HourlyData.nc"
#alternativ
require(rvest)
session<-html_session(link)
session
require(rvest)
s<- session(ESL_sites)
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
  geom_smooth(col="red",formula = y~s(x, k=64),data=dat,aes(x=utc_time,y=CO2))+
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
# save all noaa data 
saveRDS(CO2_data.lst,file = file.path("data","NOAA_data.rds"))

#download from internet nc file
require(ncdf4)
require(purrr)
require(rvest)
#nc files are located at glm.noaa.gov/.... 
#method described in datasciencebook 6.4 pages 109 ff
url_co2_path<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc/"
open(url(url_co2_path))
browseURL(url_co2_path)# opens the link also
tmp1_filename <- tempfile()
tmp_filename<-tempfile()
download.file("https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc/",destfile = "~/Downloads/tmp_filename")
lst<-read_html("~/downloads/tmp_filename")%>%
  html_table()%>% map(2)
#names of the nc files
nc_files<-lst[[1]][-c(1,2,13,14)]
# load the first nc file
file_path1<-file.path(url_co2_path,nc_files[1])
download.file(file_path1,destfile = "~/Downloads/tmp1_filename")
# follow the steps described by bartlein
ncfile1<-nc_open("~/Downloads/tmp1_filename")
ncfile1 # metadata describing format and contents of ncfile2
names(ncfile1$var)# 21 variables
time<-ncvar_get(ncfile1,"time")
utc_time<- as.POSIXct(time,origin="1970-01-01",tz="UTC")
head(utc_time)# two measurements per hour
require(tidyverse)
nc_sites<-nc_files%>%str_extract("co2_...")%>% str_extract("...$")
dat1<-tibble(utc_time,
            CO2=ncvar_get(ncfile1,"value"))%>%
  mutate(site=nc_sites[1])
head(dat1)
NOAA_CO2_data.nc<- vector("list",length=length(nc_files))
tempor.files<- vector("list",length=length(nc_files))
names(NOAA_CO2_data.nc)<-nc_sites
# download raw files in tempor.files
ibrary(ncdf4)
library(ncdf4.helpers)
library(tidyverse)
"air.2x2.250.mon.anom.comb.nc"
klima_path<-"~/desktop/Klima_Energiewende/Daten"
GIS_Temp_noaa <-nc_open("~/desktop/Klima_Energiewende/Daten/air.2x2.250.mon.anom.comb.nc")
time<-ncvar_get(GIS_Temp_noaa,"time" )
length(time)#1710
GIS_Temp_noaa$dim[[1]]# lon
# find var names
library(ncdf4)
GIS_Temp_noaa <-nc_open("~/desktop/Klima_Energiewende/Daten/air.2x2.250.mon.anom.comb.nc")
var.idNames<-names(GIS_Temp_noaa$var)#air
air_matrix<-ncvar_get(GIS_Temp_noaa,"air" )
nc_close(GIS_Temp_noaa)
air_matrix%>%as_tibble()

GIS_Temp_noaa$var
GIS_Temp_noaa$var$air$longname#"Monthly Average Temperature Anomalies"

dim(air_matrix)
air_matrix[,,1]%>%as_vector()
air_temp<-c(air_matrix[1,,])%>% na.omit()
length(air_temp)#[1] 20707593
#
absolute_temp<-nc_open(file.path(klima_path,"absolute_v5.nc"))
absolute_temp$dim
Temp<-ncvar_get(absolute_temp,"tem")
length(Temp)#[1] 31104
summary(Temp)
Time<-ncvar_get(absolute_temp,"time")# units month
length(Time)
tail(Time)
