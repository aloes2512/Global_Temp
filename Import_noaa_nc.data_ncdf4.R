library(rvest)
library(magrittr)
library(tidyverse)
require(ncdf4)
#https://pjbartlein.github.io/REarthSysSci/netCDF.html#reading-a-netcdf-data-set-using-the-ncdf4-package
browseURL("https://pjbartlein.github.io/REarthSysSci/netCDF.html#reading-a-netcdf-data-set-using-the-ncdf4-package")
# set path and filename of noaa nc data
url_co2_path<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc/"
browseURL(url_co2_path)
noaa_page <- read_html(url_co2_path)
# Lösung: Suche Tabelle wähle Spalte "Name" und "files mit Endung .nc
noaa_nc_files<- noaa_page %>% html_table() %>% {.[[1]]} %>% # selects first element of list: "html_table(noaa_page)"
  {.[["Name"]]} %>% # selects elements of a list by name
  stringr::str_subset("\\.nc$")
noaa_nc_names <-noaa_nc_files%>% str_extract("^.{7}")%>%str_extract(".{3}$")
names(noaa_nc_files)<-noaa_nc_names
#open(url_list.files)
noaa_nc_files.url<-file.path(url_co2_path,noaa_nc_files)
names(noaa_nc_files.url)<-noaa_nc_names
#noaa_files.url%>% map(download.file,dest.file= ...?)
nc_dest<- vector("list",length = length(noaa_nc_names))
nc_dest[2]
browseURL(url_co2_path)
nc_dest<-file.path("/data",noaa_nc_files)
names(nc_dest)<-noaa_nc_names
nm<-"amt"
noaa_nc_data<-vector("list",length = length(noaa_nc_names))
names(noaa_nc_data)<-noaa_nc_names
for (nm in noaa_nc_names){
   download.file(noaa_files.url[nm],destfile = noaa_nc_data[nm])
}
# now nc_dest may be opened with nc_open
#=====================
require(ncdf4)
noaa_nc_dat<- vector("list", length =  length(noaa_nc_names))
names(noaa_nc_dat)<-noaa_nc_names
for(nm in noaa_nc_names){
  noaa_nc_dat[[nm]]<- nc_open(nc_dest[nm])
}
nm
nc_dest[nm]
summary(noaa_nc_dat)


#ncpath.downlds <- "~/downloads/co2_amt_tower-insitu_1_ccgg_HourlyData.nc"
#raw data as stored in project file
ncpath<-"~/projects/Global_Temp/data/"
file.path(ncpath,"noaa_nc_files")
nc_files<-list.files("~/downloads/",pattern =".nc")
require(tidyverse)
nc_names<-nc_files%>%str_extract("^.{7}")%>%str_extract(".{3}$")
names(nc_files)<-nc_names
#=====================
require(ncdf4)
# 1 file example amt site
nc_dat<- vector("list", length =  length(nc_names))
names(nc_dat)<-nc_names
nc_dat[[1]] <- nc_open(paste0("~/downloads/",nc_files[1]))
for(nm in nc_names){
  nc_dat[[nm]]<- nc_open(paste0("~/downloads/",nc_files[nm]))
}
summary(nc_dat[[1]])
nc_dat[[1]]$filename# "~/downloads/co2_amt_tower-insitu_1_ccgg_HourlyData.nc"
# all nc sites listed
nc_data<- vector("list", length =  length(nc_names))
names(nc_data)<-nc_names
for (nm in nc_names){
  nc_dt<- nc_open(paste0("~/downloads/",nc_files[nm]))
  nc_data[[nm]]<- tibble(tnm = nc_dt%>%ncvar_get("time"),
                     CO2=nc_dt%>%ncvar_get("value"))
  
  }
summary(nc_data)

# convert list to one dfr
nc_data_dfr<-nc_data%>% map_dfr(.f=rbind,.id= "name")
head(nc_data_dfr)
dim(nc_data_dfr)#2842883 3
nc_data_dfr$name%>%unique()# 10 sites
df<-nc_data_dfr%>% mutate(name=as_factor(name))%>%
  mutate(datetime=as.POSIXct(tnm,origin="1970-01-01",tz="UTC"))%>%
  group_by(name)%>% nest()
df[[1]]==nc_names
df[[1]]# amt bao crv lef mbo sct snp wbi wgc wkt
df[[2]][[10]]%>% group_by(datetime)%>% summarise(CO2=mean(CO2,na.rm=T))
nc_sites_CO2<- list()
nc_sites_CO2<-df[[2]]%>%map(function(x){group_by(.data=x,tnm)%>%summarise(CO2=mean(CO2,na.rm=T),datetime=first(datetime))})

names(nc_sites_CO2)<-nc_names
summary(nc_sites_CO2)
nc_CO2_tibble<-nc_sites_CO2%>% map_df(bind_rows, .id= "name")
summary(nc_CO2_tibble)
#plot all data with nc formatted CO2 obs
nc_CO2_tibble%>% ggplot(aes(x=datetime,y= CO2,col =name))+
  geom_point(size= 0.2,aes(x=datetime,y= CO2,col =name))+labs(x="",y="CO2[ppm]")+
  ggtitle("CO2-Immission at 10 sites:",
          subtitle = paste(nc_names,collapse = " "))

# select some sites c("amt","bao","crv")
nc_CO2_tibble %>% subset(name %in% c("amt","bao","crv")) %>%
  ggplot(aes(x=datetime,y= CO2,col =name))+
  geom_point(size= 0.2,aes(x=datetime,y= CO2,col =name))+
  labs(x="",y="CO2[ppm]")+
  ggtitle("CO2-Immission")
nc_CO2_tibble %>% subset(name %in% c("lef","mbo","sct")) %>%
  ggplot(aes(x=datetime,y= CO2,col =name))+
  geom_point(size= 0.2,aes(x=datetime,y= CO2,col =name))+
  labs(x="",y="CO2[ppm]")+
  ggtitle("CO2-Immission")
nc_CO2_tibble %>% subset(name %in% c("snp","wbi","wgc")) %>%
  ggplot(aes(x=datetime,y= CO2,col =name))+
  geom_point(size= 0.2,aes(x=datetime,y= CO2,col =name))+
  labs(x="",y="CO2[ppm]")+
  ggtitle("CO2-Immission")
nc_CO2_tibble %>% subset(name %in% c("wkt","wbi","wgc")) %>%
  ggplot(aes(x=datetime,y= CO2,col =name))+
  geom_point(size= 0.2,aes(x=datetime,y= CO2,col =name))+
  labs(x="",y="CO2[ppm]")+
  ggtitle("CO2-Immission")
#=================================================
# extract variable values at 1 site

nc_dat[[1]]$filename#"~/downloads/co2_wkt_tower-insitu_1_ccgg_HourlyData.nc"
nc_amt[[1]]$var%>%summary()
require(ncdf4.helpers)
nc.get.variable.list(nc_amt[[1]],min.dim=1)# variables of nc_amt file
Lat<-nc_amt[[1]]%>%ncvar_get("latitude")%>% first()%>%round(digits=2)#first of  323419 values
Lon<-nc_amt[[1]]%>%ncvar_get("longitude")%>% first()%>%round(digits=2)
obs<-nc_amt[[1]]%>%ncvar_get("time")%>% length()
nc_amt[[1]]%>%ncvar_get("value")%>% length()#323419
nc_amt[[1]]$var$time$units#"seconds since 1970-01-01T00:00:00Z"
tm<- ncvar_get(nc_amt[[1]],"time")%>% as.POSIXct(origin="1970-01-01")
length(tm)#323419

require(lubridate)
# visualize first file: amt_tower_hourly.nc
CO2_amt.tibble<- nc_CO2_tibble%>%subset(name=="amt")

amt_plt<-CO2_amt.tibble%>%ggplot(aes(x=datetime,y=CO2))+
  geom_smooth(col ="red")+
  geom_point(size= 0.3, shape= 4,alpha=0.08)+
  labs(x="",y= "CO2 [ppm]")
amt_plt+ggtitle(paste("CO2","amt"),
               subtitle = paste0("Latitude = ",Lat,"° ;",
                                " Longitude = ",Lon,"° observations =", obs))
