library(rvest)
library(magrittr)
library(tidyverse)
require(ncdf4)
#https://pjbartlein.github.io/REarthSysSci/netCDF.html#reading-a-netcdf-data-set-using-the-ncdf4-package
browseURL("https://pjbartlein.github.io/REarthSysSci/netCDF.html#reading-a-netcdf-data-set-using-the-ncdf4-package")
# set path and filename of noaa nc data
url_co2_path<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc"
browseURL(url_co2_path)
open(url(url_co2_path))
noaa_page <- read_html(url_co2_path)
# Lösung: Suche Tabelle wähle Spalte "Name" und "files mit Endung .nc
noaa_nc_files<- noaa_page %>% html_table() %>% {.[[1]]} %>% # selects first element of list: "html_table(noaa_page)"
  {.[["Name"]]} %>% # selects elements of a list by name
  stringr::str_subset("\\.nc$")
noaa_nc_names <-noaa_nc_files%>% str_extract("^.{7}")%>%str_extract(".{3}$")
names(noaa_nc_files)<-noaa_nc_names
#open(url_list.files)
noaa_nc_files.url<-file.path(url_co2_path,noaa_nc_files)
length(noaa_nc_files.url)#10
names(noaa_nc_files.url)<-noaa_nc_names
summary(noaa_nc_files.url)
nc_dest<- vector("list",length = length(noaa_nc_names))
nc_dest<-file.path("data",noaa_nc_files)
nc_dest[10]
names(nc_dest)<-noaa_nc_names
# download one file after the other
open(url(url_co2_path))
download.file(noaa_nc_files.url[2],destfile = nc_dest[2])
#### only with stable internet
## worked with two sites
for (nm in noaa_nc_names){
  open(url(url_co2_path))
  download.file(noaa_nc_files.url[nm],destfile = nc_dest[nm])
  close(url(url_co2_path))
}
# all raw files ending with ".nc"
list.files("data/", ".nc")
nc_dest
# now nc_dest may be opened with nc_open
#=====================
close(url(url_co2_path))
# working locally only
require(ncdf4)
noaa_nc_data<-vector("list",length = length(noaa_nc_names))
names(noaa_nc_data)<-noaa_nc_names
# extract from raw data in nc_dest obs values
for(nm in noaa_nc_names){
   nc_raw<- nc_open(nc_dest[nm])
   noaa_nc_data[[nm]]<- tibble(tnm = nc_raw%>%ncvar_get("time"),
                          CO2=nc_raw%>%ncvar_get("value"))
   
}
#observations stored in noaa_nc_data
summary(noaa_nc_data)
noaa_nc_data[["bao"]]
require(tidyverse)
noaa_nc_names

#=====================
require(ncdf4)

# convert list of all sites to one dfr
nc_data_dfr<-noaa_nc_data%>% map_dfr(.f=rbind,.id= "name")
head(nc_data_dfr)
dim(nc_data_dfr)#2842883 3
nc_data_dfr$name%>%unique()# 10 sites
df<-nc_data_dfr%>% mutate(name=as_factor(name))%>%
  mutate(datetime=as.POSIXct(tnm,origin="1970-01-01",tz="UTC"))%>%
  group_by(name)%>% nest()
head(df)
df[[1]]==noaa_nc_names
df[[1]]# amt bao crv lef mbo sct snp wbi wgc wkt
df[[2]][[10]]%>% group_by(datetime)%>% summarise(CO2=mean(CO2,na.rm=T))
nc_sites_CO2<- list()
nc_sites_CO2<-df$data%>%map(function(x){group_by(.data=x,tnm)%>%summarise(CO2=mean(CO2,na.rm=T),datetime=first(datetime))})

names(nc_sites_CO2)<-toupper(noaa_nc_names)

summary(nc_sites_CO2)
nc_CO2_tibble<-nc_sites_CO2%>% map_df(bind_rows, .id= "name")
nc_CO2_tibble<-nc_CO2_tibble%>% 
  mutate(name= as_factor(name))%>% dplyr::select(-tnm)
summary(nc_CO2_tibble)
#plot all data with nc formatted CO2 obs
nc_CO2_tibble%>% ggplot(aes(x=datetime,y= CO2,col =name))+
  geom_point(size= 0.2,aes(x=datetime,y= CO2,col =name))+labs(x="",y="CO2[ppm]")+
  ggtitle("CO2-Immission at 10 sites:",
          subtitle = paste(noaa_nc_names,collapse = " "))

# select some sites c("AMT","BAO","CRV")
nc_CO2_tibble %>% subset(name %in% c("AMT","BAO","CRV")) %>%
  ggplot(aes(x=datetime,y= CO2,col =name))+
  geom_point(size= 0.2,aes(x=datetime,y= CO2,col =name))+
  labs(x="",y="CO2[ppm]")+
  ggtitle("CO2-Immission")
nc_CO2_tibble %>% subset(name %in% c("LEF","MBO","SCT")) %>%
  ggplot(aes(x=datetime,y= CO2,col =name))+
  geom_point(size= 0.2,aes(x=datetime,y= CO2,col =name))+
  labs(x="",y="CO2[ppm]")+
  ggtitle("CO2-Immission")
nc_CO2_tibble %>% subset(name %in% c("SNP","WBI","WGC")) %>%
  ggplot(aes(x=datetime,y= CO2,col =name))+
  geom_point(size= 0.2,aes(x=datetime,y= CO2,col =name))+
  labs(x="",y="CO2[ppm]")+
  ggtitle("CO2-Immission")
nc_CO2_tibble %>% subset(name %in% c("WKT","WBI","WGC")) %>%
  ggplot(aes(x=datetime,y= CO2,col =name))+
  geom_point(size= 0.2,aes(x=datetime,y= CO2,col =name))+
  labs(x="",y="CO2[ppm]")+
  ggtitle("CO2-Immission")
#=================================================
# extract variable values at 1 site
nc_open(nc_dest[1])$filename#"data/co2_amt_tower-insitu_1_ccgg_HourlyData.nc" 
nc_open(nc_dest[1])$var%>%summary()
require(ncdf4.helpers)
nc.get.variable.list(nc_open(nc_dest[1]),min.dim=1)# variables of nc_amt file
Lat<-nc_open(nc_dest[1])%>%ncvar_get("latitude")%>% first()%>%round(digits=2)#first of  323419 values
Lon<-nc_open(nc_dest[1])%>%ncvar_get("longitude")%>% first()%>%round(digits=2)
obs<-nc_open(nc_dest[1])%>%ncvar_get("time")%>% length()
nc_open(nc_dest[1])%>%ncvar_get("value")%>% length()#323419
nc_open(nc_dest[1])$var$time$units#"seconds since 1970-01-01T00:00:00Z"
tm<- ncvar_get(nc_open(nc_dest[1]),"time")%>% as.POSIXct(origin="1970-01-01")
length(tm)#323419
require(lubridate)
# visualize first file: amt_tower_hourly.nc
nc_CO2_tibble %>% subset(name =="AMT") %>%
  ggplot(aes(x=datetime,y= CO2,col =name))+
  geom_point(size= 0.2,aes(x=datetime,y= CO2,col =name))+
  labs(x="",y="CO2[ppm]")+
  ggtitle("CO2-Immission")

CO2_amt.tibble<- nc_CO2_tibble%>%subset(name=="AMT")

amt_plt<-CO2_amt.tibble%>%ggplot(aes(x=datetime,y=CO2))+
  geom_smooth(col ="red")+
  #geom_point(size= 0.3, shape= 4,alpha=0.01)+
  labs(x="",y= "CO2 [ppm]")
amt_plt+ggtitle(paste("CO2","AMT"),
               subtitle = paste0("Latitude = ",Lat,"° ;",
                                " Longitude = ",Lon,"° observations =", obs))
# add geoprms to obs values
add_prm<- function(nms){
  x<-nc_sites_CO2[[nms]]
  x<-mutate(.data=x,Lat=nc_open(nc_dest[[tolower(nms)]])%>%ncvar_get("latitude")%>% first(),
            Lon=nc_open(nc_dest[[tolower(nms)]])%>%ncvar_get("longitude")%>% first(),
            Alt =nc_open(nc_dest[[tolower(nms)]])%>%ncvar_get("altitude")%>% first())
}
nc_sites<-toupper(noaa_nc_names)%>% map(add_prm)
names(nc_sites)<-toupper(noaa_nc_names)
saveRDS(nc_sites,file = file.path("~/projects/Global_Temp/data","noaa_nc_data.rds"))

