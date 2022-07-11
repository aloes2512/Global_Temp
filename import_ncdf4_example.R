library(tidyverse)
browseURL("https://rpubs.com/boyerag/297592")
require(ncdf4)
library(lubridate)
nc_data <- nc_open('~/downloads/co2_amt_tower-insitu_1_ccgg_HourlyData.nc')
nc_data$format#"NC_FORMAT_NETCDF4"
print(nc_data)
#dataset_start_date: 2003-09-19T00:00:00Z
#dataset_stop_date: 2019-07-24T23:00:00Z
dif_days<-ymd_h("2019-07-24T23")-ymd_h("2003-09-19T00")
dif_days*24# 138911 days
nc_data$var%>%summary()
# get variable names
names(nc_data$var)
require(ncdf4.helpers)
nc.get.variable.list(nc_data,min.dim=1)
# Anzahl der Beobachtungen
nc_data$dim$obs$len #323419
# lade einzelne variable
ncvar_get(nc_data,"longitude")# -68.6821
ncvar_get(nc_data,"latitude")#45.0345
# working with the data
url_ncdf<-"https://rpubs.com/boyerag/297592"
browseURL(url_ncdf)
# Convert decimal date to POSIXct format
nc_data$var$time$units# "seconds since 1970-01-01T00:00:00Z"
ncvar_get(nc_data,"time") %>% length()#323419 observations
tm<- ncvar_get(nc_data,"time") 
length(tm)#323419
tm[323419]#1564009200
tm[1]# 1063929600
start<-ymd_hms("1970-01-01 00:00:00")+tm[1]# "2003-09-19 UTC"
end <- ymd_hms("1970-01-01 00:00:00")+tm[length(tm)]
tmseq<- seq(from=start[1],to=end[1],by="1 sec")
regulr_tmseq<- tibble(t= tmseq,
                      tnm= as.numeric(t))
regulr_tmseq%>% 
  mutate(t_day.nm= round(tnm/(24*60*60)))%>%
  group_by(t_day)%>% summarise(t=median(t))
nc_tibble<- tibble(tnm = nc_data%>%ncvar_get("time"),
                   CO2=nc_data%>%ncvar_get("value"))

nc_dataformated<-regulr_tmseq%>% left_join(nc_tibble,by= "tnm")
tail(nc_dataformated)
regulr_tmseq%>%ggplot(aes(x=t))+
  geom_point(data=nc_tibble,aes(x=tnm,y=CO2),size=0.2,alpha=0.3)
