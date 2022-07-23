# combine csv data-format with nc data-format
library(tidyverse)
library(lubridate)
NOAA_data.lst<-readRDS("data/NOAA_data.rds")
NOAA_data.lst[["MLO"]]
NOAA_ncdata.lst<-readRDS(file.path("~/projects/Global_Temp/data","noaa_nc_data.rds"))
names(NOAA_data.lst)
names(NOAA_ncdata.lst)
for ( i in 1:10){
  NOAA_ncdata.lst[[i]]<-NOAA_ncdata.lst[[i]]%>% 
    dplyr::select(datetime,CO2,lon="Lon",lat="Lat",alt="Alt")
  NOAA_ncdata.lst[[i]]<-NOAA_ncdata.lst[[i]]%>%
    mutate(site= names(NOAA_ncdata.lst)[i]%>%as_factor())
}
# is in both: CRV and equal considering 1 hour means
intersect(names(NOAA_data.lst),names(NOAA_ncdata.lst))
NOAA_data.lst[[1]]%>%tail()
NOAA_data.lst[["CRV"]]<-NULL
NOAA_ncdata.lst[["CRV"]]%>% tail()
NOAA_CO2.lst<-c(NOAA_data.lst,NOAA_ncdata.lst)
names(NOAA_CO2.lst)
saveRDS(NOAA_CO2.lst,file = file.path("data/NOAA_CO2_data.rds"))
rm(NOAA_data.lst,NOAA_ncdata.lst)
NOAA_CO2_data<-readRDS(file.path("data/NOAA_CO2_data.rds"))
#Wisconsin tall tower site (LEF) 
NOAA_CO2_data[["LEF"]]%>%tail()%>% mutate(HR=format(datetime,"%H"))

#Wisconsin tall tower site (LEF) 
NOAA_CO2_data[["LEF"]]%>% 
  mutate(HR=format(datetime,"%H"))%>%
  group_by(HR)%>% summarise(CO2=mean(CO2,na.rm = TRUE))%>%

  ggplot(aes(x=as.numeric(HR) ,y = CO2))+
  geom_smooth()
NOAA_CO2_data[["WKT"]]%>% 
  mutate(HR=format(datetime,"%H"))%>%
  group_by(HR)%>% summarise(CO2=mean(CO2,na.rm = TRUE))%>%
  ggplot(aes(x= HR,y = CO2))+
  geom_point()
names(NOAA_CO2_data)
NOAA_CO2_data[["DEUB044"]]%>% 
  mutate(HR=format(datetime,"%H"))%>%
  group_by(HR)%>% summarise(CO2=mean(CO2,na.rm = TRUE))%>%
  ggplot(aes(x= HR,y = CO2))+
  geom_point()
NOAA_CO2_data<-NOAA_CO2_data%>%
  map_df(bind_rows,.id= "Site")%>%
  mutate(Site=as_factor(Site))%>% dplyr::select(-site)
saveRDS(NOAA_CO2_data,file = "data/NOAA_CO2.rds")

