# linear regression model
CO2_data.lst<- readRDS("~/projects/Global_Temp/data/NOAA_data.rds")
summary(CO2_data.lst)

CO2_regr_mdl<-CO2_data.lst%>%map(~lm(.$CO2~.$datetime,data = .))
names(CO2_regr_mdl)
CO2_fitted<-CO2_regr_mdl%>% map(augment)
CO2_fitted[[1]]%>%rename(CO2=`.$CO2`)
CO2_fitted<-CO2_fitted%>% map(rename,CO2=`.$CO2`,
                  datetime=`.$datetime`,
                  CO2_fit= .fitted)

# combine with site parameters
station_sites<-readRDS("~/projects/Global_Temp/data/station_sites.rds")
stations<- dplyr::select(station_sites,Code,Name,Latitude,Longitude,Elevation)
names(CO2_fitted)==names(CO2_data.lst)

#========
CO2_data.lst[[1]]%>% mutate(dttm=as.numeric(datetime))
nmdt<-function(x){
  x<-mutate(x,dttm=as.numeric(datetime))%>%
    dplyr::select(-datetime)}
(nmdt(CO2_data.lst[[1]]))
CO2_data.<-CO2_data.lst%>% map(nmdt)
CO2_data.[[19]]
CO2_data<-CO2_data.%>% map_dfr(bind_rows,.id = "tibble")
# umwandeln von datetime in numeric
CO2_fitted<-CO2_fitted%>%map(nmdt)%>% 
  map_dfr(bind_rows,.id = "tibble")
head(CO2_fitted)
tail(CO2_fitted)
names(CO2_fitted)  
rownames(CO2_fitted)%>% tail()
# plot data

CO2_fitted<-CO2_fitted%>% 
  mutate(datetime=as.POSIXct(dttm,origin="1970-01-01",tz="UTC"))%>%
  dplyr::select(-dttm)
CO2_lin_plt<-CO2_fitted%>% group_by(tibble)%>%
  ggplot(aes(x=datetime,y=CO2_fit,col= tibble))+
  geom_line()
CO2_lin_plt+ggtitle("CO2-linear trend",
     subtitle = "19 sites NOAA and UBA data")+
  labs(x="",y= "CO2[ppm]")
CO2_lin_plt+ geom_point(aes(x=datetime,y=CO2),size =0.1,alpha= 0.1)
# select northern stations
northern_stations<-stations %>% subset(Latitude> 55)
southern_stations<-stations%>% subset(Latitude< -55) 
north_CO2_fit<-northern_stations%>% inner_join(CO2_fitted,by= c("Code"="tibble"))
south_CO2_fit<-southern_stations%>% inner_join(CO2_fitted,by= c("Code"="tibble"))
#plot northern stations trend
north_CO2_lin_plt<-north_CO2_fit%>% group_by(Code)%>%
  ggplot(aes(x=datetime,y=CO2_fit,col= Code))+
  geom_line()
north_CO2_lin_plt+ggtitle("CO2-linear trend",
          subtitle = "5 sites Arctica: Barrow,Carbon Arctic,
Storhofdi(ICE),Summit,Ny-Alesund(Svalbard,ZEP) ")+
  labs(x="",y= "CO2[ppm]")
#plot southern stations trend
south_CO2_lin_plt<-south_CO2_fit%>% group_by(Code)%>%
  ggplot(aes(x=datetime,y=CO2_fit,col= Code))+
  geom_line()

south_CO2_lin_plt+ggtitle("CO2-linear trend",
            subtitle = "2 sites South Pole (SPO)&Palmer (PSA)")+
                            labs(x="",y= "CO2[ppm]")
