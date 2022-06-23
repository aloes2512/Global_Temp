# linear regression model
CO2_data.lst<- readRDS("~/projects/Global_Temp/data/NOAA_data.rds")
summary(CO2_data.lst)
CO2_data.lst%>% map(summary)
length(CO2_data.lst)# 20
require(tidyverse)
require(mgcv)
CO2_regr_mdl<-CO2_data.lst%>%map(~gam(.$CO2~.$datetime,data = .,method = "REML"))
names(CO2_regr_mdl)
require (broom)
CO2_fitted<-CO2_regr_mdl%>% map(augment)%>%
                             map(rename,CO2=`.$CO2`,
                              datetime=`.$datetime`,
                              CO2_fit= .fitted)%>%
                          map(dplyr::select,datetime,CO2,CO2_fit)

# combine with site parameters
station_sites<-readRDS("~/projects/Global_Temp/data/station_sites.rds")
stations<- dplyr::select(station_sites,Code,Name,Latitude,Longitude,Elevation)
names(CO2_fitted)==names(CO2_data.lst)

#========
CO2_data.lst[[1]]%>% mutate(dttm=as.numeric(datetime))
nmdt<-function(x){
  x<-mutate(x,dttm=as.numeric(datetime))
}
(nmdt(CO2_data.lst[[1]]))
CO2_data.<-CO2_data.lst%>% map(nmdt)%>%map(function(x){ x<- x%>%dplyr::select(dttm,site,CO2, lat,lon,alt)})
CO2_data.[[20]]
CO2_data<-CO2_data.%>% map_dfr(bind_rows,.id = "tibble") #
# umwandeln von datetime in numeric
CO2_fit_nmdt<-CO2_fitted%>%map(nmdt)%>% map(dplyr::select,-datetime)%>%
  map_dfr(bind_rows,.id = "site")
head(CO2_fit_nmdt)
tail(CO2_fit_nmdt)
names(CO2_fit_nmdt)  
rownames(CO2_fit_nmdt)%>% tail()
# plot data

CO2_fit.tbl<-CO2_fit_nmdt%>% 
  mutate(datetime=as.POSIXct(dttm,origin="1970-01-01",tz="UTC"))%>%
  dplyr::select(-dttm)


summary(CO2_fit.tbl) # outlier CO2==195.6 @ Schauinsland replaced 
CO2_lin_plt<-CO2_fit.tbl%>% group_by(site)%>%
  ggplot(aes(x=datetime,y=CO2_fit,col= site))+
  geom_line()
CO2_lin_plt+ggtitle("CO2-linear trend",
     subtitle = "20 sites [ESRL and UBA] data")+
  labs(x="",y= "CO2[ppm]")
# select northern stations
northern_stations<-stations %>% subset(Latitude> 55)
southern_stations<-stations%>% subset(Latitude< -55) 
north_CO2_fit<-CO2_fit.tbl%>% subset (site %in% northern_stations$Code)
south_CO2_fit<-CO2_fit.tbl%>% subset (site %in% southern_stations$Code)
#plot northern stations trend
north_CO2_lin_plt<-north_CO2_fit%>% group_by(site)%>%
  ggplot(aes(x=datetime,y=CO2_fit,col= site))+
  geom_line()
north_CO2_lin_plt+ggtitle("CO2-linear trend",
          subtitle = "5 sites Arctica: Barrow (BRW),Carbon Arctic(CRV),
Storhofdi(ICE),Summit(SUM),Ny-Alesund(Svalbard(ZEP)) ")+
  labs(x="",y= "CO2[ppm]")
#plot southern stations trend
south_CO2_lin_plt<-south_CO2_fit%>% group_by(site)%>%
  ggplot(aes(x=datetime,y=CO2_fit,col= site))+
  geom_point(aes(x=datetime,y= CO2,col=site))+
  geom_line(alpha= 0.4)

south_CO2_lin_plt<-south_CO2_lin_plt+ggtitle("CO2-linear trend",
            subtitle = "2 sites South Pole (SPO)&Palmer (PSA)")+
                            labs(x="",y= "CO2 [ppm]")
#calculate residuals CO2-CO2_fit
CO2_fit_nmdt%>% head()
CO2_lin.resd<-CO2_fit_nmdt%>% mutate(CO2_resd= CO2-CO2_fit)
saveRDS(CO2_lin.resd,"~/projects/Global_Temp/data/CO2_lin.resd.rds")
