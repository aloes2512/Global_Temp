#NOAA CO2 data havebeen imported, formated and stored @ NOAA_data.rds
CO2_data.lst<-readRDS("data/NOAA_data.rds")
CO2_data.lst%>%summary()
#station parameters are listed by NOAA
station_sites<-readRDS("data/station_sites.rds")
station_sites%>%head()
nms<-names(CO2_data.lst)# 20 sites
stnms<-tibble(nms=names(CO2_data.lst))
stations.sel<-stnms%>%left_join(station_sites, by= c("nms"="Code"))%>% 
  dplyr::select(-Country)
nmdt<-function(x) x<-mutate(x,datetime=as.numeric(datetime),)

CO2_data.nmdt<-CO2_data.lst%>% map_df(nmdt)
CO2_data.nmdt<-CO2_data.nmdt%>% right_join(stations.sel, by =c("site"="nms"))
class(CO2_data.nmdt)
head(CO2_data.nmdt)
CO2_regr_mdl<-CO2_data.nmdt%>%dplyr::select(datetime,CO2,site)%>%
  split(.$site)%>%
  map(function(x) lm(x$CO2~x$datetime))
require(broom)
CO2_regr_mdl<-CO2_regr_mdl%>% map(augment)%>% 
  map(rename,
      c("CO2"=`x$CO2`),
      c("datetime"=`x$datetime`),
      c("CO2_fit" = ".fitted"),
      c("CO2_resd"=".resid"))
nms
CO2_mdl<- tibble()
for (nm in nms){
  df<-mutate(CO2_regr_mdl[[nm]],
      Name =nm,
      datetime=as.POSIXct(datetime,origin="1970-01-01",
      tz = "UTC"))
  CO2_mdl<- bind_rows(CO2_mdl,df)
}
CO2_mdl$Name<-as_factor(CO2_mdl$Name)
head(CO2_mdl)
CO2_trnd_plt<-CO2_mdl%>%ggplot(aes(x=datetime,y=CO2_fit,col = Name))+
  geom_line()
CO2_trnd_plt+ggtitle("CO2-Imission Linear Trend",
                     subtitle = "20 sites")+
  labs( x= "",y= "CO2 [ppm]")
# Residuals
CO2_resd<-CO2_mdl%>% dplyr::select(-c(.hat,CO2_fit,.cooksd,.std.resid,.sigma))
CO2_MLO.resd<-CO2_resd%>% subset(Name=="MLO")
CO2_resd.plt<-CO2_MLO.resd%>% ggplot(aes(x=datetime,y=CO2_resd))+
  geom_point(size= 0.1)
CO2_resd.plt+geom_smooth(mapping=aes(x=datetime,y= CO2_resd),col="red")
saveRDS(CO2_resd,file = "data/CO2_resd.rds")

