CO2_data.lst<-readRDS("data/NOAA_data.rds")
require(tidyverse)
CO2_data.lst%>%summary()
station_sites<-readRDS("data/station_sites.rds")
station_sites%>%head()
all_site_nms<- station_sites$Code
length(all_site_nms)#217
intersect(all_site_nms,nms)
nms<-names(CO2_data.lst)# 20 sites
stnms<-tibble(nms=names(CO2_data.lst))
# selected sites
stations.sel<-stnms%>%left_join(station_sites, by= c("nms"="Code"))%>% 
  dplyr::select(-Country)
nmdt<-function(x) x<-mutate(x,datetime=as.numeric(datetime))
CO2_data.nmdt<-CO2_data.lst%>% map_df(nmdt)
CO2_data.nmdt<-CO2_data.nmdt%>% right_join(stations.sel, by =c("site"="nms"))
# limited time span of data time in seconds from 1970-01-01
require(lubridate)

start_tm = function(st) {as.numeric(ymd_hms(paste0("20",st,"-01-01 00:00:00")))}
start_tm("19")
CO2_data.nmdt%>% subset(datetime > start_tm("00"))
# linear regression model per site
CO2_regr_mdl.00<-CO2_data.nmdt%>%
  subset(datetime > start_tm("00"))%>% 
  dplyr::select(datetime,CO2,site)%>%
  split(.$site)%>%
  map(function(x) lm(x$CO2~x$datetime))
require(broom)
CO2_regr_fit.00<-CO2_regr_mdl.00%>%map(augment)%>% 
  map(rename,
      c("CO2"=`x$CO2`),
      c("datetime"=`x$datetime`),
      c("CO2_fit" = ".fitted"),
      c("CO2_resd"=".resid"))
CO2_lin.fit.00<- tibble()
nms<-names(CO2_data.lst)
for (nm in nms){
  df<-mutate(CO2_regr_fit.00[[nm]],
             Name =nm,
             datetime=as.POSIXct(datetime,origin="1970-01-01",
                                 tz = "UTC"))
  CO2_lin.fit.00<- bind_rows(CO2_lin.fit.00,df)
}
CO2_lin.fit.00$Name<-as_factor(CO2_lin.fit.00$Name)
head(CO2_lin.fit.00)
summary(CO2_lin.fit.00)
CO2_trnd.00_plt<-CO2_lin.fit.00%>%subset(Name != "TAC")%>%
  ggplot(aes(x=datetime,y=CO2_fit,col = Name))+
  geom_line()
CO2_data.lst[["TAC"]]%>% summary()
# Focus on TAC observtions
start_tm("14")#1388534400
start_tm("16")#1451606400
# select timespan 2014 to 2016
CO2_regr_mdl.14_16<-CO2_data.nmdt%>% subset(datetime>start_tm("14")& datetime< start_tm("16"))%>%
  dplyr::select(datetime,CO2,site)%>%
  split(.$site)%>%
  map(function(x) lm(x$CO2~x$datetime)) 
CO2_regr_fit.14_16<-CO2_regr_mdl.14_16%>% map(augment)%>% 
  map(rename,
      c("CO2"=`x$CO2`),
      c("datetime"=`x$datetime`),
      c("CO2_fit" = ".fitted"),
      c("CO2_resd"=".resid"))  
CO2_lin.fit.14_16<- tibble()
nms<-names(CO2_regr_fit.14_16)# no observations at ABP Brasilian site in this timespan
nm<-nms[1]
for (nm in nms){
  df<-mutate(CO2_regr_fit.14_16[[nm]],
             Name =nm,
             datetime=as.POSIXct(datetime,origin="1970-01-01",
                                 tz = "UTC"))
  CO2_lin.fit.14_16<- bind_rows(CO2_lin.fit.14_16,df)
}
CO2_lin.plt<-CO2_lin.fit.14_16%>%ggplot(aes(x=datetime,y=CO2_fit,col = Name))+
  geom_line()
# map gam model
#"https://mfasiolo.github.io/mgcViz/articles/mgcviz.html"
url_mgcViz<-"https://mfasiolo.github.io/mgcViz/articles/mgcviz.html"
browseURL(url_mgcViz)
require(mgcViz)
CO2_gam_mdl.14_16<-CO2_data.nmdt%>% subset(datetime>start_tm("14")& datetime< start_tm("16"))%>%
  dplyr::select(datetime,CO2,site)%>%
  split(.$site)%>%
  map(function(x) mgcv::gam(formula=x$CO2~s(x$datetime,k=12),data=x,method="REML"))
CO2_gam_mdl.14_16<-getViz(CO2_gam_mdl.14_16)
myplt<-plot(CO2_gam_mdl.14_16)
myplt+ggtitle("Δ CO2 @ 19 sites",
              subtitle = " 2 years from 2014-01-01 to 2016-01-01")+
  labs(x = "datetime in sec from 1970-01-01", y="Δ CO2 [ppm]")
summary(CO2_gam_mdl.14_16)
SPO_plt<- plot(CO2_gam_mdl.14_16[["SPO"]])+
  labs(x = "datetime in sec from 1970-01-01", y="Δ CO2 [ppm]")+
  ggtitle("Δ CO2 @ South Pole (Antarctica)",
          subtitle = " 2 years from 2014-01-01 to 2016-01-01")
SPO_plt+ l_fitLine(col="red",size =2)+l_ciLine(col = "blue",linetype = 3)+
  l_points(shape = 1, size = 1) + theme_classic()
Zugsp_plt<- plot(CO2_gam_mdl.14_16[["DEUB044"]])+labs(x = "datetime in sec from 1970-01-01", y="Δ CO2 [ppm]")
Zugsp_plt+ggtitle("Δ CO2 @ Zugspitze (Germany)",
                  subtitle = " 2 years from 2014-01-01 to 2016-01-01")+
  l_fitLine(col="red",size =2)+l_ciLine(col = "blue",linetype = 3)+
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()
