#NOAA CO2 data have been imported, formated and stored @ NOAA_data.rds
# report stored as CO2_global_distribution.odt
CO2_data.lst<-readRDS("data/NOAA_CO2_data.rds")
require(tidyverse)
CO2_data.lst%>%summary()
#station parameters listed by NOAA
station_sites<-readRDS("data/station_sites.rds")
station_sites%>%head()
site_nms<-names(CO2_data.lst)# 29 sites
stnms<-tibble(site_nms=names(CO2_data.lst))
# selected sites
stations.sel<-stnms%>%left_join(station_sites, by= c("site_nms"="Code"))
stations.sel%>% filter(site_nms=="SMO")%>% .$Name # "Tutuila"
# models need numerical formatted time series
nmdt<-function(x) x<-mutate(x,datetime=as.numeric(datetime))
CO2_data.num<-CO2_data.lst%>% map_df(nmdt)
CO2_data.num<-CO2_data.num%>% right_join(stations.sel, by =c("site"="site_nms"))
class(CO2_data.num)# tbl
head(CO2_data.num)
# linear regression model per site
CO2_lin_mdl<-CO2_data.num%>%dplyr::select(datetime,CO2,site)%>%
  split(.$site)%>%
  map(function(x) lm(x$CO2~x$datetime))
require(broom)
CO2_lin_mdl%>% .[[1]]
CO2_lin_fit<-CO2_lin_mdl%>% map(augment)%>% 
  map(rename,
      c("CO2"=`x$CO2`),
      c("datetime"=`x$datetime`),
      c("CO2_fit" = ".fitted"),
      c("CO2_resd"=".resid"))

site_nms
CO2_lin_fit.tbl<- tibble()
for (nm in site_nms){
  df<-mutate(CO2_lin_fit[[nm]],
      Name =as_factor(nm),
      datetime=as.POSIXct(datetime,origin="1970-01-01",
      tz = "UTC"))
  CO2_lin_fit.tbl<- bind_rows(CO2_lin_fit.tbl,df)
}
head(CO2_lin_fit.tbl)
# Plot fitted CO2 data
CO2_lin.trnd_plt<-CO2_lin_fit.tbl%>%ggplot(aes(x=datetime,y=CO2_fit,col = Name))+
  geom_line()
CO2_lin.trnd_plt+ggtitle("CO2-Immission",
                     subtitle = "Linear Trend 29 sites")+
  labs( x= "",y= "CO2 [ppm]")
#plot only 4 noaa base sites
CO2_noaa.trnd_plt<-CO2_lin_fit.tbl%>% 
  subset(Name %in% c("MLO","SPO","SMO","BRW"))%>%
  ggplot(aes(x=datetime,y=CO2_fit,col = Name))+
  geom_line()
CO2_noaa.trnd_plt+ggtitle("CO2-Immission Linear Trend",
                         subtitle = "4 NOAA Base Sites")+
  labs( x= "",y= "CO2 [ppm]")
stations.sel%>% subset (site_nms %in% c("MLO","SPO","SMO","BRW"))
#model coefficients
CO2_mdl.pms<-CO2_data.num%>%dplyr::select(datetime,CO2,site)%>%
  split(.$site)%>%
  map_df(function(x) lm(x$CO2~x$datetime)$coefficients)%>%
                      bind_cols(stnms)%>%
                      arrange(desc(`x$datetime`))
CO2_mdl.pms<-CO2_mdl.pms%>%dplyr::select(site=site_nms,slope=`x$datetime`,Intcpt=`(Intercept)`)
yrsec<-60*60*24*365.24# 60 sec; 60 min; 24 hours; 365.24 days
CO2_mdl.pms$slope*yrsec
range(CO2_mdl.pms$slope*yrsec)
CO2_mdl.pms<-CO2_mdl.pms%>% 
  mutate(slp.pa=round(slope*yrsec,digits=2),Intcpt=round(Intcpt,digits=0))%>%
  dplyr::select(-slope)%>% arrange(slp.pa)
tail(CO2_mdl.pms)
# export as excel .xlsx
CO2_mdl.pms<-CO2_mdl.pms%>% left_join(station_sites, by= c("site"="Code"))%>%
  dplyr::select(-c(GMTime,Project))
library(openxlsx)
library(rio)
export(CO2_mdl.pms,"NOAA_linear_change.xlsx")
#================================
# Residuals from  lin fit
CO2_lin_fit.tbl%>% head()
CO2_resd<-CO2_lin_fit.tbl%>% dplyr::select(-c(.hat,CO2_fit,.cooksd,.std.resid,.sigma))
#eliminate outlier obs defined as abs(resd)> 2*sd
elim_outlier<- function(x,f=4){ var= x$CO2_resd
SD= sd(var)
x=subset(x,abs(x$CO2_resd)< f*SD)
return(x)
}
CO2_wout<-CO2_resd%>% split(.$Name)%>% map_df(elim_outlier)

CO2_MLO.resd<-CO2_resd%>% subset(Name=="MLO")
CO2_resd.plt<-CO2_MLO.resd%>% ggplot(aes(x=datetime,y=CO2_resd))+
  geom_point(size= 0.1)
SPO_dev_plt<-CO2_wout%>% subset(Name=="SPO")%>% ggplot(aes(x=datetime,y=CO2_resd))+
  geom_point(size= 0.1)+
  geom_smooth(col= "red")+labs(x="",y="CO2-deviance [ppm")
SPO_dev_plt+ ggtitle("CO2-Immission @ South Pole",
                     subtitle = "outliers > 2 * sd eliminated")
CO2_resd.plt+geom_smooth(mapping=aes(x=datetime,y= CO2_resd),col="red")+
  ggtitle("CO2_deviance to linear regression",
          subtitle= "South Pole Observations")
saveRDS(CO2_resd,file = "data/CO2_resd.rds")
CO2_resd$Name%>% unique()# 29
CO2_resd%>% subset(Name %in% c("MLO","SPO"))%>%
                     ggplot(aes(x=datetime,y= CO2_resd,col = Name))+
  geom_point(size= 0.1)+
  ggtitle("CO2-Immission Mauna Loa & South Pole",
          subtitle = "deviation from Linear Trend")+
  labs(x="",y= "CO2-deviance [ppm]")+
coord_cartesian(ylim = c(-25,+15))
CO2_resd%>% subset(Name %in% c("BRW","SMO"))%>%
  ggplot(aes(x=datetime,y= CO2_resd,col = Name))+
  geom_point(size= 0.1)+
  ggtitle("CO2-Immission Barrow & American Samoa",
          subtitle = "Deviation from Linear Trend")+
  labs(x="",y= "CO2-deviance [ppm]")
#=======================
# High altitude sites
high.alt_sites<-station_sites%>% subset(Elevation> 2000)%>% inner_join(stnms,by= c("Code"="site_nms"))
pull(high.alt_sites,Code)#"MBO"     "MEX"     "MLO"     "SPO"     "SUM"     "DEUB044"
CO2_resd%>% subset(Name %in% pull(high.alt_sites,Code))%>%
  ggplot(aes(x=datetime,y= CO2_resd,col = Name))+
  geom_point(size= 0.1)+
  coord_cartesian(ylim = c(-25,35))+
  ggtitle("CO2-Immission High Alt. ",
          subtitle = "Deviation from Linear Trend")+
  labs(x="",y= "CO2-deviance [ppm]")
CO2_resd%>% subset(Name %in% c("DEUB004","DEUB044"))%>%
  ggplot(aes(x=datetime,y= CO2_resd,col = Name))+
  geom_point(size= 0.1)+
  coord_cartesian(ylim = c(-25,35))+
  ggtitle("CO2-Immission Schauinsland, Zugspitze ",
          subtitle = "Deviation from Linear Trend")+
  labs(x="",y= "CO2-deviance [ppm]")
# CO2 variance of resides 
CO2_resd%>% filter(Name == "MLO")%>% summarise (SD = sd(CO2))# 23.4
SD_site<-CO2_resd%>% split(.$Name)%>%map_df(function(x){summarise(x,SD=sd(CO2))},
.id= "Name")
SD_site<-SD_site%>%mutate(Site=as_factor(Name))%>% dplyr::select(-Name)
station_sites<-station_sites%>%rename("Site"= "Code")
sit<-dplyr::select(station_sites,Site,Name,Elevation)%>% mutate(Site=as_factor(Site))
SD_site<-SD_site%>% left_join(sit) 
SD_site<-SD_site%>%arrange(SD)
SD_site%>%tail(12)
export(SD_site,"data/SD_NOAA_Site.xlsx")

# reduce observations to year 2000 until year 2021
require(lubridate)
CO2_lin.trnd_plt.00to21<-CO2_lin_fit.tbl%>%
                      subset(datetime>  ymd("2000-01-01"))%>%
                      subset(Name %in% c("DEUB044","BRW"))%>%
                      ggplot(aes(x=datetime,y=CO2_fit,col = Name))+
                      geom_line()
CO2_lin.trnd_plt.00to21+ggtitle("CO2-Immission Linear Trend",
                      subtitle = "2 sites; observations from 2000 to 2021")+
                      labs( x= "",y= "CO2 [ppm]")



# nonlinear trend
require(mgcv)
CO2_gam.20_mdl<-CO2_data.num%>%dplyr::select(datetime,CO2,site)%>%
  split(.$site)%>%
  map(function(x) mgcv::gam(formula = x$CO2 ~s(x$datetime,k=20)))
 summary(CO2_gam.20_mdl)
 CO2_nonlin.fit<- vector("list", length = NROW(stations.sel))
 names(CO2_nonlin.fit)<-stations.sel$site_nms
 CO2_data.num%>% subset(site=="MLO")
 CO2_gam.20_mdl[["MLO"]]$fitted.values %>% length()# 3028
CO2_nonlin.fit<-CO2_gam.20_mdl%>% map(augment)%>% 
  map(rename,
      c("CO2"=`x$CO2`),
      c("datetime"=`x$datetime`),
      c("CO2_fit" = ".fitted"),
      c("CO2_resd"=".resid"))
CO2_nonlin.fit<-CO2_nonlin.fit%>% map_dfr(bind_rows,.id= "Name")
CO2_nonlin.mdl<-CO2_nonlin.fit%>%  mutate(datetime= as.POSIXct(datetime,origin = "1970-01-01",tz="UTC"))

CO2_nonlin_plt<-CO2_nonlin.mdl%>%ggplot(aes(x=datetime,y= CO2_fit,col= as_factor(Name)))+
  geom_line(size = 0.2)#
CO2_nonlin_plt+ggtitle("CO2-nonlinear Trend 20 sites",
  subtitle = "~ MLO linear trend (black)")+labs(x="",y= "CO2_fit [ppm]")+
  geom_smooth(method ="lm",data =CO2_MLO.resd,mapping=aes(x=datetime, y= CO2),col ="black",size=0.8 )
# plot subset of data
