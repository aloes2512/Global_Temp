CO2_data.lst<-readRDS("data/NOAA_CO2_data.rds")
require(tidyverse)
require(lubridate)
# list of start dates of obs
obs_start<-function(x){head(x,1)%>%.$datetime}
start_dates<-CO2_data.lst%>% 
  map_dbl(obs_start)%>%
  as.POSIXct(origin="1970-01-01")
obs_end<-function(x){tail(x,1)%>%.$datetime}
end_dates<-CO2_data.lst%>%map_dbl(obs_end)%>%
  as.POSIXct(origin="1970-01-01")
obs_range<-tibble(site= names(CO2_data.lst),
  startdt=start_dates,enddt=end_dates)
obs_range%>%tail(10)
obs_range<-obs_range%>% mutate(obs_diff= enddt-startdt)
# select specific ranges reference MLO
ymd("2000-01-01")-ymd("1969-08-20")# 11091 days
big_range<-obs_range%>%
  subset(obs_diff>=(ymd("2000-01-01")-ymd("1969-08-20")))%>%
  arrange(desc(obs_diff))
big_range_sites<-big_range$site# 7 sites
CO2_big_range<-CO2_data.lst[big_range_sites]%>%map_df(bind_rows)
CO2_big_range.plt<-CO2_big_range%>% ggplot(aes(x=datetime,y=CO2,col=site))+
  geom_smooth(data=CO2_big_range,method = "lm")
CO2_big_range.plt+  coord_cartesian(xlim = c(as.POSIXct("1997-01-01"),as.POSIXct("2001-12-31")),
                  ylim= c(365,375))
CO2_big_range.plt+ggtitle("Linear-Trend/
    CO2-Immission",
    subtitle="11000 days of observations")
# calculate slope
require(broom)
lin.mdl<-function(df)  {tbl<-lm(formula=df$CO2~df$datetime)%>%
  augment()%>%
  dplyr::select(-c(.hat,.sigma,.cooksd))
tbl<-tbl%>%rename(CO2=`df$CO2`,datetime=`df$datetime`)
return(tbl)
}
CO2_big_range_fit<-CO2_big_range%>% split(.$site)%>%
  map_df(lin.mdl,.id = "site")
yrsec<-60*60*24*365.24# 1 year in seconds
myslope<-function(mydat)(lm(data=mydat,formula=CO2~datetime)$coeff[2])*yrsec
CO2_incr.p.yr<-CO2_big_range%>%split(.$site)%>%map_dbl(myslope)
names(CO2_incr.p.yr)
CO2_increase<- tibble(Site=names(CO2_incr.p.yr),
                      Slope.p.yr=CO2_incr.p.yr,
                      dim= "[ppm/year]")%>%arrange(desc(Slope.p.yr))
library(openxlsx)
library(rio)
export(CO2_increase,"data/CO2_lintrend_long.xlsx")
# select different time span
CO2_range_2001.2020<-CO2_big_range%>% subset(datetime> ymd("2001-01-01"))
CO2_range_2001.2020
CO2_range_2000.2020_fit<-CO2_range_2001.2020%>% split(.$site)%>%
  map_df(lin.mdl,.id = "site")
CO2_2000.2020_lin.plt<-CO2_range_2000.2020_fit%>% ggplot(aes(x=datetime,y=.fitted,col=site))+
  geom_line()+ labs(x="",y= "CO2 [ppm]")
CO2_2000.2020_lin.plt+ggtitle("Linear-Trend/
CO2-Immission")
myslope<-function(mydat)(lm(data=mydat,formula=CO2~datetime)$coeff[2])*yrsec

CO2_incr.2001.2020<-CO2_range_2001.2020%>%split(.$site)%>%map_dbl(myslope)
names(CO2_incr.2001.2020)
CO2_increase.01to20<- tibble(Site=names(CO2_incr.2001.2020),
                      Slope.p.yr=CO2_incr.2001.2020,
                      dim= "[ppm/year]")%>%arrange(desc(CO2_incr.2001.2020))
export(CO2_increase.01to20,"data/CO2_lintrend_01to20.xlsx")
#==========================================================
early_end<-c("TAC","AMT","BAO")
fullyr_2019<-c("WKT","WGC","WBI","SNP","MBO","LEF","CET")
# limit selected obs to date > 2010
site_2010<-CO2_data.lst%>%
  map(function(x) x= subset(x,datetime>as.POSIXct("2010-01-01",origin = "1970-01-01")))

obs_start.date(site_2010[["MBO"]])
names(site_2010)==
names(CO2_data.lst)
# select sites active before year 2000
obs_befor_2000<-CO2_data.lst%>%
  map(function(x) x= filter(x,datetime<as.POSIXct("2000-01-01",origin = "1970-01-01")))
names(obs_befor_2000)#
Number_obs<-CO2_data.lst%>%map_df(bind_rows,.id= "site" )%>% split(.$site)%>%map_int(NROW)# 10 sites
ggplot(CO2_long.tbl)+
  geom_smooth(method = "lm",mapping=aes(x=datetime,y=CO2,col=site),data = CO2_long.tbl)
rest_sites<-c("ZEP","MHD")
CO2_rest.long.tbl<-CO2_data.lst[rest_sites]%>% map_df(bind_rows)
CO2_rest.long_plt<-CO2_rest.long.tbl%>%ggplot()+
  geom_smooth(method = "lm",mapping=aes(x=datetime,y=CO2,col=site),data =CO2_rest.long.tbl )#
CO2_rest.long_plt+geom_smooth(method = "lm",mapping=aes(x=datetime,y=CO2),col="black",data = CO2_data.lst[["ICE"]])
#not complete TAC, ABP,CRV,MBO,BHD,CPT
site_not.complt<- c("TAC","ABP","CRV","MBO","BHD","CPT")
CO2_site.slct<-CO2_data.lst[CO2_data.lst%>% map_int(NROW)>1500]
names(CO2_site.slct)
selected_sites<-setdiff(names(CO2_site.slct),site_not.complt)
CO2_selected.tbl<-CO2_data.lst[selected_sites]%>% 
  map_df(bind_rows)%>% filter(datetime>ymd("2010-01-01"))
CO2_site_2010.plt<-CO2_selected.tbl%>%
  ggplot(aes(x=datetime,y=CO2,col=site))+
  geom_smooth(method = "lm")
CO2_selected.tbl%>% filter(datetime>ymd("2010-01-01"))%>%summary()
station_sites<-readRDS("data/station_sites.rds")
station_sites%>% filter(Code %in% selected_sites)%>%
  arrange(Latitude)%>%
  dplyr::select(-c("GMTime","Project","Longitude"))
CO2_selected.tbl%>% 
  filter(site=="MLO")%>%filter(datetime>ymd("2010-01-01"))%>%
  ggplot(aes(x=datetime,y=CO2))+
  geom_smooth(method= "lm")
mlodat<-CO2_selected.tbl%>% subset(site=="MLO")
CO2_site_2010.plt+ geom_smooth(data = mlodat,
                               mapping = aes(x=datetime,y=CO2),method = "lm",col="black")
