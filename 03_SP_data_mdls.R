url_ts<-"http://homepage.divms.uiowa.edu/~kchan/TSA.htm"
browseURL(url_ts)
# sunspots periodicity
SP_path<-"http://www.sidc.be/silso"
SP_dat_path<- "/Users/alfloeffler/Google Drive/SN_d_tot_V2.0.csv"
sunspot.daily<-read.csv2(SP_dat_path)%>%
  mutate(date = glue::glue("{Year}-{Month}-{Day}"),
         date = as.Date(date, format = "%Y-%m-%d"))%>%
  dplyr::select(date,SP=Daily.Nr)
summary(sunspot.daily)
SP_daily.xts<- xts(sunspot.daily$SP,order.by = sunspot.daily$date)
SP_daily.xts%>%head(2)
colnames(SP_daily.xts)<-"SP_daily"
# periodicity
periodogram(y=coredata(sunspots_xts))
dim(coredata(SP_daily.xts))#[1] 74568     1
length(index(SP_daily.xts))#74568
length(sunspot.daily$SP)
Y<-coredata(SP_daily.xts)%>% na.approx(x=index(SP_daily.xts))
SP_sun<-periodogram(y=Y)
dt_SPsun<- tibble(freq=SP_sun$freq,spec=SP_sun$spec)%>%arrange(desc(spec))%>%head()
(1/dt_SPsun[[1,1]])/365.24#10.81 Jahre
SP_daily.xts%>%subset(SP_daily>0) %>%ggplot(aes(x=index(.),y=SP_daily))+
  geom_point(size = 0.1)+
  geom_smooth()
# CRAN dataset
data(sunspots)
summary(sunspots)
require(tidyverse)
require(lubridate)
sunspots%>% head()
class(sunspots)#ts
# two data formats
sunspots_xts <- as.xts(sunspots)
sunspots_tbl<-tibble(date=index(sunspots_xts), SP=coredata(sunspots_xts))
length(sunspots_xts)#2820
sunspots_xts[1,1]# Jan 1749 58
autoplot(sunspots_xts)+ggtitle("Monthly average Sunspots")
require(forecast)
ma<-rollmean(sunspots_xts,k=131)# 11 years in month
class(ma)
SP_smooth<-ma%>% merge(sunspots_xts)
colnames(SP_smooth)<-c("SP_fit" ,"SP")
summary(SP_smooth)
SP_smooth%>% ggplot(aes(x= index(SP_smooth),y= SP))+
  geom_line()+
  geom_line(aes(y= SP_fit),col = "red")+
  ggtitle("Sunspots (monthly average",
  subtitle = "+ moving average 131 month")
# Fourier Analysis
require(TSA)
p_sun<-periodogram(y=coredata(sunspots_xts))
#select 6 frequencies with highest amplitude (spec)
dt_sun<- tibble(freq=p_sun$freq,spec=p_sun$spec)%>%arrange(desc(spec))%>%head()
maxfreq<-1/dt_sun[[1,1]]# 130.09 month
#Gaussian fit
library(mgcv)
head(sunspot.daily,1)
sunspot.daily<-sunspot.daily%>% mutate(nmdt=as.numeric(date))
SP_gauss.60<- gam(data=sunspot.daily,formula = SP~s(nmdt,k=60),method = "REML")
AIC(SP_gauss.60)#[1] 747421.9 # AIC with 60 Basis functions is [1] 747421.9
require(broom)
SP_fit60<-SP_gauss.60%>%augment()%>% dplyr::select(nmdt,SP,SP_fit.60=.fitted)
SP_data.fit60<-sunspot.daily%>%
  left_join(SP_fit60,by= "nmdt")%>%
  dplyr::select(date,SP=SP.x,SP_fit.60)%>%as_tibble()
SP_data.fit60%>% head(10)
SP_plt<-SP_data.fit60%>% subset(SP>0)%>%ggplot(aes(x=date,y=SP))+
  geom_point(size=0.1)+
  ggtitle("Sunspots daily",
          subtitle = "data source http://www.sidc.be/silso")+
          labs(x="",y= "SP counts")
SP_plt + geom_line(aes(y=SP_fit.60),col= "red")
# fitting with negbinom NB
require(MASS)
negative.binomial(theta = 2,link="log")
SP_mdl.nbin<- mgcv::gam(data=sunspot.daily,formula = SP~s(nmdt,k=60),family=negbin(1),scale=1)
summary(SP_mdl.nbin)# intercept 4.033414; exp(4.033414)= 56.45331 edf 58.91
AIC(SP_mdl.nbin)#[1] 719990.1 which is lower than with gaussian fit 746832.9 
SP_NB_fit<-SP_mdl.nbin%>%augment()%>% dplyr::select(nmdt,SP,.fitted)%>% mutate(date=as.Date(nmdt,origin="1970-01-01"),SP_fit=exp(.fitted))
SP_NB_plt<-SP_NB_fit%>% ggplot(aes(x=date,y= SP))+
  geom_point(size=0.1)+
  ggtitle("Sunspots daily count",
          subtitle = "data source http://www.sidc.be/silso")+
          labs(x="",y= "SP counts")
SP_NB_plt+geom_line(aes(y= SP_fit),col="red")
# periodicity
require(TSA)
# fourier transform of time series
SP_NB_fit%>%head()
SP_NB.xts<- xts(SP_NB_fit$SP_fit,order.by =SP_NB_fit$date)
p_sun = periodogram(SP_NB.xts)
summary(p_sun)
#select 10 frequencies with maximum amplitude
dt_sun<- tibble(freq=p_sun$freq,spec=p_sun$spec)%>%arrange(desc(spec)) #Max is 4000 days
1/0.000264 

