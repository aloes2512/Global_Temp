# Sunspot data WDC-SISO
#"https://wwwbis.sidc.be/silso/datafiles/SN_d_tot_V2.0.csv"
library(tidyverse)
SP_path<-"http://www.sidc.be/silso"
browseURL(SP_path)# opens page for various downloads
require(lubridate)
# sunspot data
SP_dat_path<- "/Users/alfloeffler/Desktop/Klima_Energiewende/Daten/SN_d_tot_V2.0.csv"
sunspot.daily<-read.csv2(SP_dat_path)%>%
  mutate(date = glue::glue("{Year}-{Month}-{Day}"),
         date = as.Date(date, format = "%Y-%m-%d"))%>%
  dplyr::select(date,SP=Daily.Nr)
sunspot.daily<- subset(sunspot.daily, date< ymd("2022-01-01"))
str(sunspot.daily)
# overview daily observed data
sunspot.daily%>% na.omit()%>%ggplot(aes(x=date,y=SP))+
  geom_point(size = 0.01)+
  geom_smooth()+
  ggtitle("Daily Observed Sunspots",
          subtitle = "Recorded by Belgium Royal Observatory, Brussels")+
  labs(x="",y = "Daily Counts")

require(mgcv)
SP_data<- sunspot.daily%>% 
  rownames_to_column()%>% 
  mutate(rwnm=as.numeric(rowname))%>% 
  as_tibble()%>% dplyr::select(date,rwnm,SP)
require(broom)
# fit with 60 basis functions
SP_mdl.60<- gam(data=SP_data,formula = SP~s(rwnm,k=60),method = "REML")
AIC(SP_mdl.60)#[1] 746832.9 # AIC with 60 Basis functions is 752146.5
SP_fit60<-SP_mdl.60%>%augment()%>% dplyr::select(rwnm,SP,SP_fit.60=.fitted)
SP_data.fit60<-SP_data%>%
  left_join(SP_fit60,by= "rwnm")%>%
  dplyr::select(date,SP=SP.x,SP_fit.60)%>%as_tibble()
head(SP_data.fit60,1)
names(SP_data.fit60)
SP_data.fit60%>% ggplot(aes(x=date))+
  geom_line(aes(y= SP_fit.60),col= "red",size =2)+
  geom_point(aes(y=SP),size = 0.01,alpha= 0.05)+
  ggtitle("Sunspot Numbers",
          subtitle =" fiited with 60 gaussian basis functions" )
head(SP_data.fit60)
names(SP_data.fit60)
summary(SP_data.fit60)
#======================
saveRDS(SP_data.fit60, file = "data/SP_dat.rds")
#===================================================
#==============
SP_data<- readRDS("data/SP_dat.rds")
SP_data$SP%>% mean(na.rm=T)#82.2225 on 74509 observations
SP_data$SP_fit.60%>% mean(na.rm=TRUE)#82.2225 on 74509 observations
SP_data$rwnm<-1:NROW(SP_data)# NROW = 7459
# fitting with negbinom
#negbin method taken from url_negbin
url_negbin<-"https://astrostatistics.psu.edu/su07/R/html/mgcv/html/gam.neg.bin.html"
browseURL(url_negbin)

negative.binomial(theta = 2,link="log")
SP_mdl.nbin<- mgcv::gam(data=SP_data,formula = SP~s(rwnm,k=60),family=negbin(1),scale=1)
summary(SP_mdl.nbin)# intercept 4.033414; exp(4.033414)= 56.45331
AIC(SP_mdl.nbin)#[1] 719405.3 which is lower than with gaussian fit
require(broom)
SP_data0<-SP_data%>% na.omit()%>% dplyr::select(-c(SP))
SP_mdl_data<-SP_data0%>% 
  left_join(SP_mdl.nbin%>%
              augment())%>% dplyr::select(date,SP,SP_fit.60,SP_NB=.fitted)
SP_mdl_data%>%
  ggplot(aes(x=date,y=exp(SP_NB)))+
  geom_line(aes(y= exp(SP_NB)),col="red")+
  geom_point(aes(y= SP),size=0.01,alpha = 0.01)+
  ggtitle("Sunspot Daily Observations",
          subtitle = "fitted with negative binomial")+
  labs(x="", y= "daily obs")

