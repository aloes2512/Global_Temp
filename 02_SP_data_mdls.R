# Sunspot data WDC-SISO
#data are edited in various formats .txt, .csv which can be found 
#clicking on the graph opened in the browser
#"https://wwwbis.sidc.be/silso/datafiles/SN_d_tot_V2.0.csv"
library(tidyverse)
SP_path<-"http://www.sidc.be/silso"
# link to total SP number as csv
browseURL(SP_path)# opens page for various downloads
SP_total<- read_csv2("https://www.sidc.be/silso/INFO/sndtotcsv.php",col_names=F, na = c("-1","-1.0","NA"))
names(SP_total)<-c("year","month","day","dec_year","SP_daily","SD_daily","n_stations","provisional")
COLnames<-c("year","month","day","dec_year","SP_daily","SD_daily","n_stations","provisional")
SP_total<-SP_total%>% mutate(SD_daily=as.numeric(SD_daily))
require(lubridate)
require(readr)
# sunspot data
#monthly:"/Users/alfloeffler/Google Drive/SN_m_tot_V2.0.csv"
SP_daily_path<- "/Users/alfloeffler/Google Drive/SN_d_tot_V2.0.csv"

sunspot.daily<-read_delim(SP_daily_path,
                         col_names=COLnames,
                         na= c("-1.0","-1","NA"),
                         show_col_types = F,
                         locale =locale(decimal_mark = ",",
                        grouping_mark = "."))%>%
  mutate(date = glue::glue("{year}-{month}-{day}"),
         date = as.Date(date, format = "%Y-%m-%d"),
         date=ymd(date),SP_daily=as.integer(SP_daily))%>%
  dplyr::select(date,SP_daily)%>% mutate(SP_daily=ifelse(SP_daily<=0,NA,SP_daily))
sunspot.daily<- subset(sunspot.daily, date< ymd("2022-01-01"))
str(sunspot.daily)
# overview daily observed data
sunspot.daily%>% na.omit()%>%ggplot(aes(x=date,y=SP_daily))+
  geom_point(size = 0.01)+
  geom_smooth()+
  ggtitle("Daily Observed Sunspots",
          subtitle = "Recorded by Belgium Royal Observatory, Brussels")+
  labs(x="",y = "Daily Counts")

require(mgcv)
SP_data<- sunspot.daily%>% 
  rownames_to_column()%>% 
  mutate(rwnm=as.numeric(rowname))%>% 
  as_tibble()%>% dplyr::select(date,rwnm,SP=SP_daily)
require(broom)
# fit with 60 basis functions
SP_mdl.60<- gam(data=SP_data,formula = SP~s(rwnm,k=60),method = "REML")
AIC(SP_mdl.60)#[1] [1] 632916.7
SP_fit60<-SP_mdl.60%>%augment()%>% dplyr::select(rwnm,SP,SP_fit.60=.fitted)
SP_data.fit60<-SP_data%>%
  left_join(SP_fit60,by= "rwnm")%>%
  dplyr::select(date,SP=SP.x,SP_fit.60)%>%as_tibble()
head(SP_data.fit60,1)
names(SP_data.fit60)
SP_plt<-SP_data.fit60%>% ggplot(aes(x=date))+
  geom_line(aes(y= SP_fit.60),col= "red",size =1)+
  
  ggtitle("Sunspot Numbers",
          subtitle =" fiited with 60 gaussian basis functions" )
SP_plt+geom_point(aes(y=SP),size = 0.01,alpha= 0.07)
head(SP_data.fit60)
names(SP_data.fit60)
summary(SP_data.fit60)
#======================
saveRDS(SP_data.fit60, file = "data/SP_dat.rds")
#===================================================
#==============
SP_data<- readRDS("data/SP_dat.rds")
SP_data$SP%>% mean(na.rm=T)#[1] 97.87664 on 74510 observations
SP_data$SP_fit.60%>% mean(na.rm=TRUE)#82.2225 on 74510 observations
SP_data$rwnm<-1:NROW(SP_data)# NROW = 74510
# fitting with negbinom
#negbin method taken from url_negbin
url_negbin<-"https://astrostatistics.psu.edu/su07/R/html/mgcv/html/gam.neg.bin.html"
browseURL(url_negbin)
require(MASS)
negative.binomial(theta = 2,link="log")
SP_mdl.nbin<- mgcv::gam(data=SP_data,formula = SP~s(rwnm,k=60),family=negbin(1),scale=1)
summary(SP_mdl.nbin)# intercept 4.033414; exp(4.033414)= 56.45331 edf 58.91
AIC(SP_mdl.nbin)#[1] 719405.3 which is lower than with gaussian fit 746832.9 
require(broom)
SP_data0<-SP_data%>% na.omit()%>% dplyr::select(-c(SP))
SP_fit<-SP_mdl.nbin%>%
  augment()
SP_mdl_data<-SP_data0%>% 
  left_join(SP_fit, by= "rwnm")%>% dplyr::select(date,SP,SP_NB=.fitted)
SP_mdl_data%>%
  ggplot(aes(x=date,y=exp(SP_NB)))+
  geom_line(aes(y= exp(SP_NB)),col="red")+
  geom_point(aes(y= SP),size=0.01,alpha = 0.01)+
  ggtitle("Sunspot Daily Observations",
          subtitle = "fitted with negative binomial distribution")+
  labs(x="", y= "daily obs")
ggsave(filename = "figs/SP_daily_NB.smooth.png")
saveRDS(SP_mdl_data, file = "data/SP_data_mdl.rds")
