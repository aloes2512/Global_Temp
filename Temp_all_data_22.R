# Hadley Dataset
library(tidyverse)
# for renewal
centr_engl<-"https://www.metoffice.gov.uk/hadobs/hadcet/cetdl1772on.dat"
browseURL(centr_engl)
CET_data<- url(centr_engl)
dt<-read.table(CET_data, sep = "", skip = 0, header = FALSE,
           fill = TRUE, na.string = c(-99.99, -99.9,-999.00))
colnames(dt)<- c("Year","Day",month.abb)
head(dt)
dat<-dt%>%pivot_longer(cols = -c(Year,Day),names_to = "month",values_to = "Temp10")

dat<-dat%>% na.omit()%>% mutate(Temp= Temp10/10)
require(lubridate)
summary(dat)
dat<-dat%>% mutate(datum=paste0(Year,"-",month,"-",Day),datum=ymd(datum),month= as_factor (month))
dat<-dat%>% arrange(datum)%>% dplyr::select(datum,month,Temp)
# cleaned and formatted Temp data
saveRDS(dat,file = "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds")
saveRDS(dat, file="data/Central_England_Temp.rds")
#===========================
# if data have been previously downloaded and formatted
data_path<- "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds"
dat<-readRDS(data_path)
# Overview Trends
ylab <- expression(Temperature~(~degree*C))
dat%>% ggplot(aes(datum,Temp))+
  #geom_point(size=0.01,alpha= 0.01)+
  geom_smooth()+
  ggtitle("Central England Daily Mean Temperatures",
          subtitle = "Instrumental Measurements published by UK Metoffice")+
  labs(x="",y= ylab)
head(dat)
mnth_dat<-dat%>% group_by(Year,month)%>% summarise(month_average= mean(Temp10,na.rm = TRUE)/10)
mnth_dat<-mnth_dat%>% 
  mutate(mn_datum= paste0(Year,"-",month,"-",15)%>% ymd(), month=as_factor(month))%>% 
  na.omit()

summary(mnth_dat)
mnth_dat%>% subset(Year > 1900& Year<2022)%>% # data from 2022 are only from Jan & Feb bias smoothing
  ggplot(aes(x=mn_datum))+
  geom_point(aes(y= month_average), size = 0.01)+
  stat_smooth(aes(y= month_average),span = 0.01,col = "blue")
yr_dat<- dat%>% group_by(Year)%>% summarise(yr_average= mean(Temp10,na.rm= TRUE)/10)
yr_dat<-yr_dat %>% subset(Year< 2022)
str(yr_dat)
yr_dat%>%  ggplot(aes(x= Year))+
  geom_point(aes(y= yr_average))+
  geom_smooth(aes(y= yr_average),n=5)
# Tempdat
require(xts)
Temp_dat<-readRDS("~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds")
Temp_dat<-Temp_dat%>% arrange(datum)
head(Temp_dat)
# other formats
Temp_xts<- xts(order.by = Temp_dat$datum,x= Temp_dat%>% dplyr::select(-datum))
Temp_yr_mdl<- mgcv::gam(yr_average ~ s(Year),data= subset(yr_dat,Year<2022), method = "ML")
require(stats)
preplot(Temp_yr_mdl)
require(mgcViz)
Temp.yr_b<- getViz(Temp_yr_mdl)
Temp.yr_o<-plot(sm(Temp.yr_b,1))
preplot(Temp_yr_mdl)
listLayers(Temp.yr_o)
str(Temp.yr_o)
Temp.yr_o$ggObj$data%>% head()
ylab1 <- expression(Temp-deviation~(degree*C))
centrl_engl_Temp<-Temp.yr_o+l_fitLine(colour = "red")+
  l_ciLine(linetype= 2,col = "blue")+
  l_points(size =0.8)+
  ggtitle("Central England 
Deviation of Annual Mean Temperatures",
          subtitle = "Timeseries of 90885  Instrumental Mesurements")+
  labs( x="",y=ylab1)
preplot(centrl_engl_Temp)
ggsave(centrl_engl_Temp,filename = "~/Desktop/Klima_Energiewende/Figs/Centrl_Engl_Temp.png")
