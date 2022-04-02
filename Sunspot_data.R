# Sunspot numbers
library(tidyverse)
library(mgcv)
library(mgcViz)
sunspot.month_tbl<-as.data.frame(sunspot.month)
N<-NROW(sunspot.month)# 3177
sunspot.month_index<-seq(as.Date("1749/1/1"), by = "month", length.out = N)
last(sunspot.month_index)#"2013-09-01"
sunspot.month_tbl$datum<-sunspot.month_index
names(sunspot.month_tbl)<- c("SP","datum")# x datum
summary(sunspot.month_tbl)
sunspot.month_tbl%>% ggplot()+
  geom_smooth(aes(x= datum,y=SP))
summary(sunspot.month_tbl)
SP<- sunspot.month_tbl$SP%>% as_vector()
Datum<- sunspot.month_tbl$datum %>% as_vector()
length(Datum)
length(SP)
month_dat<- list(sunspt=SP,datum=as.numeric(Datum))
month.mdl<-gam(dat=month_dat,formula = SP ~ s(datum,k=8))
require(broom)
Month_fit<-month.mdl%>% augment()%>% dplyr::select(SP,SP_fit=.fitted)
Month_fit$datum<-Datum
SP_plt<-Month_fit%>% ggplot(aes(x=datum,y=SP))+
  geom_line()+
  geom_smooth()+
  geom_line(aes(x=datum,y=SP_fit),col ="red")

# daily figures
require(lubridate)
dat_path<- "/Users/alfloeffler/Desktop/Klima_Energiewende/Daten/SN_d_tot_V2.0.csv"
sunspot.daily<-read.csv2(dat_path)%>% 
  dplyr::select(Year,Month,Day,Daily.Nr)%>% 
  mutate(datum=paste0(Year,"-",Month,"-",Day),datum=ymd(datum))
sunspot.daily<- subset(sunspot.daily, datum< ymd("2022-01-01"))
sunspot.daily%>% na.omit()%>%ggplot(aes(x=datum,y=Daily.Nr))+
  geom_point(size = 0.01)+
  geom_smooth()+
  ggtitle("Daily Observed Sunspots",
          subtitle = "Recorded by Belgium Royal Observatory, Brussels")+
  labs(x="",y = "Daily Counts")
summary(sunspot.daily)
ymd("2022-01-01")-ymd("1818-01-02")# 74509 days
ymd("2022-01-01")-ymd("1818-01-02")/(19*365)
NROW(sunspot.daily)# 74509
SPmax<-max(sunspot.daily$Daily.Nr,na.rm = TRUE)
sunspot.daily<-sunspot.daily%>% mutate(prz= Daily.Nr*100/SPmax)
x <- tibble (index= 1:NROW(sunspot.daily),
             Zahl = sunspot.daily$Daily.Nr,
             prz = sunspot.daily$prz)
last(sunspot.daily)#2022-02-28
summary(x)
sunspot_xts<- xts(order.by =  ymd(sunspot.daily$datum),x)
last(sunspot_xts)

require(time)
library(stats)
library(tseries)
datum<-sunspot.month%>% as.xts()%>% index()
sunspotmonth<- tibble(datum =sunspot.month%>% as.xts()%>% index(),
                     Number=sunspot.month )

sunspotmonth%>% ggplot(aes(x=datum,y=Number))+
  geom_point(size= 0.2)+
  geom_smooth()
# combine with Hadley Temp data
data_path<- "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds"
dat<-readRDS(data_path)
require(xts)
require(tidyverse)
head(dat)
tail(dat)
NROW(dat)#91311
last(dat)#2021    31 Dec      129 2021-12-31
ymd("2021-12-31")-ymd("1772-01-01")# Date difference91310 days
x<- dat$Temp10
dat_xts<- xts(x,order.by = dat$datum)
first(sunspot_xts)#1818-01-01
my_dat<-merge(dat_xts,sunspot_xts)["1818-01-02/"]
index(my_dat)
first(my_dat)
my_tbl<-as_tibble(coredata(my_dat)) %>% mutate(Temp= dat_xts/10)%>% dplyr::select(Temp,przSP=prz)
my_tbl$datum<- index(my_dat)
length(index(my_dat))
require(mgcv)
my_tbl%>%summary()
my_tbl%>% head()
length(my_tbl$Temp)
length(my_tbl$przSP)
lenght(my_tbl$datum)# ???

my_tbl%>% ggplot()+
  geom_smooth(data=my_tbl,method=gam,formula= Temp~ s(x, k=12),aes(x= index(my_dat),y=Temp),col = "red")+
  geom_smooth(data= my_tbl,aes(x=datum,y= przSP))
