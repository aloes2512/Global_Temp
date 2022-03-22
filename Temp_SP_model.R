#Temp SP model
# Temp timeseries: Hadley central england dataset
require(tidyverse)
data_path<- "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds"
Temp_dat<-readRDS(data_path)%>% 
  mutate(Temp= Temp10/10,Temp_chg=lead(Temp)-Temp)%>% dplyr::select(-Temp10)
summary(Temp_dat)
head(Temp_dat)
# sunspot daily dat
## Sunspot data WDC-SISO
SP_path<-"http://www.sidc.be/silso"
browseURL(SP_path)
dat_path<- "/Users/alfloeffler/Desktop/Klima_Energiewende/Daten/SN_d_tot_V2.0.csv"
require(lubridate)
raw_SP_data<-read.csv2(dat_path)
head(raw_SP_data,1)

sunspot.daily<-read.csv2(dat_path)%>% 
  dplyr::select(Year,Month,Day,Daily.Nr)%>% 
  mutate(datum=paste0(Year,"-",Month,"-",Day),datum=ymd(datum))
sunspot.daily<- subset(sunspot.daily, datum< ymd("2022-01-01"))
sunspot.daily<-sunspot.daily%>% dplyr::select(datum,"SP"= Daily.Nr)
head(sunspot.daily)
summary(sunspot.daily)
NROW(sunspot.daily)#[1] 74509
sunspot.daily<-sunspot.daily%>% na.omit()
# join SP Temp- data
SP_Temp_data<-sunspot.daily%>% 
  left_join(Temp_dat)%>%dplyr::select(datum,Temp,SP,Temp_chg)
SP_Temp_data%>% ggplot(aes(x= SP))+
  geom_point(aes(y=Temp_chg),size= 0.01,alpha= 0.1)+
  geom_smooth(data=SP_Temp_data,aes(x=SP,y= Temp_chg),formula=y ~ x)
