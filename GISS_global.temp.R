giss_url<-"https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.txt"
browseURL(giss_url)
# GLOBAL Land-Ocean Temperature Index in 0.01 degrees Celsius   base period: 1951-1980
library(readr)
library(tidyverse)
library(lubridate)
GISS_data<-read_table(giss_url,skip=7,col_names=TRUE)%>%
  dplyr::select(1:13)%>% pivot_longer(cols=-Year,names_to = "month",values_to = "Temp.100")
NROW(GISS_data)# 1860
datum<- seq(from= ymd("1880-01-15"), by = "1 month", length=1860)
GISS_data$datum<-datum
GISS_data<-GISS_data%>% mutate(Temp.100= as.numeric(Temp.100))
GISS_data%>% ggplot(aes(x=datum, y= Temp.100))+
  geom_line()+geom_smooth()+
  ggtitle("Global Monthly - Mean Temperature",
  subtitle = "Deviation(K*100) from mean 1951-1980, data source= NASA-GSFC")+
  labs(x="", y= "Temp.Δ [0.01 °C]")
GISS_data$Temp.100%>% is.na(.)%>% sum()# 145 values missing
library(mgcv)
GISS_mdl<-mgcv::gam(Temp.100 ~ s(as.numeric(datum),k=20)+1,data=GISS_data,method= "REML")
