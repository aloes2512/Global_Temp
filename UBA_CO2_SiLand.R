library(tidyverse)
library(lubridate)
# Import Schauinsland CO2 time series
# UBA Schauinsland 1h Daten N 47.911944, E 7.898611 alt =1284m
UB2019SCO2_SiL<- read_delim("~/Desktop/Klima_Energiewende/CO2_Schauinsland/UB2019SCO2_inv1SMW_20220414.csv",
                            delim = ";",quote = "'", escape_double = FALSE, trim_ws = TRUE)%>% subset(Wert > 0)
summary(UB2019SCO2_SiL)
head(UB2019SCO2_SiL,2)
UB2019SCO2_SiL<- UB2019SCO2_SiL%>% mutate(datetime= paste(Datum,Uhrzeit),
                                          datetime = ymd_hms(datetime),datetime=floor_date(datetime,unit = "hour"))
UB2019SCO2_SiL%>%ggplot(aes(x=datetime,y=Wert))+
  geom_point(size = 0.1,alpha=0.1)
# General function
File<- paste0("UB",yr,"SCO2_inv1SMW_20220414.csv")
SiL_read<- function(yr){
  File<- paste0("UB",yr,"SCO2_inv1SMW_20220414",".csv")
  path<- "~/Desktop/Klima_Energiewende/Daten/CO2_STD_SC_1972-2019"
  dfr<-read_delim(file.path(path,File),
             delim = ";",quote = "'", escape_double = FALSE, trim_ws = TRUE)%>% subset(Wert > 0)%>%
        mutate(datetime= paste(Datum,Uhrzeit),
             datetime = ymd_hms(datetime),datetime=floor_date(datetime,unit = "hour"))
        return(dfr)
  }

yr<-1972
dx<-SiL_read(yr=1972)
summary(dx)
plt<-dx%>%ggplot(aes(x=datetime,y=Wert))+
  ggtitle("CO2-Immission Schauinsland",
          subtitle = "lat=N47.911944,long=E7.898611,alt=1284m")+
  labs(x="",y="CO2[ppm]")
plt+geom_point(size=0.1,alpha =0.1)
# all data
SiL_file.list<-c(1972:1978,1980:2019)
SiLand<-tibble()
for (yr in SiL_file.list) {
  tbl<- SiL_read(yr)
  SiLand<-bind_rows(SiLand,tbl)
}
summary(SiLand)# 1972 bis 2019 (ohne 1979)
var(SiLand$Wert)%>%sqrt#26.36597 ppm
mean(SiLand$Wert)#366.2391
lm(SiLand$Wert~SiLand$datetime)#Intercept 3.183e+02 ;slope=5.800e-08  

saveRDS(SiLand,file="~/projects/Global_Temp/data/SiLand.rds")
CO2_data.lst<- readRDS("~/projects/Global_Temp/data/NOAA_data.rds")
CO2_data.lst$DEUB004%>%summary()
SiLand%>%summary()
