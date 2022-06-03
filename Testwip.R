loc_LMP<-station_sites %>% subset(Code=="LMP")
summary(CO2_data.lst)
require(tidyverse)
CO2_data.lst$LMP<-CO2_data.lst$LMP%>% mutate(lon=loc_LMP[[5]],
                           lat=loc_LMP[[4]],
                           name=loc_LMP[[2]],
                           alt=loc_LMP[[6]],
                           site= "LMP")

saveRDS(CO2_data.lst,file="~/projects/Global_Temp/data/NOAA_data.rds")
dat<-CO2_data.lst$LMP
dat<-dat%>% mutate(datetime=format(datetime,"%Y-%m-%d %H:%M:%S"))
datetm<-dat$datetime
CO2<-dat$CO2
lm(CO2~datetm)
