# astrophysical calculations of earth orbit by laskar
reference<-"https://www.aanda.org/articles/aa/pdf/2011/08/aa16836-11.pdf"
browseURL(reference)
# raw data:https://vizier.cds.unistra.fr/viz-bin/VizieR-4
library(tidyverse)
# data downloaded to milankovich.data.txt
dtable<-read.table("~/downloads/milankovitch.data.txt",sep ="",
                   skip = 7, header = T)%>% as_tibble()
head(dtable)
names(dtable)
range(dtable$perihelion)
mean(dtable$perihelion)
require(mgcViz)
ggplot(dtable,aes(x = time,y=insolation))+geom_line()+
  ggtitle("Astrophysical model :Insolation close to Arctic Circle",
          subtitle = "daily mean TOA solar flux at 65N summer solstice")+
  labs(y= "insolation W/m2",x= "time before now kyr")
geo_mdl<- gam(data = dtable,
              formula= insolation ~ s(time, k=700),
              method = "REML")
AIC(geo_mdl1000)# -5575.155
AIC(geo_mdl700)# -4418.203
geo_mdl700<- getViz(geo_mdl)
o<- plot(sm(geo_mdl700,1))
o+l_fitLine(col="red")+l_ciLine(col = "blue",linetype = 3)
# global insolation
ggplot(dtable,aes(x = time,y=global.insolation))+
  geom_line()+
  ggtitle("Astrophysical model :Global Insolation")+
    labs(x= "time before now kyr",y = "Average W/m2")
# subset of 15000 recent years
recent.tbl<-dtable%>% tail(60) %>% head(15)
recent_mdl<-gam(data = recent.tbl,
                formula= insolation ~ s(time, k=4),
                method = "REML")
NROW(recent.tbl)
recent.tbl<-recent.tbl%>% 
  mutate(yr=time*1000+2000)%>% 
  dplyr::select(yr,time,insolation,global.insolation)
require(broom)
recent_mdl%>% augment()%>% ggplot(aes(x=time,y=.fitted))+
  geom_line()+
  geom_point(aes(y= insolation))
new_dat<- tibble(time=-9:5+0.5)
NROW(new_dat)
predict(recent_mdl,newdata = new_dat)
# use forecast for prediction