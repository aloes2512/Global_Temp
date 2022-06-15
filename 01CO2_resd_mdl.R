# general mdl
# load detrend CO2-data
CO2_resd<-readRDS("data/CO2_resd.rds")
summary(CO2_resd)
class(CO2_resd)
require(tidyverse)
library(mgcv)
# mutata datetime format to numeric
CO2_resd<-CO2_resd%>% mutate(datetime=as.numeric(datetime))
nms<-CO2_resd$Name%>% unique()
length(nms)#20
# gam model 6 knots
head(CO2_resd,1)
require(broom)
gam.mdl.6 <-function(x) gam(CO2_resd ~ s(datetime, k= 6), data = x)
x<- vector("list",length= 20)
names(x)<-nms
for (nm in nms){
  df<-CO2_resd%>%subset(Name == nm)%>%gam.mdl.6()%>%augment()
  x[[nm]]<-df%>%dplyr::select(CO2_fit=.fitted,datetime)%>%
    mutate(datetime=as.POSIXct(datetime,origin="1970-01-01",tz="UTC"))
 }
x%>%summary
x[["ZEP"]]%>%ggplot(aes(x=datetime,y=CO2_fit))+geom_line()
CO2_dttm<- tibble()
for (nm in nms){
  dfr<- x[[nm]]%>% mutate(Name=nm)
  CO2_dttm<-bind_rows(CO2_dttm,dfr)
}
head(CO2_dttm) 
CO2_trnd.plt<-CO2_dttm%>% ggplot(aes(x=datetime,y=CO2_fit,col=Name))+
  geom_line()
CO2_trnd.plt+ggtitle("CO2-trend 20 sites",
                     subtitle = "fitted: 6 gauss functions")
