# load detrend CO2-data
CO2_resd<-readRDS("data/CO2_resd.rds")
summary(CO2_resd)
require(tidyverse)
CO2_resd_MLO <- CO2_resd %>% subset(Name == "MLO")
CO2_resd_MLO %>% head(2)
CO2_MLO.plt<-CO2_resd_MLO %>% ggplot(aes(x=datetime,y= CO2_resd))+
  geom_line(size=0.2,col ="red")+
  ggtitle("CO2 concentration @ MLO",
          subtitle= "Deviation from linear trend")
CO2_MLO.plt+  geom_line(aes(x=datetime,y= CO2-CO2_resd),size =0.2)+
  labs(x ="",y="CO2 [ppm]")
# fit gam model
library(mgcv)
CO2_resd_MLO<-CO2_resd_MLO%>% mutate(dttm=as.numeric(datetime))
head(CO2_resd_MLO)
NROW(CO2_resd_MLO)
# visualise
CO2_MLO.plt<-CO2_resd_MLO%>% ggplot(aes(x=datetime,y= CO2))+
  geom_smooth(data=CO2_resd_MLO,
              method = lm,
              formula= y~ x ,
              mapping=aes(x=datetime,y=CO2),col="red")+
  labs(x="",y="CO2 [ppm]")
CO2_MLO.plt+ ggtitle("Linear Trend CO2-Concentrations
                     @ Mauna Loa")
# Analyse residuals
require(broom)
# trend MLO data fitted with 6 gauss function
CO2_gam.mdl.6 <- gam(CO2 ~ s(dttm, k= 6), data = CO2_resd_MLO)
CO2_gam_fit.6<-CO2_gam.mdl.6%>%
  augment%>%
  dplyr::select(dttm,CO2,CO2_resd=.resid,CO2_fit=.fitted)%>%
  mutate(datetime=as.POSIXct(dttm,origin="1970-01-01",tz="UTC"))
CO2_trend.plt<-CO2_MLO.plt+geom_line(data = CO2_gam_fit.6,
                      mapping = aes(x=datetime,y=CO2_fit),col="black" )+
            ggtitle("Trend CO2-Concentrations
                     @ Mauna Loa",
                     subtitle = ("Linear (red), 6 gauss fnct (black)"))
# Residuals of 6 gauss trend
head(CO2_gam_fit.6)
#eliminating outliers: abs(CO2_resd)> 8
CO2_ft.6.cln<-CO2_gam_fit.6%>% 
  subset(abs(CO2_resd)<8)
CO2_ft.6.cln%>%  ggplot(aes(x= datetime,y=CO2_resd))+
  geom_point(size = 0.2)
# fitting 
CO2_ft.6.cln%>% tail()
CO2_gam.mdl.108 <- gam(CO2_resd ~ s(dttm, k= 108), data = CO2_ft.6.cln)
CO2_MLO_yr.plt<-CO2_gam.mdl.108%>%augment()%>%
  mutate(datetime=as.POSIXct(dttm,origin ="1970-01-01",tz ="UTC"))%>%
  ggplot(aes(x=datetime,y = .fitted))+
  geom_line()
CO2_MLO_yr.plt+ggtitle("Mean Yearly deviations of
CO2-Imission from long trend",
  subtitle = "Mauna Loa time series")+
  labs(x="",y= "CO2 [ppm]")
# combine longterm model with yearly model  
