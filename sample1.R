mlodat<-MLOdat%>% subset(datetime>ymd("1980-01-01")&datetime<ymd("1980-12-31"))
negative.binomial(theta = 2,link="log")
CO2_sm.nbin<- mgcv::gam(data=mlodat,formula = CO2~s(dttm,k=12),family=negbin(1),scale=1)
summary(CO2_sm.nbin)# intercept 5.9119; exp(4.033414)= 369.4074 edf 1
AIC(CO2_sm.nbin)#[1] 865
CO2_sm.fit<-CO2_sm.nbin%>%augment()%>%
  mutate(CO2_fit= exp(.fitted),datetime=as.POSIXct(dttm,origin="1970-01-01",tz = "UTC"))%>%
  dplyr::select(datetime,CO2,CO2_fit)
head(CO2_sm.fit,1)
CO2_sm.fit%>% ggplot(aes(x= datetime, y= CO2_fit))+
  geom_point()
