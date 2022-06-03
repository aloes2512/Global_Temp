#CO2 Negative binomial fitting
CO2_data.lst<-readRDS(file.path("data","NOAA_data.rds"))
length(CO2_data.lst)#19
names(CO2_data.lst)# 19 sites
summary(CO2_data.lst)
# example MLO
CO2_data.lst[[1]]%>% summary()#MLO
MLOdat<-CO2_data.lst[[1]]
MLOdat<-MLOdat%>% mutate(dttm=as.numeric(datetime))
head(MLOdat,1)
MLOdat%>%ggplot(aes(x=datetime,y=CO2))+
  geom_point(size= 0.2)

#example 12 BRW
CO2_data.lst[12]%>%summary()
CO2_data.lst[[12]]%>% summary()
CO2_data.lst[[12]]$alt%>% length()#375825
CO2_data.lst[[12]]%>% str()
BRWdat<-CO2_data.lst[[12]]
BRWdat%>%ggplot(aes(x=datetime,y=CO2))+
  geom_point(size= 0.2)

# NB example MLO
negative.binomial(theta = 2,link="log")
CO2_mdl.nbin<- mgcv::gam(data=MLOdat,formula = CO2~s(dttm,k=160),family=negbin(1),scale=1)
summary(CO2_mdl.nbin)# intercept 5.9119; exp(4.033414)= 369.4074 edf 1
AIC(CO2_mdl.nbin)#[1] 41870.87
CO2_nbin.fit<-CO2_mdl.nbin%>%augment()%>%
  mutate(CO2_fit= exp(.fitted),datetime=as.POSIXct(dttm,origin="1970-01-01",tz = "UTC"))%>%
  dplyr::select(datetime,CO2,CO2_fit)
head(CO2_nbin.fit,1)
CO2_nbin.fit%>% ggplot(aes(x= datetime, y= CO2_fit))+
  geom_point()
# NB example BRW
BRWdat<-BRWdat%>% mutate(dttm= as.numeric(datetime))
negative.binomial(theta = 2,link="log")
CO2_mdl.nbin<- mgcv::gam(data=BRWdat,formula = CO2~s(dttm,k=100),family=negbin(1),scale=1)
summary(CO2_mdl.nbin)
