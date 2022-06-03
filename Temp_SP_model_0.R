#Temp SP model
# Temp timeseries: Hadley central England dataset
require(tidyverse)
require(mgcViz)
data_path<- "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds"
#Temp data have been formatted and fitted "data/Temp_list.rds"
Temp_list<- readRDS("data/Temp_list.rds")
Temp_list%>% summary()
Temp_dat.72<-Temp_list$Temp.72
## Sunspot data WDC-SISO
Silso_path<-"http://www.sidc.be/silso"
browseURL(Silso_path)
SP_path<- "/Users/alfloeffler/Desktop/Klima_Energiewende/Daten/SN_d_tot_V2.0.csv"
SP_raw<- read_csv2(SP_path,col_names = T)
head(SP_raw,10)
# Sunspot data formatted and saved in "SP_data_mdl.rds"
SP_dat<-readRDS("data/SP_data_mdl.rds")
SP_dat%>% ggplot(aes(x=date,y=SP_NB))+
  geom_line()
# join SP Temp- data
SP_Temp_data<-SP_dat%>% 
  left_join(Temp_dat.72,by=c("date"="datum"))%>%dplyr::select(date,Temp,SP,SP_NB,Temp.fit)
SP_Temp_data%>% ggplot(aes(y= Temp.fit))+
  #geom_point(aes(x=SP_NB),size= 0.01,alpha= 0.1)+
  geom_smooth(data=SP_Temp_data,aes(x=SP_NB,y= Temp.fit),formula=y ~ s(x,k=5))
# plot with Temp.18
SP_dat%>% left_join(Temp_list$Temp.72,by=c("date"="datum"))%>%
  ggplot()+
  geom_smooth(aes(x=log(SP),y= Temp),formula=y ~ s(x))
SP_dat%>% left_join(Temp_list$Temp.72,by=c("date"="datum"))%>%
  ggplot()+
  geom_point(aes(x=log(SP),y = Temp.fit ),size = 0.05,alpha = 0.5)
SP_dat%>% left_join(Temp_list$Temp.72,by=c("date"="datum"))%>%
  subset(date>as.Date("2000-01-01")&date<as.Date("2015-12-31"))%>%
  ggplot()+
  geom_point(aes(x=log(SP),y = Temp.fit ),size = 0.1,alpha = 0.5)
SP_dat%>% left_join(Temp_list$Temp.72,by=c("date"="datum"))%>%
  subset(date>as.Date("2010-01-01")&date<as.Date("2015-12-31"))%>%
  ggplot()+
  geom_point(aes(x=log(SP),y = Temp.fit ),size = 0.1,alpha = 0.5)
SP_dat%>% left_join(Temp_list$Temp.72,by=c("date"="datum"))%>%
  subset(date>as.Date("2000-01-01")&date<as.Date("2005-12-31"))%>%
  ggplot()+
  geom_point(aes(x=log(SP),y = Temp.fit ),size = 0.1,alpha = 0.5)


#"https://mfasiolo.github.io/mgcViz/articles/mgcviz.html"
url_mgcViz<-"https://mfasiolo.github.io/mgcViz/articles/mgcviz.html"
browseURL(url_mgcViz)
# b <- gam(y ~ s(x1) + s(x2) + x3, data = dat, method = "REML")
library(mgcViz)
head(SP_Temp_data)
Temp_SP.mdl<- gam( Temp.fit ~ date+s(as.numeric(date), k=36)+s(exp(SP_NB),k= 36),
                                  data= SP_Temp_data,
                                 method ="REML")
dfr.fit<-Temp_SP.mdl$model
dfr.fit$fit<-Temp_SP.mdl$fitted.values

dfr.fit%>% summary()
ggplot(dfr.fit,aes(x=date,y=fit))+#geom_line()+
  geom_line(aes(x=date,y=fit))
Temp_SP.mdl<- getViz(Temp_SP.mdl)
plot(sm(Temp_SP.mdl,1))
plot(sm(Temp_SP.mdl,2))
Temp_SP_dat.mdl<- gam( Temp ~ date+s(as.numeric(date), k=36)+s(SP,k= 36),
                   data= SP_Temp_data,
                   method ="REML")
Temp_SP_dat.mdl<-getViz(Temp_SP_dat.mdl)
dtfr.dat<-Temp_SP_dat.mdl$model
SP_Temp_data%>%head()
dtfr.dat$fit<-Temp_SP_dat.mdl$fitted.values
dtfr.dat%>% ggplot(aes(x=date,y=fit))+geom_line()
temp.averg<-mean(dtfr.dat$fit)
dtfr.dat<-dtfr.dat%>% mutate(Tmp.scl=fit/temp.averg)
dtfr.dat%>% ggplot(aes(x=date,y=Tmp.scl))+geom_point(size= 0.1)
SP.averg<- dtfr.dat$SP%>% mean(na.rm = TRUE)#82.2225
dtfr.dat<-dtfr.dat%>% mutate(SP.scl= SP/SP.averg)
dtfr.dat%>% ggplot(aes(x=date,y=Tmp.scl))+geom_line(size =2)+
  geom_point(aes(x=date, y= SP.scl),size = 0.05,col = "red",alpha= 0.2)
plt1<-plot(sm(Temp_SP_dat.mdl,1))
plt2<-plot(sm(Temp_SP_dat.mdl,2))
plt1+l_fitLine(col="red")+l_ciLine(col = "blue",linetype = 3)+
  labs(x= "day difference to 1970-01-01")
plot(pterm(Temp_SP_dat.mdl,select = 1))+l_ciLine()+l_fitLine()
# subsets of data
SP_Temp_data%>% head(2)
SP_Temp_data%>% ggplot(aes(x=date))+
  geom_line(aes(y=SP_NB), size = 0.2,col= "red")+
  geom_line(aes(y=Temp.fit/3),size= 0.2)
SP_Temp_data%>%  ggplot(aes(x=date))+
  geom_line(aes(y=Temp.fit,col="month"))+
  geom_line(aes(y=SP_NB))
  