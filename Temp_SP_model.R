#Temp SP model
# Temp timeseries: Hadley central England dataset
require(tidyverse)
data_path<- "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds"
#Temp data have been formatted and fitted "data/Temp_list.rds"
Temp_list<- readRDS("data/Temp_list.rds")
Temp_dat<-Temp_list$Temp.72
## Sunspot data WDC-SISO
SP_path<-"http://www.sidc.be/silso"
browseURL(SP_path)
dat_path<- "/Users/alfloeffler/Desktop/Klima_Energiewende/Daten/SN_d_tot_V2.0.csv"
# Sunspot data formatted and saved in "SP_data_mdl.rds"
SP_dat<-readRDS("data/SP_data_mdl.rds")
# join SP Temp- data
SP_Temp_data<-SP_dat%>% 
  left_join(Temp_dat,by=c("date"="datum"))#%>%dplyr::select(datum,Temp,SP,Temp_chg)
SP_Temp_data%>% ggplot(aes(x= Temp.fit))+
  geom_point(aes(y=SP),size= 0.01,alpha= 0.1)+
  geom_smooth(data=SP_Temp_data,aes(y=SP,x= Temp.fit),formula=y ~ s(x,k=5))
# qqplots 
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
dfr.fit$lin.pred
Temp_SP.mdl$store$termsFit%>% NROW
ggplot(dfr.fit,aes(x=date,y=fit))+#geom_line()+
  geom_line(aes(x=date,y=lin.pred))
Temp_SP.mdl<- getViz(Temp_SP.mdl)
plot(sm(Temp_SP.mdl,1))
plot(sm(Temp_SP.mdl,2))
Temp_SP_dat.mdl<- gam( Temp ~ date+s(as.numeric(date), k=36)+s(SP,k= 36),
                   data= SP_Temp_data,
                   method ="REML")
Temp_SP_dat.mdl<-getViz(Temp_SP_dat.mdl)
dtfr.dat<-Temp_SP_dat.mdl$model
dtfr.dat$fit<-Temp_SP_dat.mdl$fitted.values
dtfr%>% ggplot(aes(x=date,y=fit))+geom_line()
plt1<-plot(sm(Temp_SP_dat.mdl,1))
plt2<-plot(sm(Temp_SP_dat.mdl,2))
plt1+l_fitLine(col="red")+l_ciLine(col = "blue",linetype = 3)+
  labs(x= "day difference to 1970-01-01")
plot(pterm(Temp_SP_dat.mdl,select = 1))+l_ciLine()+l_fitLine()
# subsets of data
SP_Temp_data%>% head(2)
SP_Temp_data%>% subset(month== "Feb")%>% ggplot(aes(x=exp(SP_NB)))+
  geom_line(aes(y=Temp.fit))#+
  