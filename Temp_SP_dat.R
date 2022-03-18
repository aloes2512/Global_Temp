# Temp timeseries: Hadley central england dataset
require(tidyverse)
data_path<- "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds"
Temp_dat<-readRDS(data_path)%>% 
  mutate(Temp= Temp10/10)%>% dplyr::select(-Temp10)
summary(Temp_dat)
head(Temp_dat)
# Sunspot data WDC-SISO
SP_path<-"http://www.sidc.be/silso"
browseURL(SP_path)# opens page for various downloads
# EISN_current.csv download and moved to
SP.2022_path <-"~/Desktop/Klima_Energiewende/Daten/EISN_current.csv"
SP_2022 <-read.csv(SP.2022_path,header = FALSE)
dat_path<- "/Users/alfloeffler/Desktop/Klima_Energiewende/Daten/SN_d_tot_V2.0.csv"
#variables of "SN_d_tot_V2.0.csv":
#year, month, day, decimal.year, SNvalue , SNerror, Nb_observations
# names(sunspot.daily)
require(lubridate)
sunspot.daily<-read.csv2(dat_path)%>% 
  dplyr::select(Year,Month,Day,Daily.Nr)%>% 
  mutate(datum=paste0(Year,"-",Month,"-",Day),datum=ymd(datum))
sunspot.daily<- subset(sunspot.daily, datum< ymd("2022-01-01"))
sunspot.daily<-sunspot.daily%>% dplyr::select(datum,"SP"= Daily.Nr)
str(sunspot.daily)
Temp_dat<-Temp_dat%>% dplyr::select(datum,Temp)
str(Temp_dat)
Temp_SP_dat<-Temp_dat%>% left_join(sunspot.daily)
summary(Temp_SP_dat)
Temp_SP_dat<-Temp_SP_dat%>% 
  na.omit()%>%mutate(Datum=datum,datum=as.numeric(datum),SP=as.double(SP))
head(Temp_SP_dat,1)
require(mgcv)
require(mgcViz)
Temp_SP_mdl<- gam(data=Temp_SP_dat,formula=Temp~ s(datum,k=12)+s(SP,k=12), method="REML")
summary(Temp_SP_mdl)
Temp_SP_mdl<- getViz(Temp_SP_mdl)
summary(Temp_SP_mdl)
plot(sm(Temp_SP_mdl,2))
o <- plot( sm(Temp_SP_mdl,1) )
o+l_fitLine(colour = "red") + 
  l_ciLine(mul = 5, colour = "blue", linetype = 2) 
plot(sm(Temp_SP_mdl,2))
require(broom)
Temp_SP_fit<-Temp_SP_mdl%>%augment()%>%
  dplyr::select(Temp,Tmp_fit=.fitted,SP,Tmp_se=.se.fit)
Temp_SP_fit$Datum<-Temp_SP_dat$Datum

#====================
Sunspots_daily<-Temp_SP_fit%>% ggplot(aes(x=Datum))+
  geom_smooth(aes(y=SP)) +
  ggtitle("Mean Daily Sunspots (SP)",
          subtitle = "Source:https://wwwbis.sidc.be/silso/")+
  labs(x= "", y= "Daily-Counted-Sunspots")
ggsave(Sunspots_daily,"Sunspots_daily.png")
#===============
Temp_SP_plt<-Temp_SP_fit%>%ggplot(aes(x=SP))+
  geom_point(aes(y= Tmp_fit),size= 0.1)+
  geom_smooth(aes(y= Tmp_fit),col= "red",linetype=2,size=0.5)+
  ggtitle("Mean-Daily-Temperature~ Sunspots(counts)
  Central-England")+
  labs(x=" Sunspots",y= expression(Temperature~(degree*C)))
Temp_SP_plt+geom_smooth(data=Temp_SP_fit,
                        mapping=aes(x=SP,y=Tmp_fit),
                        method = "lm",
                        formula = y ~ x,
                        col="red")
####==============
Temp_SP_viz<-function(dfr,x=Datum,y1=Tmp_fit,y2=SP){
  dfr<-dfr%>%dplyr::select(Datum,Tmp_fit,SP)
  from<-range(dfr$Datum)[1]
  to<-range(dfr$Datum)[2]
  xab<-lm(dfr,formula=Tmp_fit~SP)$coefficients
  plt<-dfr%>%ggplot(aes(x=SP))+
    geom_point(aes(y= Tmp_fit),size= 0.1)+
    geom_smooth(aes(y= Tmp_fit),col= "red",linetype=2,size=0.5)+
    geom_abline(intercept=xab[1],slope = xab[2],col = "red")+
    ggtitle("Mean-Daily-Temperature~ Sunspots(counts)
  Central-England",
            subtitle=paste(from,to))+
    labs(x=" Sunspots",y= expression(Temperature~(degree*C)))
  return(plt)
}
# select by date
plt<-Temp_SP_viz(Temp_SP_fit)
Temp_SP_select_l<-Temp_SP_fit%>%subset(Datum<ymd("1900-01-01"))
Temp_SP_viz(Temp_SP_select_l)
Temp_SP_select_h<-Temp_SP_fit%>%subset(Datum>=ymd("2010-01-01"))
Temp_SP_viz(Temp_SP_fit%>%subset(Datum>=ymd("1845-01-01")&
              Datum<=ymd("1854-12-31")))
Temp_SP_viz(Temp_SP_fit%>%subset(Datum>=ymd("1895-01-01")&
                                   Datum<=ymd("2004-12-31")))
