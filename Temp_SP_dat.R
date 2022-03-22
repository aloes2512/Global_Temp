# Temp time series: Hadley central england dataset
require(tidyverse)
Temp_dat_path<- "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds"

dat_path<- "~/Desktop/Klima_Energiewende/Daten"
Temp_dat<-readRDS(Temp_dat_path)%>% 
  mutate(Temp= Temp10/10)%>% dplyr::select(-Temp10)
summary(Temp_dat)
head(Temp_dat,1)#Year   Day month datum       Temp
                #1772     1 Jan   1772-01-01   3.2   
# Sunspot data WDC-SISO
SP_path<-"http://www.sidc.be/silso"
browseURL(SP_path)# opens page for various downloads
# EISN_current.csv download and moved to
SP.2022_path <-"~/Desktop/Klima_Energiewende/Daten/EISN_current.csv"
SP_2022 <-read.csv(SP.2022_path,header = FALSE)
SP_dat_path<- "/Users/alfloeffler/Desktop/Klima_Energiewende/Daten/SN_d_tot_V2.0.csv"
#variables of "SN_d_tot_V2.0.csv":
#year, month, day, decimal.year, SNvalue , SNerror, Nb_observations
# names(sunspot.daily)
# sunspot daily dat
## Sunspot data WDC-SISO
SP_path<-"http://www.sidc.be/silso"
browseURL(SP_path)
require(lubridate)
# sunspot data
SP_dat_path<- "/Users/alfloeffler/Desktop/Klima_Energiewende/Daten/SN_d_tot_V2.0.csv"
sunspot.daily<-read.csv2(SP_dat_path)%>% 
  dplyr::select(Year,Month,Day,Daily.Nr)%>% 
  mutate(datum=paste0(Year,"-",Month,"-",Day),datum=ymd(datum))
sunspot.daily<- subset(sunspot.daily, datum< ymd("2022-01-01"))
sunspot.daily<-sunspot.daily%>% dplyr::select(datum,Month,"SP"= Daily.Nr)
str(sunspot.daily)
# join SP and Temp data
Temp_SP_dat<-Temp_dat%>% left_join(sunspot.daily)%>%
  dplyr::select(datum,Month,Temp,SP)
summary(Temp_SP_dat)
saveRDS(Temp_SP_dat,file =file.path(dat_path,"Temp_SP.rds") )


#====================
## determine interdependence of variables
Temp_SP_dat<-Temp_SP_dat%>% 
  na.omit()%>%mutate(Datum=datum,datum=as.numeric(datum),SP=as.double(SP))
head(Temp_SP_dat,1)

require(mgcv)
require(mgcViz)
Temp_SP_mdl<- gam(data=Temp_SP_dat,formula=Temp~ s(datum,k=36)+s(SP,k=36), method="REML")
AIC(Temp_SP_mdl)#[1] 438671.1
summary(Temp_SP_mdl)
Temp_SP_mdl<- getViz(Temp_SP_mdl)
summary(Temp_SP_mdl)
o2 <-plot(sm(Temp_SP_mdl,2))
o <- plot( sm(Temp_SP_mdl,1) )
o+l_fitLine(colour = "red") + 
  l_ciLine(mul = 5, colour = "blue", linetype = 3) 
o2+l_fitLine(col= "red")+l_ciLine(linettype =2)+
  ggtitle("Central England Mean Temp ~ SP")
require(broom)
Temp_SP_fit<-Temp_SP_mdl%>%augment()%>%
  dplyr::select(Temp,Tmp_fit=.fitted,SP,Tmp_se=.se.fit)
Temp_SP_fit$Datum<-Temp_SP_dat$Datum
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
Temp_SP_viz(Temp_SP_fit%>%subset(Datum>=ymd("1945-01-01")&
              Datum<=ymd("1954-12-31")))
Temp_SP_viz(Temp_SP_fit%>%subset(Datum>=ymd("1895-01-01")&
                                   Datum<=ymd("2004-12-31")))
