# daily figures
require(lubridate)
dat_path<- "/Users/alfloeffler/Desktop/Klima_Energiewende/Daten/SN_d_tot_V2.0.csv"
sunspot.daily<-read.csv2(dat_path)%>% 
  dplyr::select(Year,Month,Day,"SP"=Daily.Nr)%>% 
  mutate(datum=paste0(Year,"-",Month,"-",Day),datum=ymd(datum))
sunspot.daily<- subset(sunspot.daily, datum< ymd("2022-01-01"))
sunspot.daily<-sunspot.daily%>% mutate(day.n=1:NROW(sunspot.daily))
sunspots<-sunspot.daily
range(sunspot.daily$datum)#"1818-01-02" "2021-12-31"
NROW(sunspot.daily)#74509
sunspot.daily%>% na.omit()%>%ggplot(aes(x=datum,y=SP))+
  geom_point(size = 0.01)+
  geom_smooth()+
  ggtitle("Daily Observed Sunspots",
          subtitle = "Recorded by Belgium Royal Observatory, Brussels")+
  labs(x="",y = "Daily Counts")
sunspot.daily<-sunspot.daily%>% dplyr::select(datum,day.n,SP)%>%as_tibble()
head(sunspot.daily,1)
str(sunspot.daily)
SP_mdl<-gam(data= sunspot.daily,formula=SP~ s(day.n,k=11),family= "gaussian",method="REML")
SP_mdl$model%>%summary()
require(broom)
names(SP_mdl)
#built a datelookup table
date.lookup <- format(seq(as.Date("1818-01-02"), as.Date("2021-12-31"), by = "1 day"))

SP_fit_tbl<-SP_mdl%>% 
  augment()%>% 
  as_tibble()%>% dplyr::select(day.n,"SP_fit"=.fitted,"se"=.se.fit )
SP_fit_tbl<-SP_fit_tbl%>% mutate(datum=as.Date(date.lookup[day.n]))
SP_plt<-SP_fit_tbl%>%  ggplot(aes(x=datum))+
  geom_line(aes(y=SP_fit),col = "blue")+
  ggtitle("Average Daily Sunspots",
          subtitle = "smoothed with 11 (blue) & 28(black) basis functions")+
  labs(x="",y="Average Daily Counts")
# smoothing with 28 basis functions:
head(sunspots)
 SP_mdl.28<-gam(data= sunspots,formula=SP~ s(day.n,k=28),family= "gaussian",method="REML")
 SP_mdl.28$model%>%summary()
 SP_fit_28<-SP_mdl.28%>% 
   augment()%>% 
   as_tibble()%>% dplyr::select(day.n,"SP_fit"=.fitted,"se"=.se.fit )
 SP_fit_28<-SP_fit_28%>% mutate(datum=as.Date(date.lookup[day.n]))
 SP_plt+
   geom_line(data=SP_fit_28,aes(x=datum,y=SP_fit),col = "black",linetype = 3)
 






# add Temp data
data_path<- "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds"
Temp_dat<-readRDS(data_path)%>% 
  mutate(Temp= Temp10/10)%>% dplyr::select(-Temp10)
Temp_dat<-Temp_dat%>% arrange(datum)
NROW(Temp_SP_dat)#91311
head(Temp_dat)
NROW(na.omit(Temp_SP_dat))
Temp_SP_dat<-left_join(Temp_dat,sunspot.daily,by= c("datum"="datum"))%>%as_tibble()
Temp_SP_dat<-na.omit(Temp_SP_dat)
# Temp SP model
Temp_mdl<-gam(data=Temp_SP_dat, formula = Temp~ s(as.numeric(datum),k=12)+s(SP,k=12), method = "REML")
b<- getViz(Temp_mdl)
plot(b,allTerms = TRUE)
require(broom)
Temp_SP_fit<-Temp_mdl%>%augment()%>% dplyr::select(Temp.ft=.fitted,Temp,se=.se.fit)
NROW(Temp_SP_fit)
Temp_SP_fit$datum<-Temp_SP_dat$datum
Temp_SP_fit%>% ggplot(aes(x =datum,y=Temp.ft))+
  geom_point(size = 0.01)+
  geom_point(aes(y=Temp),size= 0.01,col="red",alpha= 0.05)
plot(Temp_mdl,all.terms = T,pages = 1)
