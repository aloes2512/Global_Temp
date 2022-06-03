# evaluate Hadley central england dataset
# the data have been previously downloaded and formatted "full_data22.R"
data_path<- "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds"
dat<-readRDS(data_path)
require(tidyverse)
dat<-subset(dat,datum<as.Date("2022-01-01"))# take only fullyears for models
dat<-dat%>% mutate(Temp=Temp10/10)%>% dplyr::select(-Temp10)
head(dat)
tail(dat)
## Overview dat
ylab= expression(Temperature~(degree*C))
CET_plt<-dat%>% ggplot(aes(x=datum))+
  geom_smooth(aes(x= datum, y= Temp))+
  coord_cartesian(ylim = c(8,11))+
  ggtitle("Central England Mean Temperatures",
          subtitle = paste0("from ",range(dat$datum)[1]," to ",range(dat$datum)[2]))+
  labs(y=ylab,x="")+
  coord_cartesian(ylim = c(9,10.2))
ggsave(filename = "figs/Central_England_Temp.png")
# gam model
require(mgcv)
require(mgcViz)
require(broom)
Temp.mdl..<- function(dt,K){
  mdl = mgcv::gam(dt,formula=Temp~ s(as.numeric(datum),k=K),family="gaussian",method ="REML")
    return(mdl)
}
Temp_fitted<- function(mdl,dt){
  mdl<- mdl%>% 
    augment()%>% 
    dplyr::select(Temp.fit= .fitted,Temp.se=.se.fit)
  mdl$datum<-dt$datum
  mdl<-left_join(dt,mdl,by="datum")
  return(mdl)
}
# apply to with different number of basis functions
Temp.mdl.18<-Temp.mdl..(dat,K=18)
AIC(Temp.mdl.18)#564849.2
summary(Temp.mdl.18)# edf 14.22 
Temp.fit.18<-Temp_fitted(Temp.mdl.18,dat)  
summary(Temp.fit.18)
#check dimensions 
NROW(Temp.fit.18)== NROW(dat)#91401 #check NROW
# increase number of basis functions
# 24
Temp.mdl.24 <-Temp.mdl..(dat,K=24)
summary(Temp.mdl.24)#edf 19.73
AIC(Temp.mdl.24)#564809.8
Temp.fit.24<-Temp_fitted(Temp.mdl.24,dat)  
summary(Temp.fit.24)
# 36 basis functions
Temp.mdl.36 <-Temp.mdl..(dat,K=36)
summary(Temp.mdl.36)# edf 25.52
AIC(Temp.mdl.36)#564792.6
Temp.fit.36<-Temp_fitted(Temp.mdl.36,dat)  
summary(Temp.fit.36)
#40 basis functions
Temp.mdl.40 <-Temp.mdl..(dat,K=40)
summary(Temp.mdl.40)# edf 29.16
AIC(Temp.mdl.40)# 564774.1 is better
Temp.fit.40<-Temp_fitted(Temp.mdl.40,dat)  
summary(Temp.fit.40)
#48 basis functions
Temp.mdl.48 <-Temp.mdl..(dat,K=48)
summary(Temp.mdl.48)# edf 34.76
AIC(Temp.mdl.48)# 564748.2 is better
Temp.fit.48<-Temp_fitted(Temp.mdl.48,dat)  
summary(Temp.fit.48)
# 66 basis functions
Temp.mdl.66 <-Temp.mdl..(dat,K=66)
summary(Temp.mdl.66)#  
AIC(Temp.mdl.66)# 564719.9 is better
Temp.fit.66<-Temp_fitted(Temp.mdl.66,dat)  
summary(Temp.fit.66)

# 72 basis functions
Temp.mdl.72 <-Temp.mdl..(dat,K=72)
summary(Temp.mdl.72)# edf 48.15
AIC(Temp.mdl.72)# 564696.4 is better
Temp.fit.72<-Temp_fitted(Temp.mdl.72,dat)  
summary(Temp.fit.72)
#===========================
# plot fitted data 
ylab<- expression(Temperature~(degree*C))
Temp_plt<-Temp.fit.18 %>% ggplot(aes(x=datum))+
  geom_line(aes(y= Temp.fit),col = "red")+
  geom_line(aes(y= Temp.fit+Temp.se),col= "purple")+
  geom_line(aes(y= Temp.fit-Temp.se),col= "purple")+
  ggtitle("Central England Temperatures
  two gam models",
          subtitle = " 18 & 24 basis functions
    95% confidence intervall")+
  labs(x="",y= ylab)
Temp_plt+geom_line(data = subset(Temp.fit.24,datum< as.Date("2022-01-01")),
                   aes(x=datum,y= Temp.fit),size=1.2,col= "black",linetype= 3)
 #36 basis functions
head(Temp.fit.36,2)
Temp.fit.36 %>% ggplot()+
  geom_line(data = Temp.fit.36,aes(x=datum,y=Temp.fit),col="red")+
  geom_line(data = Temp.fit.36,aes(x=datum,y=Temp.fit+Temp.se),col="blue",linetype= 3)+
  geom_line(data = Temp.fit.36,aes(x=datum,y=Temp.fit-Temp.se),col="blue",linetype= 3)+
  labs(x="",y=ylab)+
  ggtitle("Central England Temperature Trend",
          subtitle = "data fitted with 36 basis functions")

ggsave (filename = "figs/Central_England_Temp_fitted_36.png") 
#72 basisfunctions
head(Temp.fit.72,2)
Temp_plt.72<-subset(Temp.fit.72,datum<as.Date("2022-01-01")) %>% ggplot()+
  geom_line(data = Temp.fit.72,aes(x=datum,y=Temp.fit),col="red")+
  geom_line(data = Temp.fit.72,aes(x=datum,y=Temp.fit+Temp.se),col="blue",linetype= 3)+
  geom_line(data = Temp.fit.72,aes(x=datum,y=Temp.fit-Temp.se),col="blue",linetype= 3)+
  labs(x="",y=ylab)+
  ggtitle("Central England Temperature Trend",
          subtitle = " daily means fitted with 72 basis functions")
ggsave (Temp_plt.72,filename = "figs/Central_England_Temp_fitted_72.png") 
Temp_plt.72 +geom_point(aes(x=datum,y=Temp),size=0.01,alpha=0.2)
#=================================
Temp_plt.60<-subset(Temp.fit.60,datum<as.Date("2022-01-01")) %>% ggplot()+
  geom_line(data = Temp.fit.60,aes(x=datum,y=scale(Temp.fit)),col="red")
  
#==================================
# Temperature change rate
dat.degree.chg<- dat%>% 
  mutate(degr_chg= lead(Temp)-Temp)
degr_chg.rng<-dat.degree.chg$degr_chg%>% range(na.rm = TRUE)# -9.9 to 11.2
Temp_fit.36 %>% head(2) # 36 basis functions
tmp.chg<- function(dtfr){
  dfr= dtfr%>% mutate(degr_chg=lead(Temp)-Temp)%>%
    dplyr::select(datum,degr_chg)
}
tmp.chg(Temp.fit.60)%>% head()
Temp_chg.36<-Temp.fit.36%>% 
  mutate(Temp.chg= lead(Temp.fit)-Temp.fit)
# chg rate from fit 36 basis functions 
Temp_chg.36%>%ggplot(aes(x=datum))+
  geom_line(aes(y=Temp.chg*365.25))+
  ggtitle("Central England ",
         subtitle = "temperature-change-rate
36 basis functions")+
  labs(x="",y= expression(Temperature-Change~degree*C/a))
#annual change rate 36 basis functions
rng.36<-(Temp_chg.36$Temp.chg%>% range(na.rm = TRUE))*365.25# -0.06626885  0.09019538
# chg rate from fit 72 basis functions 
Temp_chg.72<-Temp.fit.72%>% 
  mutate(Temp.chg= lead(Temp.fit)-Temp.fit)
Temp_chg.72%>%ggplot(aes(x=datum))+
  geom_line(aes(y=Temp.chg*365.25))+
  ggtitle("Central England ",
          subtitle = "temperature-change-rate
72 basis functions")+
  labs(x="",y= expression(Temperature-Change~degree*C/a))
#annual change rate 72 basis functions
rng.72<-(Temp_chg.72$Temp.chg%>% range(na.rm = TRUE))*365.25# [1] -0.2349974  0.1869758
Temp_data_list<- list(CET_dat= dat,
                      Temp.18= Temp.fit.18,
                      Temp.24= Temp.fit.24,
                      Temp.36= Temp.fit.36,
                      Temp.72=Temp.fit.72
                   )
saveRDS(Temp_data_list,file = "data/Temp_list.rds")
Temp_data_list<-readRDS("data/Temp_list.rds")
Temp_list<- list(CET_dat=dat,
                 Temp.chg = tmp.chg,
                 Temp.mdl= Temp.mdl..,
                 Temp.fit= Temp_fitted)

mdl<-Temp_list[[3]]
fit<-Temp_list$Temp.fit
chg<-Temp_list$Temp.chg
Temp_dat_48<-Temp_list$Temp.fit(mdl(Temp_list$CET_dat,K=48),dat)
Temp_chg.48<-Temp_dat_48%>% dplyr::select(datum,Temp.fit)%>% mutate(Tmp.chg=lead(Temp.fit)-Temp.fit)
Temp_chg.48%>% ggplot(aes(x= datum,y= scale(Tmp.chg))) +
  geom_line()
Temp_dat_36<-Temp_list$Temp.fit(mdl(Temp_list$CET_dat,K=36),dat)
Temp_chg.36<-Temp_dat_36%>% dplyr::select(datum,Temp.fit)%>% mutate(Tmp.chg=lead(Temp.fit)-Temp.fit)
Temp_chg.36%>% ggplot(aes(x= datum,y= scale(Tmp.chg))) +
  geom_line()  
Temp.chg<-Temp_chg.36%>% 
  left_join(Temp_chg.48,by="datum")%>%
  dplyr::select(datum,Tmp.chg.36=Tmp.chg.x,Tmp.chg.48=Tmp.chg.y)
Temp.chg<-Temp.chg%>% pivot_longer(-"datum",names_to = "Temp_chg",values_to = "chg.rate")%>%
  mutate(chg.rate=scale(chg.rate))
Tmp.chg_plt<-Temp.chg%>%subset(datum>as.Date("1950-01-01"))%>% 
  ggplot(aes(x= datum,col=Temp_chg))+
  geom_line(aes(y=chg.rate))
#import fitted SP data
SP_dat<-readRDS("data/SP_data_mdl.rds")
SP.slct1950<-SP_dat%>%subset(date>as.Date("1950-01-01"))
Tmp.chg_plt+ geom_line(data=dat.sct1950,aes(x= date,y=scale(SP_NB),col="SP_NB"))
SP_plt<-SP.slct1950%>% ggplot(aes(x=date, y=scale(SP_NB),col= "SP_NB"))+
  geom_line()
Temp_chg.66<-Temp.fit.66%>% 
  mutate(Temp.chg= lead(Temp.fit)-Temp.fit)%>% subset(datum>as.Date("1950-01-01"))

Temp_chg.72<-Temp.fit.72%>% 
  mutate(Temp.chg= lead(Temp.fit)-Temp.fit)
Tmp_chg.72<-Temp_chg.72%>% subset(datum>as.Date("1950-01-01"))
SP_plt+geom_line(data = Temp_chg.66,aes(x=datum,y=scale(Temp.chg),col = "Temp.chg"))
