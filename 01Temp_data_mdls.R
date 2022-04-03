# evaluate Hadley central england dataset
# the data have been previously downloaded and formatted "full_data22.R"
data_path<- "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds"
dat<-readRDS(data_path)
require(tidyverse)
dat<-subset(dat,datum<as.Date("2022-01-01"))# take only fullyears for models
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
#==================================
# Temperature change rate
dat.degree.chg<- dat%>% 
  mutate(degr_chg= lead(Temp)-Temp)
degr_chg.rng<-dat.degree.chg$degr_chg%>% range(na.rm = TRUE)# -9.9 to 11.2
Temp_fit.36 %>% head(2) # 36 basis functions

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



