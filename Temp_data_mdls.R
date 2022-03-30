# evaluate Hadley central england dataset
# the data have been previously downloaded and formatted "full_data22.R"
data_path<- "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds"
dat<-readRDS(data_path)
require(xts)
require(tidyverse)
head(dat)
tail(dat)
range(dat$datum)[2]-range(dat$datum)[1]# 91310 days
N<-NROW(dat)#91311
# Overview dat
ylab= expression(Temperature~(degree*C))
CET_plt<-dat%>% ggplot(aes(x=datum))+
  #geom_point(aes(y= Temp_rol_mean),size= 0.1, alpha= 0.1)+
  geom_smooth(aes(x= datum, y= Temp10/10))+
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
Temp_mdl.18<-mgcv::gam(data= dat,formula=Temp10/10~ s(as.numeric(datum),k=18),family= "gaussian",method="REML")
summary(Temp_mdl.18)
AIC(Temp_mdl.18)#564849.2
#Temp_mdl<- getViz(Temp_mdl)
Temp_mdl.18<-Temp_mdl.18%>%augment()
NROW(Temp_mdl.18)#91311
NROW(dat)#91311
# increase number of basis functions
Temp_mdl.24<-mgcv::gam(data= dat,formula=Temp10/10~ s(as.numeric(datum),k=24),family= "gaussian",method="REML")
AIC(Temp_mdl.24)#564809.8 is better
Temp_mdl.24<-Temp_mdl.24%>% augment()
# 36 basis functions
Temp_mdl.36<-mgcv::gam(data= dat,formula=Temp10/10~ s(as.numeric(datum),k=36),family= "gaussian",method="REML")
AIC(Temp_mdl.36)#564792 is better
Temp_mdl.36<-Temp_mdl.36%>% augment()

# collect data in one tibble
ylab<- expression(Temperature~(degree*C))
Temp_fit.18<- tibble(datum = dat$datum,
                     Temp = dat$Temp10/10,
                     fit=Temp_mdl.18$.fitted,
                     se=Temp_mdl.18$.se.fit)
Temp_fit.24<-tibble(datum = dat$datum,
                    Temp = dat$Temp10/10,
                    fit=Temp_mdl.24$.fitted,
                    se=Temp_mdl.24$.se.fit)
Temp_fit.36<- tibble(datum = dat$datum,
                  Temp = dat$Temp10/10,
                  fit=Temp_mdl.36$.fitted,
                  se=Temp_mdl.36$.se.fit)

Temp_plt<-Temp_fit.18 %>% ggplot(aes(x=datum))+
  geom_line(aes(y= fit),col = "blue")+
  geom_line(aes(y= fit+se),col= "red")+
  geom_line(aes(y= fit-se),col= "red")+
  ggtitle("Central England Temperatures
  two gam models",
          subtitle = " 18 & 24 basis functions
    95% confidence intervalls")+
  labs(x="",y= ylab)
Temp_plt+geom_smooth(method = mgcv::gam,formula = y ~ s(x, k=24),
  data = dat,aes(x= datum, y= Temp10/10),col= "black",linetype= 2)

#36 basis functions
head(Temp_fit.36,2)
Temp_fit.36 %>% ggplot()+
  geom_line(data = Temp_fit.36,aes(x=datum,y=fit),col="red")+
  geom_line(data = Temp_fit.36,aes(x=datum,y=fit+se),col="blue")+
  geom_line(data = Temp_fit.36,aes(x=datum,y=fit-se),col="blue")+
  labs(x="",y=ylab)+
  ggtitle("Central England Temperature Trend",
          subtitle = "data fitted with 36 basis functions")

ggsave (filename = "figs/Central_England_Temp_fitted_36.png") 
#==================================
# Temperature change rate
Temp_fit.36 %>% head(2) # 36 basis functions
Temp_chg_plt.36<-Temp_fit.36%>% 
  mutate(Temp.chg= lead(fit)-fit)
# fitting with 36 basis functions means 
#that the total time span is divided into 35 intervals
partl_length.36<- round((N-1)/35)#2609 days 7 years
Temp_chg.36%>%ggplot(aes(x=datum))+
  geom_line(aes(y=Temp.chg*365))+
  ggtitle("Central England ",
         subtitle = "temperature-change-rate
  36 basis functions")+
  labs(x="",y= expression(Temperature-Change~degree*C/a))
partl_length.18<- round((N-1)/17)#5371 days ~ 14 years
Temp_chg.18<-Temp_fit.18%>% 
  mutate(Temp.chg= lead(fit)-fit) 

Temp_chg.18%>%ggplot(aes(x=datum))+
  geom_line(aes(y=Temp.chg*365))+
  ggtitle("Central England ",
          subtitle = "temperature-change-rate
  18 basis functions")+
  labs(x="",y= expression(Temperature-Change~degree*C/a))
rng.18<-Temp_chg.18$Temp.chg%>% range(na.rm = TRUE)
#annual range
rng.18<-rng.18*365# -0.03632875  0.03671094
# change rate
rng.36<-Temp_chg.36$Temp.chg%>% range(na.rm = TRUE)
rng.36<-rng.36*365# -0.06978824  0.09509886
# day to day change rate 
head(dat,1)# Year   Day month Temp10 datum 
           #1772     1 Jan       32 1772-01-01
dat.degree<- dat%>% 
  mutate(Temp=Temp10/10,degr_chg= lead(Temp)-Temp)%>%
  dplyr::select(-Temp10)
degr_chg.rng<-dat.degree$degr_chg%>% range(na.rm = TRUE)


