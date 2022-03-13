# evaluate Hadley central england dataset
# the data have been previously downloaded and formatted "full_data22.R"
data_path<- "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds"
dat<-readRDS(data_path)
require(xts)
require(tidyverse)
head(dat)
tail(dat)
NROW(dat)#91311
# Overview dat
ylab= expression(Temperature~(degree*C))
CET_plt<-dat%>% ggplot(aes(x=datum))+
  #geom_point(aes(y= Temp_rol_mean),size= 0.1, alpha= 0.1)+
  geom_smooth(aes(x= datum, y= Temp10/10))+
  coord_cartesian(ylim = c(8,11))+
  ggtitle("Central England Mean Temperatures",
          subtitle = "30 days moving average")+
  labs(y=ylab,x="")
ggsave(filename = "figs/Central_England_Temp.png")
# gam model
require(mgcv)
require(mgcViz)
Temp_mdl<-mgcv::gam(data= dat,formula=Temp10/10~ s(as.numeric(datum),k=18),family= "gaussian",method="REML")
Temp_mdl<- getViz(Temp_mdl)
Temp_mdl<-Temp_mdl%>%augment()
NROW(Temp_mdl)#91311
NROW(dat)#91311
# collect data in one tibble
ylab<- expression(Temerature~(degrees*C))
Temp_fit<- tibble(datum = dat$datum,
                  Temp = dat$Temp10/10,
                  fit=Temp_mdl$.fitted,
                  se=Temp_mdl$.se.fit)
Temp_plt<-Temp_fit %>% ggplot(aes(x=datum))+
  geom_line(aes(y= fit),col = "blue")+
  geom_line(aes(y= fit+se),col= "red")+
  geom_line(aes(y= fit-se),col= "red")+
  ggtitle("Central England Temperatures
  two gam models",
          subtitle = " 12 &18 basis functions
    95% confidence intervalls")+
  labs(x="",y= ylab)
Temp_plt+geom_smooth(method = mgcv::gam,formula = y ~ s(x, k=12),
  data = dat,aes(x= datum, y= Temp10/10),col= "black",linetype= 2)
# Day-Mean-Temperatures
ylab0<-expression(Mean-Temperatur~(degree*C))
Day_Temp<- dat%>% ggplot(aes(x=datum,y=Temp10/10))+geom_point(size =0.01)+ggtitle("Day-Mean Temperature
  Central England")+
  labs(y= ylab0, x="")
