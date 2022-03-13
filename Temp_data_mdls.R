# evaluate Hadley central england dataset
# the data have been previously downloaded and formatted "full_data22.R"
data_path<- "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds"
dat<-readRDS(data_path)
require(xts)
require(tidyverse)
head(dat)
# calculate moving average by package xts::rollmean 
k<- 30 # window width
dat<-dat%>% 
  mutate(Temp_rol_mean=rollmean(Temp10/10,
                                k ,fill = NA,
                                align = "right"))
sum(is.na(dat$Temp_rol_mean))#29
dat<- dat%>% subset(!is.na(dat$Temp_rol_mean))
summary(dat)
tail(dat)
NROW(dat)#91282
# Overview dat
ylab= expression(Temperature~(degree*C))
CET_plt<-dat%>% ggplot(aes(x=datum))+
  #geom_point(aes(y= Temp_rol_mean),size= 0.1, alpha= 0.1)+
  geom_smooth(aes(x= datum, y= Temp_rol_mean))+
  coord_cartesian(ylim = c(8,11))+
  ggtitle("Central England Mean Temperatures",
          subtitle = "30 days moving average")+
  labs(y=ylab,x="")
ggsave(filename = "figs/Central_England_mean_Temp.png")
# gam model
require(mgcv)
require(mgcViz)
mdl<-mgcv::gam(data= dat,formula=Temp10/10~ s(as.numeric(datum),k=12),family= "gaussian",method="REML")
mdl<- getViz(mdl)
mdl<-mdl%>%augment()
NROW(mdl)#91282
NROW(dat)
# collect data in one tibble
ylab<- expression(Temerature~(degrees*C))
Temp_fit<- tibble(datum = dat$datum,
                  Tenp = dat$Temp10/10,
                  fit=mdl$.fitted,
                  se=mdl$.se.fit)
Temp_fit %>% ggplot(aes(x=datum))+
  geom_line(aes(y= fit),col = "blue")+
  geom_line(aes(y= fit+se),col= "red")+
  geom_line(aes(y= fit-se),col= "red")+
  ggtitle("Central England Mean Temperatures",
          subtitle = " with 95% confidence intervalls")+
  labs(x="",y= ylab)
saveRDS(Temp_fit, file= "Temp_dat_fitted.rds")
