install.packages("mgcViz")
require(tidyverse)
library(mgcViz)
library(mgcv)
library(broom)
# Data imported from NOAA ESLR + UBA Germany (Schauinsland DEUB004, Zugspitze DEUB044)
head(CO2_data.lst[["MLO"]])
names(CO2_data.lst)
MLO_data<-CO2_data.lst[["MLO"]]
MLO_data%>%NROW()#3028
CO2_regr_mdl<-CO2_data.lst%>%map(~gam(.$CO2~.$datetime,k=6,data = .,method = "REML"))
mgcv::gam(formula= CO2 ~ s(as.numeric(datetime),k=6),data = MLO_data,method = "REML")
gam_fnct<- function(x,k=6){
  res<-mgcv::gam(formula= CO2 ~ s(as.numeric(datetime),k=k),data = x,method = "REML")
  res<-res%>%augment()%>% 
    mutate(datetime=as.POSIXct(`as.numeric(datetime)`,origin="1970-01-01",tz="UTC"))%>% 
             dplyr::select(datetime,CO2_fit=.fitted)
 return(res)
}
CO2_mdl.10<-gam_fnct(MLO_data,k=10)
head(CO2_mdl.10)
MLO_plt.10<-CO2_mdl.10%>% ggplot(aes(x=datetime,y=CO2_fit))+geom_line()
predct<-CO2_regr_mdl[[1]]$linear.predictors
MLO_predct<-tibble(datetime= MLO_data$datetime,
  prdct = predct,
  fit= CO2_regr_mdl[[1]]$fitted.values)
MLO_predct%>%ggplot(aes(x=datetime,y= prdct))+geom_line()+
  geom_line(aes(x=datetime,y=fit),col="red")
MLO_plt.10+geom_line(data=MLO_predct,aes(x=datetime,y=fit),col="red")
#apply to all data
 site_fit_lst<-map(CO2_data.lst,gam_fnct,k=10)
