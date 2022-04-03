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
