require(rvest)
require(tidyverse)
# general backgound rvest:
url_rvest<-"https://cran.r-project.org/web/packages/rvest/rvest.pdf"
browseURL(url_rvest)
# example: text import abp ( Brasilian) co2-data
url_data<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_abp_surface-flask_1_ccgg_event.txt"
sessn<-session(url_data) 
sessn$response%>% summary()

NOAA_meta_data<-sessn$response# 71 header lines txt
temp<-tempfile()
# correct header
## line 71 describes variables (header)
read.table(url_data)# reads values only 
temp<-read_table(url_data,skip = 70,col_names = T)
nm<- c(names(temp)[-c(1,2)],"x1","x2")# delete the first two variables and add X1,X2
names(temp)<-nm
head(temp)
#select variables needed
temp<-temp%>%dplyr::select(nm[1:5],CO2="analysis_value")
ABP_CO2<- temp
