require(rvest)
url_data<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_abp_surface-flask_1_ccgg_event.txt"
sessn<-session(url_data) 
sessn$response%>% summary()

NOAA_data<-sessn$response# 71 header lines txt
temp<-tempfile()
temp<-read_table(url_data,skip = 70,col_names = T)
nm<- c(names(temp)[-c(1,2)],"x1","x2")
names(temp)<-nm
head(temp)
temp<-temp%>%dplyr::select(nm[1:5],CO2="analysis_value")
