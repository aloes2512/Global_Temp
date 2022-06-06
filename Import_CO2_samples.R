# import function data have been downloaded from https://gml.noaa.gov
install.packages("tidyverse")
require(tidyverse)
require(lubridate)
# Station sites
ESL_sites<-"https://gml.noaa.gov/dv/site/"
browseURL(ESL_sites)
require(rvest)
s<- html_session(ESL_sites)
page <- s %>%       #html document
  read_html()
tabl<- page%>%html_table()
summary(tabl)
station_sites<-tabl[[1]]
saveRDS(station_sites,file = "~/projects/Global_Temp/data/station_sites.rds")
url_data<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_abp_surface-flask_1_ccgg_event.txt"
browseURL(url_data)
## ABP ( Brasilia)Arembepe, Bahia
ABP_data<- read.table(url_data)%>% as.data.frame()
# variable names (if not provided as header)
names<-c("sample_site_code sample_year sample_month sample_day sample_hour sample_minute sample_seconds sample_id sample_method parameter_formula analysis_group_abbr analysis_value analysis_uncertainty analysis_flag analysis_instrument analysis_year analysis_month analysis_day analysis_hour analysis_minute analysis_seconds sample_latitude sample_longitude sample_altitude sample_elevation sample_intake_height event_number
")
names<- str_split(names," ")[[1]]# select variables needed
names(ABP_data)<-names
# mk datetime time series
#====================================
mk_datetime<- function(yr,mn,dy,hr){
  dt<-paste0(yr,"-",mn,"-",dy," ",hr)
  datetime= ymd_h(dt)
}
# select variables needed
#==============================
sel_tbl.27<- function(dat){
  dat<- mutate(dat,datetime=mk_datetime(sample_year,sample_month,sample_day,sample_hour))
  dat<- dat%>%dplyr::select(site=sample_site_code,
                            datetime, 
                            CO2=analysis_value,
                            lon=sample_longitude,
                            lat= sample_latitude,
                            alt=sample_altitude)%>%
    mutate(datetime=floor_date(datetime,unit= "hour")%>%
             format("%Y-%m-%d %H"))
  dat<- dat%>% group_by(datetime)%>% 
    summarise(CO2= mean(CO2,na.rm = TRUE),
              lon=median(lon),
              lat=median(lat),
              alt=median(alt),
              site=first(site)
              )
  dat<-as_tibble(dat)%>%mutate(datetime=ymd_h(datetime))
  return(dat)
}
# select data with 19 variables
sel_tbl.19<-function(dfr){
  dfr<- as_tibble(dfr)%>%mutate(datetime= date_decimal(time_decimal),
                                datetime=floor_date(datetime,unit ="hour"))%>%
    group_by(datetime)%>% 
    summarise(site= first(SMO_data$site_code),
              CO2=mean(value,na.rm=TRUE),
              lat= first(SMO_data$latitude),
              lon=first(SMO_data$longitude),
              alt=first(SMO_data$altitude))
  dfr<- subset(dfr,CO2>0)
}
colnames(ABP_data)#27
ABP_dat<-sel_tbl.27(ABP_data)
summary(ABP_dat)
#plt discrete sampled data
#==============================
discrt_plt<- function(dat){
  plt= ggplot(dat,aes(x=datetime,y= CO2))+
    ggtitle(paste("CO2-Immission @",first(dat$site)),
            subtitle = paste("latitude",first(dat$lat),
                             "longitude",first(dat$lon),
                             "altitude",first(dat$alt),"m"))+
    labs(x= "", y= "CO2[ppm]")
  return(plt)
}
#=========================================
discrt_plt(ABP_dat)+
  geom_point(size= 0.5)+
  geom_smooth(method="lm",col = "red",size = 0.4)
#===================================================
#Samoa data
url_samoa_dat<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/surface/smo/co2_smo_surface-insitu_1_ccgg_HourlyData.txt"
browseURL(url_samoa_dat)
SMO_data<-read.table(url_samoa_dat,header = TRUE)
names(SMO_data)
length(names(SMO_data))# 19
#Samoa data
SMO_dat<-sel_tbl.19(SMO_data)
summary(SMO_dat)
SMO_plt<-discrt_plt(SMO_dat)
SMO_plt+  geom_point(size = 0.1,alpha= 0.1)+
  geom_smooth(method="lm",col = "red",size = 0.3,alpha= 0.1)
  #=======
BRW<-"co2_brw_surface-insitu_1_ccgg_HourlyData.webarchive"
dat_path<- "~/desktop/Klima_Energiewende/Daten"
BRW_path_h<- "https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/surface/brw/co2_brw_surface-insitu_1_ccgg_HourlyData.txt"
BRW_data<-read.table(BRW_path_h,header = TRUE)
names(BRW_data)# 19 variables 

head(BRW_data,2)
BRW_dat<-BRW_data%>% mutate(CO2= as.numeric(value),
                   datetime=mk_datetime(year,month,day,hour))%>% 
  dplyr::select(site=site_code,datetime,CO2,lat= latitude,lon=longitude,alt=altitude)%>% 
  subset(CO2>0)
summary(BRW_dat)
head(BRW_dat)
Lat<- as.numeric(first(BRW_dat$lat))%>% round(digits=4)
Lon<- as.numeric(first(BRW_dat$lon))%>% round(digits=4)
Alt<- as.numeric(first(BRW_dat$alt))%>% floor()
BRW_plt<-BRW_dat%>% ggplot(aes(x=datetime,y=CO2))+
  ggtitle("CO2-Immissionen @ BRW",
          subtitle = paste("latitude:",Lat,"longitude:",Lon,"altitude:",Alt,"m"))+
  labs(x= "",y="CO2[ppm]")
BRW_plt+  geom_point(size=0.1,alpha=0.1)+
  geom_smooth(method= "lm",col = "red",linetype= 4)+
  geom_smooth(col= "purple",size = 0.4)
#Kap Südafrika CPT South African Weather Service
url_CPT<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_cpt_surface-flask_1_ccgg_event.txt"
CPT_data<-read.table(url_CPT,header = FALSE)

# names of discrete flask sampled data
#=====================================
colnames(CPT_data)<- names #27 vars
CPT_data%>%str()
# select variables needed and eliminate outliers
CPT_dat<-sel_tbl.27(CPT_data)%>% as_tibble()%>% subset(CO2>350 & CO2< 419)
CPT_plt<-discrt_plt(CPT_dat)
CPT_plt+ geom_point(size=0.2,alpha= 0.5) +geom_smooth(method="lm",col="red")         
#new zealand baring head
url_BHD<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_bhd_surface-flask_1_ccgg_event.txt"
BHD_data<-read.table(url_BHD,header = FALSE)
names(BHD_data)<-names
BHD_data%>%str()

BHD_dat<-sel_tbl.27(BHD_data)%>% as_tibble()
head(BHD_dat)
summary(BHD_dat)
BHD_plt<- discrt_plt(BHD_dat)
BHD_plt + geom_point(size=0.2,alpha=0.2)+geom_smooth(method = "lm",col ="red",size= 0.5)
#=========================================
# Kiribati Island
url_CHR<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_chr_surface-flask_1_ccgg_event.txt"
CHR_data<- read.table(url_CHR)
head(CHR_data)
colnames(CHR_data)<-names
CHR_dat<-sel_tbl.27(CHR_data)%>% subset(CO2>0)
CPT_plt<-discrt_plt(CHR_dat)
CPT_plt+ geom_point(size = 0.2,alpha= 0.1)+
  geom_smooth(method = "lm", col = "red",size = 0.5)
# Hohenpleissenberg
url_HPB<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_hpb_surface-flask_1_ccgg_event.txt"
HPB_data<- read.table(url_HPB)
head(HPB_data)
colnames(HPB_data)<-names #27
require(lubridate)
HPB_dat<-sel_tbl.27(HPB_data)

HPB_plt<-discrt_plt(HPB_dat)
HPB_plt+geom_point(size= 0.3,alpha=0.2)+
  geom_smooth(col = "red", size= 0.5)
#Storhofdi, Vestmannaeyjar, Iceland (ICE)
url_ICE<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_ice_surface-flask_1_ccgg_event.txt"
ICE_data<- read.table(url_ICE)
colnames(ICE_data)<-names
ICE_dat<-sel_tbl.27(ICE_data)%>% subset(CO2>0& CO2<450)
ICE_plt<-discrt_plt(ICE_dat)
ICE_plt+ geom_point(size= 0.2,alpha =0.1)+
  geom_smooth(col = "red",size= 0.5)
#High Altitude Global Climate Observation Center, Mexico MEX
url_MEX<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_mex_surface-flask_1_ccgg_event.txt"
MEX_data<-read.table(url_MEX)
colnames(MEX_data)<-names
MEX_dat<-sel_tbl.27(MEX_data)
MEX_plt<-discrt_plt(MEX_dat)
MEX_plt+ geom_point(size=0.4,alpha= 0.2)+
  geom_smooth(col= "red",size= 0.5)
# Mace Head, County Galway, Ireland (MHD)
url_MHD<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_mhd_surface-flask_1_ccgg_event.txt"
MHD_data<- read.table(url_MHD)
colnames(MHD_data)<- names
MHD_dat<-sel_tbl.27(MHD_data)%>%subset(CO2>0&CO2<450)
MHD_plt<- discrt_plt(MHD_dat)
MHD_plt+ geom_point(size=0.4,alpha= 0.2)+
  geom_smooth(col= "red",size= 0.5)
#Palmer Station, Antarctica, United States (PSA)
url_PSA<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_psa_surface-flask_1_ccgg_event.txt"
PSA_data<-read.table(url_PSA)
names(PSA_data)<- names
PSA_dat<-sel_tbl.27(PSA_data)%>%subset(CO2>0&CO2<480)
PSA_plt<- discrt_plt(PSA_dat)
PSA_plt+ geom_point(size= 0.2,alpha =0.2)+
  geom_smooth(col="red",size= 0.5)
# Summit, Greenland (SUM)
url_SUM<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_sum_surface-flask_1_ccgg_event.txt"
SUM_data<- read.table(url_SUM)
names(SUM_data)<-names
SUM_dat<- sel_tbl.27(SUM_data)
SUM_plt<-discrt_plt(SUM_dat)
SUM_plt+geom_point(size=0.2,alpha= 0.2)+
  geom_smooth(col = "red",size= 0.5)
#Ny-Alesund, Svalbard, Norway and Sweden (ZEP)
url_ZEP<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_zep_surface-flask_1_ccgg_event.txt"
ZEP_data<-read.table(url_ZEP)
names(ZEP_data)<-names
ZEP_dat<- sel_tbl.27(ZEP_data)%>% subset(CO2>0&CO2< 450)
ZEP_plt<- discrt_plt(ZEP_dat)
ZEP_plt+geom_point(size =0.2, alpha=0.2)+
  geom_smooth(col= "red",size=0.5)


#  Mauna Loa, Hawaii, United States (MLO)
url_MLO<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_mlo_surface-flask_1_ccgg_event.txt"
MLO_data<- read.table(url_MLO)
names(MLO_data)<-names
MLO_dat<- sel_tbl.27(MLO_data)%>% subset(CO2>200&CO2<450)
MLO_plt<- discrt_plt(MLO_dat)
MLO_plt+ geom_point(size=0.2,alpha=0.2)+
  geom_smooth(col="red",size=0.4)


# save imported data
NOAA_CO2<- list(MLO=MLO_dat,
                ZEP=ZEP_dat,
                SUM=SUM_dat,
                PSA=PSA_dat,
                MHD=MHD_dat,
                MEX=MEX_dat,
                ICE= ICE_dat,
                HPB= HPB_dat,
                CPT=CPT_dat,
                CHR= CHR_dat,
                BHD= BHD_dat,
                CPT= CPT_dat,
                CHR= CHR_dat,
                BRW= BRW_dat,
                SMO=SMO_dat,
                ABP=ABP_dat)
summary(NOAA_CO2)

