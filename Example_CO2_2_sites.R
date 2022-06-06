# select and format NOAA data
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

# variable names from ESL files (if not provided as header)
names<-c("sample_site_code sample_year sample_month sample_day sample_hour sample_minute sample_seconds sample_id sample_method parameter_formula analysis_group_abbr analysis_value analysis_uncertainty analysis_flag analysis_instrument analysis_year analysis_month analysis_day analysis_hour analysis_minute analysis_seconds sample_latitude sample_longitude sample_altitude sample_elevation sample_intake_height event_number
")
names<- str_split(names," ")[[1]]# select variables needed

# ALT data
url_alt<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_alt_surface-flask_1_ccgg_event.txt"
ALT_data<- read.table(url_alt)%>%as.data.frame()
# ALT coordinates
#Country: Canada Country Flag
#Latitude: 82.4508° North
#Longitude: 62.5072° West
#Elevation: 185.00 masl
#Time Zone: Local Standard Time + 5.0 hour(s) = UTC
names(ALT_data)<-names
mydf <-sel_tbl.27(ALT_data)
#ABP data
url_abp<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/flask/surface/co2_abp_surface-flask_1_ccgg_event.txt"


ABP_data<- read.table(url_abp)%>% as.data.frame()
names(ABP_data)<-names

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

ABP_CO2_dat<-sel_tbl.27(ABP_data)
ALT_CO2_dat<- sel_tbl.27(ALT_data)%>% subset(CO2>0)
# visualise site CO2 data
ABP_CO2_dat%>% head()
ABP_CO2_dat%>% ggplot(aes(x=datetime,y =CO2))+
  geom_point(size= 0.2)+
  geom_smooth(method = "lm",col ="red")+
  ggtitle("CO2-Immission -lin trend-",
          subtitle = paste("@lon:",first(ABP_CO2_dat$lon),"lat:",first(ABP_CO2_dat$lat)))
# plot_trend
plt_trnd<- function(dfr){
  plt<-dfr%>% ggplot(aes(x=datetime,y =CO2,col=site))+
    geom_point(size= 0.2)+
    geom_smooth(method = "lm",col ="red")+
    ggtitle("CO2-Immission -lin trend-",
            subtitle = paste("@lon:",first(dfr$lon),"lat:",first(dfr$lat)))
  return(plt)
}
plt_trnd(ALT_CO2_dat) 
sel_dat<-ALT_CO2_dat%>% bind_rows(ABP_CO2_dat)  
sel_dat%>%plt_trnd()
