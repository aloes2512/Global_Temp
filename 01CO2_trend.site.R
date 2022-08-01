#NOAA CO2 data have been imported, formated and stored @ NOAA_data.rds
CO2_data.lst<-readRDS("data/NOAA_data.rds")
require(tidyverse)
CO2_data.lst%>%summary()
#station parameters are listed by NOAA
station_sites<-readRDS("data/station_sites.rds")
station_sites%>%head()
nms<-names(CO2_data.lst)# 20 sites## MLO"     "ZEP"     "SUM"     "PSA"     "MHD"     "MEX"     "ICE"     "HPB"     "CPT"     "CHR"     "BHD"    
# "BRW"     "SMO"     "ABP"     "SPO"     "CRV"     "DEUB004" "LMP"     "DEUB044" "TAC"    
tolower(nms)
stnms<-tibble(nms=names(CO2_data.lst))
# NOAA nc files
# set path and filename of noaa nc data
url_co2_path<-"https://gml.noaa.gov/aftp/data/trace_gases/co2/in-situ/tower/nc/"

browseURL(url_co2_path)
noaa_page <- read_html(url_co2_path)
# Lösung: Suche Tabelle wähle Spalte "Name" und "files mit Endung .nc
noaa_nc_files<- noaa_page %>% html_table() %>% {.[[1]]} %>% # selects first element of list: "html_table(noaa_page)"
  {.[["Name"]]} %>% # selects elements of a list by name
  stringr::str_subset("\\.nc$")
noaa_nc_names <-noaa_nc_files%>% str_extract("^.{7}")%>%str_extract(".{3}$")#"amt" "bao" "crv" "lef" "mbo" "sct" "snp" "wbi" "wgc" "wkt"
NC_names<- toupper(noaa_nc_names)

# selected sites
stations.sel<-stnms%>%left_join(station_sites, by= c("nms"="Code"))%>% 
  dplyr::select(-Country)
# models need numerical time series
nmdt<-function(x) x<-mutate(x,datetime=as.numeric(datetime))
CO2_data.nmdt<-CO2_data.lst%>% map_df(nmdt)
CO2_data.nmdt<-CO2_data.nmdt%>% right_join(stations.sel, by =c("site"="nms"))
class(CO2_data.nmdt)# tbl
head(CO2_data.nmdt)

# linear regression model per site
CO2_regr_mdl<-CO2_data.nmdt%>%dplyr::select(datetime,CO2,site)%>%
  split(.$site)%>%
  map(function(x) lm(x$CO2~x$datetime))
require(broom)
CO2_regr_fit<-CO2_regr_mdl%>% map(augment)%>% 
  map(rename,
      c("CO2"=`x$CO2`),
      c("datetime"=`x$datetime`),
      c("CO2_fit" = ".fitted"),
      c("CO2_resd"=".resid"))

nms
CO2_lin.fit<- tibble()
for (nm in nms){
  df<-mutate(CO2_regr_fit[[nm]],
      Name =nm,
      datetime=as.POSIXct(datetime,origin="1970-01-01",
      tz = "UTC"))
  CO2_lin.fit<- bind_rows(CO2_lin.fit,df)
}
CO2_lin.fit$Name<-as_factor(CO2_lin.fit$Name)
head(CO2_lin.fit)
summary(CO2_lin.fit)
CO2_trnd_plt<-CO2_lin.fit%>%ggplot(aes(x=datetime,y=CO2_fit,col = Name))+
  geom_line()
CO2_trnd_plt+ggtitle("CO2-Immission Linear Trend",
                     subtitle = "20 sites")+
  labs( x= "",y= "CO2 [ppm]")
#model coefficients
CO2_mdl.pms<-CO2_data.nmdt%>%dplyr::select(datetime,CO2,site)%>%
  split(.$site)%>%
  map_df(function(x) lm(x$CO2~x$datetime)$coefficients)%>%
                      bind_cols(stnms)%>%
                      arrange(desc(`x$datetime`))
CO2_mdl.pms<-CO2_mdl.pms%>%dplyr::select(site=nms,slope=`x$datetime`,Intcpt=`(Intercept)`)
yrsec<-60*60*24*365.24
CO2_mdl.pms$slope*yrsec
CO2_mdl.pms<-CO2_mdl.pms%>% mutate(slp.pa=round(slope*yrsec,digits=2),Intcpt=round(Intcpt,digits=0))%>%dplyr::select(-slope)
# write to clipboard
install.packages("clipr")
require(clipr)
write_clip(CO2_mdl.pms)# writes to clipboard
# Residuals
CO2_resd<-CO2_lin.fit%>% dplyr::select(-c(.hat,CO2_fit,.cooksd,.std.resid,.sigma))
CO2_MLO.resd<-CO2_resd%>% subset(Name=="MLO")
CO2_resd.plt<-CO2_MLO.resd%>% ggplot(aes(x=datetime,y=CO2_resd))+
  geom_point(size= 0.1)
CO2_resd.plt+geom_smooth(mapping=aes(x=datetime,y= CO2_resd),col="red")+
  ggtitle("CO2_deviance to linear regression",
          subtitle= "Mauna Loa Observations")
saveRDS(CO2_resd,file = "data/CO2_resd.rds")
# reduce observations to year 2000 until year 2021
require(lubridate)
CO2_trnd_plt.00to21<-CO2_lin.fit%>%
                      subset(CO2_mdl$datetime>  ymd("2000-01-01"))%>%
                      subset(Name %in% c("DEUB044","BRW"))%>%
                      ggplot(aes(x=datetime,y=CO2_fit,col = Name))+
                      geom_line()
CO2_trnd_plt.00to21+ggtitle("CO2-Immission Linear Trend",
                      subtitle = "2 sites; observations from 2000 to 2021")+
                      labs( x= "",y= "CO2 [ppm]")



# nonlinear trend
require(mgcv)
CO2_gam_mdl<-CO2_data.nmdt%>%dplyr::select(datetime,CO2,site)%>%
  split(.$site)%>%
  map_df(function(x) mgcv::gam(formula = x$CO2 ~s(x$datetime,k=20)%>%augment()))
 
CO2_nonlin.fit<-CO2_gam_mdl%>% map(augment)%>% 
  map(rename,
      c("CO2"=`x$CO2`),
      c("datetime"=`x$datetime`),
      c("CO2_fit" = ".fitted"),
      c("CO2_resd"=".resid"))
CO2_nonlin.fit<-CO2_nonlin.fit%>% map_dfr(bind_rows,.id= "Name")
CO2_nonlin.mdl<-CO2_nonlin.fit%>%  mutate(datetime= as.POSIXct(datetime,origin = "1970-01-01",tz="UTC"))

CO2_nonlin_plt<-CO2_nonlin.mdl%>%ggplot(aes(x=datetime,y= CO2_fit,col= as_factor(Name)))+
  geom_line(size = 0.2)#
CO2_nonlin_plt+ggtitle("CO2-nonlinear Trend 20 sites",
  subtitle = "~ MLO linear trend (black)")+labs(x="",y= "CO2_fit [ppm]")+
  geom_smooth(method ="lm",data =CO2_MLO.resd,mapping=aes(x=datetime, y= CO2),col ="black",size=0.8 )
# plot subset of data
