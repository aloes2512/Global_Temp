CO2_data.lst<-readRDS("data/NOAA_data.rds")
require(tidyverse)
require(lubridate)
require(mgcViz)
require(broom)
CO2_data.lst%>%summary()
nms<-CO2_data.lst%>%names()
station_sites<-readRDS("data/station_sites.rds")
station_sites%>%head()
all_site_nms<- station_sites$Code
length(all_site_nms)#217
intersect(all_site_nms,nms)# all sites included
station_sites$Latitude%>% range()# -89.98 82.451

# selected sites with geo params
stnms<-tibble(nms=names(CO2_data.lst))
stations.sel<-stnms%>%left_join(station_sites, by= c("nms"="Code"))%>% 
  dplyr::select(-Country)
nmdt<-function(x) x<-mutate(x,datetime=as.numeric(datetime))
CO2_data.nmdt<-CO2_data.lst%>% map_df(nmdt)
CO2_data.nmdt<-CO2_data.nmdt%>% right_join(stations.sel, by =c("site"="nms"))
#================================
equ_nms<- as.vector(equatorial_sites$Code)# 6 sites including MLO
# overview equatorial sites full timespan
equ_plt<-CO2_data.nmdt%>% 
  subset(site %in%equ_nms )%>%
  ggplot(aes(x=datetime,y= CO2,col = site))+
  geom_smooth()
equ_plt+ggtitle("CO2 equatorial sites |Latitude|< 35°")+
  labs(x = "datetime in sec from 1970-01-01", y="CO2 [ppm]")
# select sites and timespan
start_tm = function(st) {as.numeric(ymd_hms(paste0("20",st,"-01-01 00:00:00")))}
plt_nms.yrs<- function(sites,yrs){
  nms<- sites
  nams<-sites%>%paste(collapse = ", ")
  CO2_data.yrs <-CO2_data.nmdt%>% subset(site %in% nms&datetime>start_tm(as.character(yrs[1]))& datetime< start_tm(as.character(yrs[2])))
  CO2_mdl<-CO2_data.yrs%>% split(.$site)%>%map(~gam(.$CO2~s(.$datetime,k=12),data = .,method = "REML"))
  CO2_fitted<-CO2_mdl%>% map(augment)%>%
    map(rename,CO2=`.$CO2`,
        datetime=`.$datetime`,
        CO2_fit= .fitted)
  CO2_fitted<-CO2_fitted%>% map_dfr(bind_rows, .id= "Name") %>%
    dplyr::select(Name,datetime,CO2,CO2_fit) %>% 
    mutate(datetime= as.POSIXct(datetime,origin = "1970-01-01"))
  CO2_plt<-CO2_fitted%>%ggplot(aes(x=datetime,y=CO2_fit,col = Name))+geom_line()
  CO2_plt<-CO2_plt+ labs(x=" ",y= "CO2_fit [ppm]")+
    ggtitle(paste("CO2-Immission",nams),
            subtitle = " Trend smoothed with 12 Basisfunctions")
  return(CO2_plt)
}
MLO.CHR_CO2_plt.14.21<-plt_nms.yrs(c("MLO","CHR"),yrs = c("14","21"))



# select two sites MLO and CHR
start_tm = function(st) {as.numeric(ymd_hms(paste0("20",st,"-01-01 00:00:00")))}
slct_dat<-CO2_data.nmdt%>% subset(site %in% c("CHR","MLO")&datetime>start_tm("08")& datetime< start_tm("16"))
require(lubridate)
require(mgcViz)
slct_mdl<-slct_dat%>% split(.$site)%>%map(~gam(.$CO2~s(.$datetime,k=12),data = .,method = "REML"))
 require (broom)
slct_CO2_fitted<-slct_mdl%>% map(augment)%>%
   map(rename,CO2=`.$CO2`,
       datetime=`.$datetime`,
       CO2_fit= .fitted)#%>%
slct_CO2_fitted<-slct_CO2_fitted%>% map_dfr(bind_rows, .id= "Name") %>%
   dplyr::select(Name,datetime,CO2,CO2_fit) %>% 
  mutate(datetime= as.POSIXct(datetime,origin = "1970-01-01"))  
slct_CO2_fitted%>%ggplot(aes(x=datetime,y=CO2_fit,col = Name))+geom_line()
# select equatorial sites 
equatorial_sites<-station_sites%>%subset(Code %in% intersect(all_site_nms,nms))%>%
  subset (abs(Latitude)< 35)
equ_nms<- as.vector(equatorial_sites$Code)# 6 sites including MLO
equator_CO2_plt.08.16<-plt_nms.yrs(equ_nms,yrs = c("08","16"))
plt_nms.yrs<- function(sites,yrs){
  nms<-sites
  CO2_data.yrs <-CO2_data.nmdt%>% subset(site %in% nms&datetime>start_tm(as.character(yrs[1]))& datetime< start_tm(as.character(yrs[2])))
  CO2_mdl<-CO2_data.yrs%>% split(.$site)%>%map(~gam(.$CO2~s(.$datetime,k=12),data = .,method = "REML"))
  CO2_fitted<-CO2_mdl%>% map(augment)%>%
    map(rename,CO2=`.$CO2`,
        datetime=`.$datetime`,
        CO2_fit= .fitted)
  CO2_fitted<-CO2_fitted%>% map_dfr(bind_rows, .id= "Name") %>%
    dplyr::select(Name,datetime,CO2,CO2_fit) %>% 
    mutate(datetime= as.POSIXct(datetime,origin = "1970-01-01"))
  CO2_plt<-CO2_fitted%>%ggplot(aes(x=datetime,y=CO2_fit,col = Name))+geom_line()
  return(CO2_plt)
}
CO2_sel_plt.14.21<-plt_nms.yrs(sites = c("ZEP","SUM"),yrs=c(14,21))+labs(x="",y="CO2_fitted [ppm]")
CO2_sel_plt.14.21 + ggtitle(paste(c("ZEP","SUM"),collapse = ", "),
                            "CO2_Trend")                             
# equatorial sites
equ_nms<- as.vector(equatorial_sites$Code)# 6 sites including MLO
equatorial_sites_plt.14.21<-plt_nms.yrs(equ_nms,yrs = c("14","21"))
equatorial_sites_plt.08.14<-plt_nms.yrs(equ_nms,yrs = c("08","14"))
# northern sites
north_sites<-station_sites%>%
  subset (Latitude> 23.43637& Latitude< 66.5)%>%# Polarkreis at 66,5°
  subset(Code %in% nms)
north_nms<- as.vector(north_sites$Code) 
north_sites_plt.14.20<-plt_nms.yrs(sites = north_nms,yrs=c(14,20))+labs(x="",y="CO2_fitted [ppm]")
north_sites%>%subset(Code == "CRV")# Carbon in Arctic Reservoirs Vulnerability
# arctic sites
arctic_sites<-station_sites%>%
  subset ( Latitude> 66.5)%>%# Polarkreis at 66,5°
  subset(Code %in% nms)
arctic_nms<-arctic_sites$Code
arctic_sites_plt.00.21<-plt_nms.yrs(arctic_nms,yrs = c("00","21"))
# sites with more than 500 obs
CO2_Ngreatr<-CO2_data.lst%>% map_dbl(function(x)Nrw= NROW(x))
# low obs sites             
lwobs_nms<-CO2_Ngreatr[CO2_Ngreatr<500]%>% names()#"MEX" "CPT" "BHD" "ABP" "TAC"
# high obs sites exlude CRV (with restricted time range 2015 to 2020) obs to detect changes in arctic carbon reservoir
hghobs_nms<-setdiff(names(CO2_data.lst),lwobs_nms)%>%setdiff("CRV")# 15
high_obs_CO2.plt.00.21<-plt_nms.yrs(hghobs_nms,yrs = c("00","21"))
BRW_SCHIL_HPB_CO2.plt.00.21<-plt_nms.yrs(c("BRW","DEUB004","HPB"),yrs = c("00","21"))
# Southpole
require(readxl)
SPO_1h_werte <- read_excel("~/Desktop/Klima_Energiewende/Daten/SPO_1h_werte.xlsx",
                           sheet = "SPO2005-2021", 
                           col_types = c("text", "numeric", "numeric",
                                         "numeric", "numeric", "skip",
                                         "skip", "numeric", "numeric", "skip",
                                         "skip", "skip", "numeric",
                                         "numeric", "numeric", "skip",
                                         "skip", "skip", "skip"))%>% subset(value>0)

SPO_1h_werte<-SPO_1h_werte%>% mutate(datetime=ymd_h(paste(year,"-",month,"-",day,"  ",hour)))%>%
  dplyr::select(-c(year,month,day,hour,time_decimal))
SPO_1h_werte%>%summary()
SPO_plt<- SPO_1h_werte%>%  ggplot(aes(x= datetime,y= value))+
  geom_point(size = 0.2,alpha= 0.7,shape =1)+
  geom_smooth(method="lm",mapping = aes(x=datetime,y=value),col = "red",data= SPO_1h_werte,linetype=4)
SPO_plt+ ggtitle("CO2-Immission @ Southpole")+
  labs(x= "",y="CO2 [ppm]" )
wk_averages<-SPO_1h_werte%>% mutate(wk = format(datetime, "%U"))%>% split(.$wk)%>%
 map_dbl( ~mean(.$value))
names(wk_averages)                                     
wk_CO2<- tibble(wk = 0:53,
                CO2_mean= wk_averages)
SPO_seasonal_plt<-wk_CO2%>% ggplot(aes(wk,CO2_mean))+geom_point()+
  geom_smooth(col= "red")+ 
  labs(x= "Calendarweek",y= "Mean_CO2 [ppm]")
SPO_seasonal_plt+ggtitle("Annual period of CO2-Immission @ Southpole",
                         subtitle =(" 1-h Observations from 2005 to 2021"))
WK_mean<- function (x){
  Mean<-x%>% mutate(wk = format(datetime, "%U"))%>% 
    split(.$wk)%>%
    map_dbl( ~mean(.$CO2))
  return(Mean)
}

CO2_data.lst[["BRW"]]%>%summary()
BRW_CO2_wk<-WK_mean(CO2_data.lst[["BRW"]])
length(BRW_CO2_wk)#54
BRW_wk_CO2<- tibble(wk = 0:53,
                CO2_mean= BRW_CO2_wk)
BRW_seasonal_plt<-BRW_wk_CO2%>%ggplot(aes(wk,CO2_mean))+geom_point()+
  geom_smooth(col= "red")+ 
  labs(x= "Calendarweek",y= "Mean_CO2 [ppm]")
BRW_seasonal_plt+ggtitle("Annual period of CO2-Immission @ BRW arctic site",
                         subtitle =(" 375825 Observations from 1973 to 2020"))
Annual_plt<- function(site_nm){
  x<- CO2_data.lst[[site_nm]]
  x_wk.mean= WK_mean(x)
  N_wk<- length(x_wk.mean)
  x_wk_CO2 <- tibble(wk= 1:N_wk-1,
                     CO2_mean=x_wk.mean)
  Seasonal_plt<-x_wk_CO2%>%ggplot(aes(wk,CO2_mean))+geom_point()+
    geom_smooth(col= "red")+ 
    labs(x= "Calendarweek",y= "Mean_CO2 [ppm]")+ggtitle(paste("Annual period of CO2-Immission @",site_nm))
  return(Seasonal_plt)
  }
Annual_plt(site_nm =  "BRW")
station_sites%>% subset (Code %in% "BRW")
Annual_plt("MLO")
Annual_plt("SUM")
Annual_plt("ZEP")#Spitzbergen
station_sites%>% subset (Code %in% "ZEP")
Annual_plt("PSA")# no dependence
station_sites%>% subset (Code %in% "PSA")
Annual_plt("MHD")
station_sites%>% subset (Code %in% "MHD") # Mace Head Ireland
Annual_plt("MEX")
Annual_plt("ICE")
Annual_plt("HPB")
Annual_plt("CPT")# no dependence Capepoint
station_sites%>% subset (Code %in% "CPT")
Annual_plt("CHR")# no dependence
station_sites%>% subset (Code %in% "CHR")
Annual_plt("SMO")# inverse
Annual_plt("ABP")#inverse
Annual_plt("SPO")# based on annual data no dependence (but inverse on 1-H data)
Annual_plt("CRV")
station_sites%>% subset (Code %in% "CRV")
Annual_plt("DEUB004")
Annual_plt("DEUB044")
Annual_plt("LMP")
Annual_plt("TAC")# based on little data no dependence
# sites geo location
station_sites%>%head(1)
station_sites%>% subset (Code %in% c("CPT","CHR"))
