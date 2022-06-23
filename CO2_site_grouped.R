require(tidyverse)
library(mgcViz)
library(mgcv)
library(broom)
library(lubridate)
CO2_resd.1<-readRDS("~/projects/Global_Temp/data/CO2_lin.resd.rds")
summary(CO2_resd.1)
head(CO2_resd.1)
CO2_resd.1$site%>%unique()# 20 stations
# convert numerical date into POSIXct format
CO2_Resd<-CO2_resd.1%>%
  mutate(datetime= as.POSIXct(dttm,origin="1970-01-01",tz= "UTC"))%>%
  dplyr::select(site,datetime,CO2_rsd=CO2_resd,CO2)
head(CO2_Resd)
# plot deviation from lin.resd
CO2_resd_plt<-CO2_Resd%>% ggplot(aes(x=datetime,y= CO2_rsd,col=site))+
  geom_point(size= 0.2,alpha= 0.2)
CO2_resd_plt+ggtitle("CO2-deviation from trend",
                      subtitle = "20 sites (NOAA + UBA)")+labs(x="",y="ΔCO2 [ppm]")
## combine observations with site coordinates & altitude
station_sites<-readRDS("~/projects/Global_Temp/data/station_sites.rds")
stations<- dplyr::select(station_sites,Code,Name,Latitude,Longitude,Elevation)
CO2_resd.namd<-CO2_Resd%>% left_join(stations,by= c("site"="Code"))
summary(CO2_resd.namd)
# plot arctic sites selected by latitude 
arct_sites<-stations%>%subset(Latitude > 65)
arct_nams<-arct_sites$Code%>% unique()# 12 sites
CO2_red_plt.2<-CO2_resd.namd%>% subset(site %in% arct_nams)%>% 
  ggplot(aes(x=datetime,y= CO2_rsd,col=site))+
  geom_point(size= 0.3,alpha= 0.2)
CO2_red_plt.2+ggtitle("CO2-deviation from trend",
                      subtitle= "site-latitude > 65°")
# select & plot
plt_sel_sites<- function(st,Lt.min,Lt.max){
  seltd_sites<-stations%>%subset(Latitude > Lt.min& Latitude< Lt.max)
  seltd_nms<-seltd_sites$Code%>% unique()
  CO2_plt<-CO2_Resd%>% subset(site %in% seltd_nms)%>% 
    ggplot(aes(x=datetime,y= CO2_rsd,col=site))+
    geom_point(size= 0.6,alpha= 0.5)+
    ggtitle("CO2-deviation from trend",
    subtitle= paste("site:  ",Lt.min,"° <  latitude <",Lt.max,"°"))+
    labs(x="",y= "ΔCO2 [ppm]")
  print(CO2_plt)
}

#antarctic sites: SPO
plt_sel_sites(st=stations,Lt.min = -90,Lt.max = -65)
antarctic_sites<-stations%>%subset(Latitude < -65)# ARH,HBA, MCM,SPO, SYO
# south stations
southern_sites<- stations%>%subset(Latitude>=-90&Latitude< -40)
plt_sel_sites(st=stations,Lt.min = -90,Lt.max = -40)
# northern latitudes Wendekreis N 23,43472°; Polarkreis N 66.5°
northern_sites<- stations%>%subset(Latitude>=23.43&Latitude< 66.5)
plt_sel_sites(st=stations,Lt.min = 23.43,Lt.max = 66.5)
northern.nms<-northern_sites%>% .$Code%>% unique()
names(northern.nms)<-northern.nms
#select sites including CO2 immission
CO2_resd_north<-CO2_Resd%>% subset(site %in% northern.nms)%>% 
  filter(site!="TAC")%>%filter(site!= "DEUB044")
summary(CO2_resd_north)
north_site_nms<-CO2_resd_north$site%>%unique()#"MHD","ICE","HPB","CRV","DEUB004","LMP"
north_sites<-north_site_nms%>% paste(collapse = ", ")
stations%>% subset(Code %in% north_site_nms)
#plt selected northern sites
plt_sel_sites(st=stations,Lt.min = 23.43,Lt.max = 66.5)
#===============================
#plot site selected by Code
site_nms<-CO2_resd$site%>% unique()
sel.sites<- c("MLO","LMP")
plt_sel_sit.nm<- function(sel.sites,obs=CO2_Resd){
  seltd_sites<-stations%>%subset(Code %in% sel.sites)
  Lt.max<- max(seltd_sites$Latitude)
  Lt.min<- min(seltd_sites$Latitude)
  CO2_plt<-obs%>% subset(site %in% sel.sites)%>% 
    ggplot(aes(x=datetime,y= CO2_rsd,col=site))+
    geom_smooth()+
    ggtitle("CO2-deviation from trend",
            subtitle= paste("site:",Lt.min,"° < Latitude <",Lt.max,"°" ))+
    labs(x="",y= "ΔCO2 [ppm]")
  print(CO2_plt)
}
#HPB, MLO Zugspitze, ICE
plt_sel_sit.nm(sel.sites = c("MLO"))
plt_sel_sit.nm(sel.sites = c("HPB"))
plt_sel_sit.nm(sel.sites = c("ICE"))
plt_sel_sit.nm(sel.sites = c("DEUB004","MLO"))

# Zugspitze
CO2_Zugsp_plt<-CO2_resd.namd%>% filter(site== "DEUB044")%>%ggplot(aes(x=datetime,y= CO2_rsd,col=site))+
 geom_point(size= 0.3,alpha= 0.5)+
  geom_smooth(method = "gam",formula = y ~ s(x, k=10),col="black")
CO2_Zugsp_plt+ggtitle("CO2-Immission Zugspitze",
        subtitle = "1 h observations + smoothed:10 Basis-fnct")+
  labs(x= "",y = " ΔCO2 [ppm]")
# equatorial sites
equatorial_sites<-stations%>% subset(abs(Latitude)< 20)
equatorial_site_nms<- equatorial_sites$Code %>%unique() # 41 sites              
CO2_resd.namd%>%subset(site %in% equatorial_site_nms)%>% ggplot(aes(x=datetime,y= CO2_rsd,col = site))+
  geom_smooth()
equ_CO2<-CO2_resd.namd%>%subset(site %in% equatorial_site_nms)%>% filter(site!= "ABP"& site != "CHR")
obs_sites<-equ_CO2$site%>%unique()#"MLO" "MEX" "SMO"
stations%>%subset(Code %in% unique(equ_CO2$site))
plt_sel_sit.nm(sel.sites = c("SMO","MLO"))

# sites with timespan including 1998
obs_range<-CO2_resd.1%>%group_by(site)%>% summarise(start=min(dttm),end=max(dttm))
sel_obs<- obs_range%>% mutate(start=as.POSIXct(start,origin="1970-01-01",tz="UTC"),
                    end =as.POSIXct(end,origin="1970-01-01",tz="UTC"))%>% 
            filter(start < ymd("1990-01-01") & end> ymd("2020-01-01"))
sel.site.nms<- sel_obs[["site"]]%>% paste(collapse = ", ")
my.sites<- sel_obs[["site"]]
plt_sel_sit.nm(sel.sites = my.sites)
# Modify function for various  basis functions
plt_sel_site.K<- function(sel.sites,obs=CO2_Resd, k = K){
  seltd_sites<-stations%>%subset(Code %in% sel.sites)
  Lt.max<- max(seltd_sites$Latitude)
  Lt.min<- min(seltd_sites$Latitude)
  my.obs<- obs %>% subset(site %in% seltd_sites$Code)
  CO2_plt<-my.obs%>% 
    ggplot(aes(x=datetime,y= CO2_rsd))+
    geom_smooth(method = "gam",
                data = my.obs,
                mapping = aes(x=datetime,y= CO2_rsd,col = site),formula = y ~ s(x, k= k))+
    ggtitle(paste("CO2-deviation from trend",k,"Basis functs"),
            subtitle= paste("site:", Lt.min,"°< Latitude <",Lt.max,"°"))+
    labs(x="",y= "ΔCO2 [ppm]")
  print(CO2_plt)
}
plt_sel_site.K(sel.sites = my.sites,k=6)
plt_sel_site.K(sel.sites = my.sites,k=10)
plt_sel_site.K(sel.sites = my.sites,k=4)
