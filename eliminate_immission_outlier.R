CO2_data.lst<-readRDS("data/NOAA_CO2_data.rds")
nmdt<-function(x) x<-mutate(x,datetime=as.numeric(datetime))
CO2_data.nmdt<-CO2_data.lst%>% map_df(nmdt)
CO2_lin_mdl<-CO2_data.nmdt%>%dplyr::select(datetime,CO2,site)%>%
  split(.$site)%>%
  map(function(x) lm(x$CO2~x$datetime))
CO2_lin_fit<-CO2_lin_mdl%>% map(augment)%>% 
  map(rename,
      c("CO2"=`x$CO2`),
      c("datetime"=`x$datetime`),
      c("CO2_fit" = ".fitted"),
      c("CO2_resd"=".resid"))
CO2_lin_fit.tbl<- tibble()
for (nm in nms){
  df<-mutate(CO2_lin_fit[[nm]],
             Name =nm,
             datetime=as.POSIXct(datetime,origin="1970-01-01",
                                 tz = "UTC"))
  CO2_lin_fit.tbl<- bind_rows(CO2_lin_fit.tbl,df)
}
CO2_resd<-CO2_lin_fit.tbl%>% dplyr::select(-c(.hat,CO2_fit,.cooksd,.std.resid,.sigma))
MLO_sd<-CO2_resd%>% filter(Name=="MLO")%>% summarise(SD=sd(CO2_resd))
CO2_resd%>% filter(Name=="MLO")%>%summarise(range(CO2_resd))# -23.4 to 65.4
CO2_resd%>% filter(Name=="MLO")%>%subset(abs(CO2_resd)<2*MLO_sd)
elim_outlier<- function(x){ var= x$CO2_resd
                            SD= sd(var)
                            x=subset(x,abs(x$CO2_resd)< 2*SD)
                            return(x)
}
x<-filter(CO2_resd,Name=="MLO")
summary(x)# 3028 obs
x<-elim_outlier(x)
summary(x)             
CO2_wout<-CO2_resd%>% split(.$Name)%>% map_df(elim_outlier)
CO2_wout$CO2_resd%>% sd()
CO2_wout%>% filter(Name== "SPO")%>%ggplot(aes(x=datetime,y=CO2_resd))+
  geom_point(size=0.2)
