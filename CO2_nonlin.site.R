# nonlinear trend
require(mgcv)
CO2_gam.20_mdl<-CO2_data.nmdt%>%dplyr::select(datetime,CO2,site)%>%
  split(.$site)%>%
  map(function(x) mgcv::gam(formula = x$CO2 ~s(x$datetime,k=20)))
summary(CO2_gam.20_mdl)
CO2_gam.20_mdl$BRW$fitted.values
require(broom)
CO2_nonlin.fit[["BRW"]]<-CO2_gam.20_mdl$BRW%>%augment()%>% dplyr::select(datetime=`x$datetime`,
                                                CO2_fit= .fitted,
                                                CO2_se=.se.fit,
                                                CO2_resd= .resid)
CO2_nonlin.fit[["SPO"]]<-CO2_gam.20_mdl$SPO%>%
  augment()%>% 
  dplyr::select(datetime=`x$datetime`, 
                CO2_fit= .fitted, 
                CO2_se=.se.fit, 
                CO2_resd= .resid)
CO2_nonlin.fit[["SPO"]]%>%ggplot(aes(x=datetime,y=CO2_fit))+
  geom_line(col = "black")
CO2_nonlin.plt<-CO2_nonlin.fit[["BRW"]]%>%ggplot(aes(x=datetime,y=CO2_fit))+
  geom_line(col = "black")
  # example MLO
CO2_nonlin.fit[["MLO"]]<-CO2_gam.20_mdl$MLO%>%augment()%>% 
  dplyr::select(datetime=`x$datetime`,
                CO2_fit= .fitted,
                CO2_se=.se.fit,
                CO2_resd= .resid)
CO2_nonlin.plt+geom_point(data = CO2_nonlin.fit[["BRW"]],mapping=aes(x=datetime,y=CO2_fit),
                          col="red",alpha= 0.1)
CO2_data.nmdt%>% subset(site=="MLO")
CO2_gam.20_mdl[["MLO"]]$fitted.values #length()# 3028
CO2_data.lst[["MLO"]]%>% head()
CO2_nonlin<-tibble(CO2_gam.fit=CO2_gam.20_mdl[["MLO"]]$fitted.values)
CO2_data.lst[["MLO"]]<-CO2_gam.20_mdl[["MLO"]]$fitted.values 
CO2_nonlin.fit[["MLO"]]<-CO2_data.lst[["MLO"]]%>%bind_cols(CO2_nonlin)
CO2_nonlin.fit[["MLO"]]%>% ggplot(aes(x=datetime,y= CO2))+
  geom_line(aes(y= CO2_gm.fit), col ="red")+
  geom_smooth(method = "lm", mapping=aes(x=datetime,y= CO2),
              formula= y~ x, data = CO2_nonlin.fit[["MLO"]])
# all 29 sites
CO2_nonlin.fit<- vector("list", length = length(nms))
names(CO2_nonlin.fit)<-nms

for (nm in nms){
  mytbl<-tibble(nlin.fit=CO2_gam.20_mdl[[nm]]$fitted.values)
  CO2_nonlin.fit[[nm]]<-CO2_data.lst[[nm]]%>% bind_cols(mytbl)
}
summary(CO2_nonlin.fit)
CO2_nonlin<-CO2_nonlin.fit%>% map_df(bind_rows,.id = "site")
CO2_nonlin%>% ggplot(aes(x=datetime,y=nlin.fit,col=site))+
  geom_line()
CO2_nonlin.fit[["ZEP"]]%>% ggplot(aes(x=datetime,y=nlin.fit,col=site))+
  geom_line()
