#Data Days Model Building, Visualization, and Management
url_lm<-"https://rpubs.com/YTSchipper23/744637"
browseURL(url_lm)
# calculate linear regression
CO2_data.lst<-readRDS(file.path("data","NOAA_data.rds"))
CO2_data.lst[[19]]%>% summary()

length(CO2_data.lst)#19
unique(CO2_data.lst)
names(CO2_data.lst)# 19 sites
summary(CO2_data.lst)
for (nm in names(CO2_data.lst)){
  CO2_data.lst[[nm]]$site = as_factor(CO2_data.lst[[nm]]$site)
}
saveRDS(CO2_data.lst,file.path("data","NOAA_data.rds"))
#============
lin_mdl<- vector("list",length = length(names(CO2_data.lst)))
names(lin_mdl)<- names(CO2_data.lst)
CO2_data.lst[[nm]]# nm =0 DEUB044
for (nm in names(CO2_data.lst)){
  dtm= CO2_data.lst[[nm]]$datetime
  co2= CO2_data.lst[[nm]]$CO2
 lin_mdl[[nm]]= lm(co2~dtm)$coefficients
}
CO2_data.lst[[nm]]# nm = DEUB044
CO2_data.lst[[nm]]%>%dplyr::select(-c(datetime,CO2))%>%group_by(site)%>%nest()
summary(lin_mdl)
require(tidyverse)
lin.mdl<-lin_mdl%>% map_dfr(bind_rows)
lin.mdl$site<-names(CO2_data.lst)%>% as_factor
summary(lin.mdl)
head(lin.mdl)
class(lin.mdl)#tbl
names(lin.mdl)<-c("intcp","slp","site")
lin.mdl.ord<-lin.mdl%>% arrange(dtm)%>% mutate(ord=1:19)

site_parm<-map_df(CO2_data.lst,`[`,c("site","lon","lat","alt"))%>%
  group_by(site)%>% summarise(lon =first(lon),lat=first(lat),alt=first(alt))
summary(site_parm)
tail(site_parm)
names(site_parm)
Lin.mdl<-lin.mdl%>%left_join(site_parm)
#==========
require(mgcv)
negbin.mdl<-gam(data= CO2_data.lst[[1]],formula = CO2~datetime,family = "nb",method="REML")
summary(negbin.mdl)
require(broom)
MLO_fit<-negbin.mdl%>%augment()
MLO_fit$site<-as_factor("MLO")
MLO_fit%>% ggplot(aes(x=datetime,y=CO2))+
  geom_point(size = 0.2)+
  geom_line(aes(y=exp(.fitted)))
# use a function
neg.bin.fnct<- function(df){
  mdl<-mgcv::gam(df,formula = CO2~datetime,family = "nb",method="REML")
  df<-mdl%>%augment()%>%mutate(CO2_fit=exp(.fitted))%>%
    dplyr::select(datetime,CO2,CO2_fit)%>% mutate(site= first(df$site))
  }
xx<-neg.bin.fnct(CO2_data.lst[[2]])
xx%>%ggplot(aes(x=datetime,y=CO2))+
  geom_line(aes(y= CO2_fit))
CO2_data.lst[1:4]%>%map_dfr(neg.bin.fnct)%>% 
  ggplot(aes(x=datetime,y=CO2,col=site))+
  geom_point(size = 0.2)#
CO2_data.lst[5:9]%>%map_dfr(neg.bin.fnct)%>% 
  ggplot(aes(x=datetime,y=CO2,col=site))+
  geom_point(size = 0.2)
CO2_data.lst[12]%>%map_dfr(neg.bin.fnct)%>% 
  ggplot(aes(x=datetime,y=CO2,col=site))+
  geom_point(size = 0.2)
CO2_data.lst[14:19]%>%map_dfr(neg.bin.fnct)%>% 
  ggplot(aes(x=datetime,y=CO2,col=site))+
  geom_point(size = 0.2)
#try selected dframa
df<-CO2_data.lst[[12]]
mdl<-mgcv::gam(df,formula = CO2~datetime,family = "nb",method="REML")



