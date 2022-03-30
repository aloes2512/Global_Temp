library(tidyverse)
# uk mtoffice data are available at url:
url <- "https://www.metoffice.gov.uk/hadobs/hadcet/cetdl1772on.dat"
df_raw <- read_tsv(url, col_names = FALSE)
spec(df_raw)
div_10 <- function(...){
  .../10
}
# NA = -999 at false dates
df_raw[31:32,]# NA == -999 at col 4,6,8,11
require(glue)
df_1772 <- df_raw %>% 
  separate(X1, into = c("year", "day", month.abb)) %>% 
  mutate_at(.vars = vars(year:Dec), as.double) %>% 
  mutate_at(.vars = vars(Jan:Dec), div_10) %>% 
  pivot_longer(cols = Jan:Dec, names_to = "month", values_to = "temp") %>% 
  filter( temp < 99) %>% 
  mutate(date = glue::glue("{year}-{month}-{day}"),
         date = as.Date(date, format = "%Y-%b-%d")) %>% 
  arrange(date)
df_1772<-df_1772%>%dplyr::select(-c(year,day,month))%>% mutate(Nr=1:NROW(df_1772))
require(lubridate)
ylab<- expression(Temperature~(degree*C))
df_1772%>% ggplot(aes(x=date))+
  geom_smooth(aes(y=temp),formula=y ~ s(x, bs="cs",k=48),col= "red")+ 
  geom_smooth(aes(y=temp),formula=y ~ s(x, bs="cs",k=12), col="purple")+ 
  
  labs(x="",y=ylab)+
  ggtitle("Central England Mean Daily Temperatures",
          subtitle = "smoothed with 12 & 48 basis functions")
names(df_1772)
#========
# select full years only
require(lubridate)
df_1772<-df_1772%>% subset(date<ymd("2022-01-01"))
require(mgcv)
df.model.6<-gam(data=df_1772,formula=temp ~ s(Nr,k=6), method="REML")
AIC(df.model.6)#374820.5

df.model.12<-gam(data=df_1772,formula=temp ~ s(Nr,k=12), method="REML")
AIC(df.model.12)#374810
df.model.24<-gam(data=df_1772,formula=temp ~ s(Nr,k=24), method="REML")
AIC(df.model.24)#374775.2
df_model.36<-gam(data=df_1772,formula=temp ~ s(Nr,k=36), method="REML")
AIC(df_model.36)#374765.5
df_model.48<-gam(data=df_1772,formula=temp ~ s(Nr,k=48), method="REML")
AIC(df_model.48)#374760.3 is minimum
require(broom)
dfr_mdl<- function(.mdl){
  .dfr<-.mdl%>%augment()%>% dplyr::select(Tmp=.fitted,se=.se.fit)
  .dfr$datum<-df_1772$date
  return(.dfr)
}
DFTemp.6<- dfr_mdl(df.model.6)
DFTemp.12<- dfr_mdl(df.model.12)
DFTemp.48<-dfr_mdl(df_model.48)
#check model and data df_1772
dim(DFTemp.48) #[1] 60311     3
dim(df_1772)==dim(DFTemp.48)#TRUE
head(DFTemp.48)
range(DFTemp.48$Tmp)#9.860549 11.584374
ylab= expression(Temperature~(degree*C))
CET_plt<-DFTemp.48%>%ggplot(aes(x=datum))+
  geom_line(aes(y=Tmp),col="red")+
  geom_line(aes(y=Tmp+se),linetype = 3)+
  geom_line(aes(y=Tmp-se),linetype = 3)+
  
  coord_cartesian(ylim = c(9.860549,11.584374))+
  ggtitle("Central England Daily Temperatures",
          subtitle = "Mean Trend with  6 & 48 Basis Functions(red)")+
labs(x="",y=ylab)
CET_plt+
  geom_line(aes(x=datum,y=Tmp),col="blue",data=DFTemp.6)
# combine some models
DF.6<-DFTemp.6%>%dplyr::select(datum,Tmp.6=Tmp)
DF.12<-DFTemp.12%>%dplyr::select(datum,Tmp.12=Tmp)
DF.48<-DFTemp.48%>%dplyr::select(datum,Tmp.48=Tmp)
DF<-left_join(DF.6,DF.12)%>%left_join(DF.48)
DF.l<-DF%>% pivot_longer(-datum,names_to = "Temp",values_to = "Grad")
DF.l%>% ggplot(aes(x= datum))+
  geom_line(aes(y= Grad,col = Temp))+
  labs(x="",y=ylab)+
  ggtitle("Central England Mean Temperatures",
          subtitle = "6,12 & 48 basis functions")
ggsave("CET_average_Temp.png")
# change rate
rate<- function(...){
 chg_rt = lead(...)-...
 chg_rt=chg_rt*356
}
DF.chg<-DF%>%
  mutate_at(.vars = vars(Tmp.6:Tmp.48), rate)
names(DF.chg)<-c("datum","chgrt.6","chgrt.12","chgrt.48")
DF_chg.rate<-DF.chg%>%pivot_longer(-datum,names_to = "chg.rate",values_to = "grd.p.a")
DF_chg.rate%>% ggplot(aes(x=datum))+
  geom_line(aes(y=grd.p.a,col=chg.rate))+
  ggtitle("CET Anual Change Rate
             1772 to 2021")+
  labs(x= "", y= expression(changerate~(degree*C/a)))
CETem_data<<-df_1772%>%left_join(DF,by=c("date"="datum"))%>%
  left_join(DF.chg, by=c("date"="datum"))%>%
  dplyr::select(-Nr)
saveRDS(CETem_data, file = "data/CETemp.rds")
