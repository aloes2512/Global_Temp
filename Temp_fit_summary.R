# data calculated 01Temp_data_mdls.R
head(Temp.fit.36,2)
loc_dat<-Temp.fit.36%>%dplyr::select(datum,Temp.36= Temp.fit)
loc_dat<-loc_dat%>%left_join(dplyr::select(Temp.fit.24,datum,Temp.24=Temp.fit))
loc_dat<-loc_dat%>%left_join(dplyr::select(Temp.fit.48,datum,Temp.48=Temp.fit))
loc_dat<-loc_dat%>%left_join(dplyr::select(Temp.fit.40,datum,Temp.40=Temp.fit))

loc_dat<-loc_dat%>%pivot_longer(-datum,names_to = "mdl",values_to = "Temp")
SP_dat<- readRDS("data/SP_data_mdl.rds")
loc_dat%>% ggplot(aes(x= datum,col=mdl))+
  geom_line(aes(y=scale(Temp)))+
  geom_line(data=SP_dat,aes(x=date,y=scale(SP_NB)),col="red")+
  coord_cartesian(xlim = c(as.Date("1950-06-30"),as.Date("2021-12-31")))+
  ggtitle("Sunspot(SP) vs. CETemp",
  subtitle="smoothed: SP-log negbinom (red),Temp 24 to 48 gaussian")+
  labs(y="Temp/ log SP+6",x="")
