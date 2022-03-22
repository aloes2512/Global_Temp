url_path<-"https://noamross.github.io/gams-in-r-course/chapter2"
browseURL(url_path)
head(Temp_SP_dat,1)
Temp_SP_dat<-Temp_SP_dat%>% mutate(mnth=format(Datum,"%m"),mnth=as.numeric(mnth))
Temp_SP_mdl<- gam(data=Temp_SP_dat,
                  formula=Temp~ s(datum,k=12)+s(SP,k=12)+s(mnth), method="REML")
summary(Temp_SP_mdl)
tidy(Temp_SP_mdl)
glimpse(Temp_SP_mdl)
#Visualize using standart plot
# plot with mgcViz
vignette("mgcviz","mgcViz")
#convert model to mgcViz
b<-getViz(Temp_SP_mdl)
listLayers(o)
o<-plot(sm(b,1))
o+l_fitLine(col="red")+
  l_ciLine(mul = 5, colour = "black", linetype = 2)
  plot(sm(b,2))+l_ciLine(col="red",linetype= 2)+
  l_fitLine(col="black")
print(plot(b,allTerms = T),pages=1)

# plot using ggplot
Temp_SP_mdl$linear.predictors# is equivalent to gam smooth k=12
Temp_SP_dat$pred<-Temp_SP_mdl$linear.predictors
head(Temp_SP_dat,1)
Temp_SP_dat%>% ggplot(aes(x=Datum))+
   geom_smooth(method= "lm",aes(x=Datum,y=pred),col= "black")+
  geom_smooth(method = "auto",aes(x=Datum,y= Temp), formula= y ~ s(x,k=10),
              data = Temp_SP_dat)+
  geom_smooth(method = "auto",aes(x=Datum,y=SP/10),formula= y ~ s(x,k=5),
  data=Temp_SP_dat,col ="RED")

