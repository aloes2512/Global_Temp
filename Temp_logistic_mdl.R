# Logistic model
data_path<- "~/Desktop/Klima_Energiewende/Daten/Central_England_Temp.rds"
dat<-readRDS(data_path)
Temp_fit<- readRDS(file = "Temp_dat_fitted.rds")
head(Temp_fit)
require(tidyverse)
require(lubridate)
Temp_fit_1950<- Temp_fit%>% subset(datum>ymd("1950-01-01"))
summary(Temp_fit_1950)
# local minimum between 1950 and now
loc_min<-which.min(Temp_fit_1950$Tenp)
Temp_fit_1950[loc_min,]# 1981-12-12  Temp =-8.5  fit=9.63
Temp_gr10<-Temp_fit_1950 %>% subset(fit>10.1& fit< 10.4)
Temp_gr10%>% ggplot(aes(x=datum,y= fit))+
  geom_line()
Temp_chg<-Temp_gr10%>%
  mutate(dT_dt= (lead(fit)-fit))
Temp_chg%>%  ggplot(aes(x= fit,y= dT_dt))+
  geom_line()

Temp_gr10[1,]#1999-03-13;Temp =9.1;fit=10.1;se=0.0551
chg_mdl<-lm(dT_dt ~ fit,data = Temp_chg)
Temp_mx<- -chg_mdl$coefficients[[1]]/chg_mdl$coefficients[[2]]#10.69916
# plot asymptotic limit
ylab= expression(Temperature~(degree*C))
Temp_plt<-Temp_fit %>% ggplot(aes(x=datum))+
  geom_line(aes(y= fit),col = "blue")+
  geom_line(aes(y= fit+se),col= "red")+
  geom_line(aes(y= fit-se),col= "red")+
  ggtitle("Central England Mean Temperatures",
          subtitle = " with 95% confidence intervalls")+
  labs(x="",y= ylab)+
  geom_hline(yintercept = Temp_mx, linetype = 3,size= 0.5)
Temp0<-Temp_gr10[[1,"fit"]]
t0<-Temp_gr10[[1,"datum"]]
C<- Temp_mx/Temp0-1%>%as.numeric()# 0.05932029
k<-chg_mdl$coefficients[[2]]#-0.000125
Temp_mx # 10.69916
# built model for selected date starting with local minimum
Temp_fit_1950%>%head()
N<-NROW(Temp_fit_1950)# 26297
local_data<-Temp_fit_1950[c(loc_min:N),]# loc_min = 11668
head(local_data,1)# 1981-12-12
C<-Temp_mx/local_data[[1,3]]-1
logit<- tibble(t= 1:NROW(local_data),
               temp_mdl=Temp_mx/(1+C*exp(t*k)))
logit$datum<-local_data$datum
summary(logit)
logit%>% ggplot(aes(x=datum,y=temp_mdl))+
  geom_line()
# model to fitted
Temp_plt+ geom_line(data = logit,aes(x=datum,y= temp_mdl))
# extrapolate logit model

summary(logit2)


Temp_plt+geom_line(data = logit2,aes(x=datum,y= temp_mdl))
