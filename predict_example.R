url_tidy_mdls<-"https://www.tidyverse.org/tags/tidymodels/"
browseURL(url_tidy_mdls)
library(broom)
install.packages("modeltests")
library(modeltests)
fit <- lm(mpg ~ ., mtcars)
tidy(fit)
td <- tidy(fit)
(check_tidy_output(td))#TRUE
# Temp_SP.mdl from "Temp_SP_model_0.R"
Temp_SP.mdl%>%summary#
td<-tidy(Temp_SP.mdl)
augmnt<-augment(Temp_SP.mdl)
check_tidy_output(augmnt)
# sample to fit
Temp_dat%>% summary()
Temp_smpl<-Temp_dat%>% 
  subset(month %in% c("Jan","Feb"))%>% mutate(datenm=as.numeric(datum))
Test.mdl<-gam( Temp ~ s(datenm,k=12),method = "REML",
                           data= Temp_smpl
                           )
new<-Temp_dat%>% 
  subset(month %in% c("Mar","May"))%>%
  mutate(datenm= as.numeric(datum))
prdct<- predict(Test.mdl,newdata = new)
head(new)
new$prdct<- prdct
ggplot(new, aes(x=datum, y = prdct))+geom_point()
Temp_smpl%>% ggplot(aes(x=datum, y = Temp.fit))+
  geom_line()+
  geom_line(data = new,aes(x=datum, y= prdct),col ="red", size= 0.2)
# subset year
require(lubridate)
Temp_yr<- Temp_dat%>% 
  mutate( yr = format (datum,"%Y"),stichtag= ymd(paste0(yr,"-06-21")))%>%
  subset(datum == stichtag)

