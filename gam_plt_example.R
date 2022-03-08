# yr_dat from full_data_22
require(gam)
yr_dat%>% head()
yr_dat%>% gam::gam(yr_average~ s(Year))
library(gam)
x = 1:10000/1000
y = sin(x)+rnorm(10000,sd=2)
m = gam(y~s(x))
preplot(m)
x = yr_dat$Year
length(x)
y= yr_dat$yr_average
mdl<- gam::gam(y ~s(x))
prplt<-preplot(mdl)
preplt_data<- tibble(x=prplt$`s(x)`$x,
                     y=prplt$`s(x)`$y,
                     se= prplt$`s(x)`$se.y)
preplt_data%>% ggplot(aes(x=x))+
  geom_line(aes(y=y))+
  geom_line(aes(y=se+y),col="red",linetype= 2)+
  geom_line(aes(y= y-se),col="red",linetype= 2)

