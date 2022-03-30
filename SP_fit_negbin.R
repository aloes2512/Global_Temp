#negbin method taken from url_negbin
url_negbin<-"https://astrostatistics.psu.edu/su07/R/html/mgcv/html/gam.neg.bin.html"
browseURL(url_negbin)
library(MASS) # required for negative binomial families
#  theta ...
negative.binomial(theta = 2,link="log")

b<-gam(data=dfr,y~s(x0)+s(x1)+s(x2)+s(x3),family=negbin(1),method = "REML")

plot(b,pages=4)

print(b)
b<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=negbin(1)) # unknown theta
library("mgcViz")
b<-getViz(b)
plot(b,sm(b,2))

print(b)
SP_mdl.nbin<- mgcv::gam(data=SP_data,formula = SP~s(rwnm,k=60),family=negbin(1),scale=1)
AIC(SP_mdl.nbin)#1] 719405.3
SP_mdl.nbin<- mgcv::gam(data=SP_data,formula = SP~s(rwnm,k=24),family=negbin(1),scale=1)
AIC(SP_mdl.nbin)#[1] 762629.5
SP_data%>% head(2)
require(broom)
SP_mdl.nbin%>%augment()%>%ggplot(aes(x=rwnm,y=.fitted))+geom_line()
SP.mdl<-getViz(SP_mdl.nbin)
plt<- plot(sm(SP.mdl,1) )
dates<-dplyr::select(SP_data,c(date,rwnm))# date,rwnm
SP_fit<- SP_mdl.nbin%>%augment()%>%dplyr::select(rwnm,SP.fit=.fitted)
SP_fitted<-dates%>%left_join(SP_fit)
SP_fitted%>% ggplot(aes(x=date))+
  geom_line(aes(y=SP.fit))

