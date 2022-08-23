#timeseries decomposition
url_ts.deco<-"https://anomaly.io/seasonal-trend-decomposition-in-r/index.html"
browseURL(url_ts.deco)
# example data
library(tidyverse)
x<- seq(0,10,0.01)
y1 <- sin(2*x*pi)+rnorm(length(x))
y2<- sin(3*x*pi)
y3<- cos(x*pi)
dt<- tibble(x,y1,y2)%>% as.ts()
dts<- ts(y1*y2)
dt%>% ggplot(aes(x,y1))+geom_line(size=0.1)+
geom_line(aes(x,y1*y2),col= "red")+
geom_line(aes(x,y3))
# data from tutorial
#additive 
install.packages("fpp")
library(fpp)
data(ausbeer)
timeserie_beer = tail(head(ausbeer, 17*4+2),17*4-4)
plot(as.ts(timeserie_beer))
#multiplicative
install.packages("Ecdat")
library(Ecdat)
data(AirPassengers)
timeserie_air = AirPassengers
plot(as.ts(timeserie_air))
#detect seasonality
# Install and import TSA package
install.packages("TSA")
library(TSA)
dt<-read.csv("~/downloads/20131120 - Tabellenblatt1.csv",skip = 0)%>%na.omit()
tail(dt)
dt<- dt%>%mutate(Datum=dmy(Datum))
require(xts)
require(lubridate)
ts<- xts(x=dt$Visite,order.by=dt$Datum)
# compute the Fourier Transform
p = periodogram(ts)
max(p$spec)

dd = data.frame(freq=p$freq, spec=p$spec)

freq.3mx<-dd%>%arrange(desc(spec))%>%head(3)
time<- 1/freq.3mx$freq # 7 days, 1 year, 2 years
# second example
p2<-periodogram(dts)
pp<- tibble(freq= p2$freq,spec=p2$spec)
pp%>% ggplot(aes(x=freq,y= spec))+
  geom_line()
pp%>% arrange(desc(spec))%>% head(3)
