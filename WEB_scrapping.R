#Web scrapping :extracting data from a website.
url <- paste0("https://en.wikipedia.org/w/index.php?title=", "Gun_violence_in_the_United_States_by_state&oldid=843135608")
browseURL(url)
# there is no link to a data file
#text is code written in hyper text markup language (HTML)
#rvest: The first step using this package  to import the webpage into R:
library(tidyverse) 
library(rvest)
h <- read_html(url)
str(h)
class(h)#[1] "xml_document" "xml_node" 
#code with downloaded code
html_text(h)# a character file of length 1
#extract nodes named "table"
tab <- h %>% html_nodes("table")# node set with 3 tables
# we are interested in tab[[2]]

tab <- tab[[2]] %>% html_table 
class(tab)# data.frame

tab<-tab %>% setNames(c("state", "population", "total", "murder_rate"))
head(tab,2)
#remove the commas and turn characters into numbers
# try with noaa web page
## USE OF NOAA GML DATA
#monthly mean carbon dioxide measured at Mauna Loa Observatory, Hawaii
url_data<-"https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_weekly_mlo.csv"
browseURL(url_data)
Mauna_Loa_CO2<-read.table(url_data,skip=42,header = T,sep=",",na= -999.99)
summary(Mauna_Loa_CO2)# from 1974 to 2022
require(lubridate)
Mauna_Loa_CO2<-unite(Mauna_Loa_CO2,datetime,year,month,day,sep = "-")%>%
  mutate(site="MLO",datetime=ymd(datetime))%>%dplyr::select(site,datetime,CO2=average)
Mauna_Loa_CO2%>%ggplot(aes(x= datetime,y=CO2))+
  geom_line()+
  ggtitle("CO2_weekly.av @Mauna Loa")+
  labs(x="",y="CO2[ppm]")
#global monitoring data
url_gml<-"https://gml.noaa.gov/dv/data/"
browseURL(url_gml)
h<-read_html(url_gml)
class(h)# "xml_document" "xml_node"
tab <- h %>% html_nodes("table")
tab[[1]]%>% html_table()# 50 datasets at page ID =1
class(tab)# nodeset
url_gml.68<-"https://gml.noaa.gov/dv/data/index.php?pageID=68"
browseURL(url_gml.68)
