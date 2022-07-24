#owid-co2-data.csv
#@article{owidco2andothergreenhousegasemissions,
#author = {Hannah Ritchie, Max Roser and Pablo Rosado},
#title = {CO₂ and Greenhouse Gas Emissions},
#journal = {Our World in Data},
#year = {2020},
#note = {https://ourworldindata.org/co2-and-other-greenhouse-gas-emissions}
#}
install.packages("XML")
library(XML)
owid_url<-"https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
co2_worl.data<-read.csv(owid_url)
head(co2_worl.data)
names(co2_worl.data)# measured parameters
require(tidyverse)
co2_total<-co2_worl.data%>%dplyr::select(iso_code,country,year,co2)
co2_total%>%head()
co2_world <-co2_total%>% filter(country== "World")
 co2_em_plt<-co2_world%>%ggplot(aes(x=year, y= co2))+geom_line()+
  geom_smooth(method="gam", formula = y ~s(x,k=6),col = "red")
co2_em_plt+ggtitle("Worldwide CO2 Emissions",
                   subtitle = "source worldbank/githubusercontent/owid")+
  labs(x="",y= "CO2 equivalent [Mio.t]")
