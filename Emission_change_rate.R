library(readxl)
url<-"https://github.com/owid/co2-data"
browseURL(url)# loads data as xlsx to downloads
# file moved to desktop/Klima_Energiewende/Daten
path<-"~/desktop/Klima_Energiewende/Daten/owid_co2_data.xlsx"
CO2_world<-read_xlsx(path)
CO2_world%>% summary()
CO2_world$iso_code%>%unique()
CO2_world%>%tail()
CO2_world%>% subset(iso_code=="OWID_WRL")%>%ggplot(aes(x=year,y=co2))+
  geom_line()+
  labs(y= "CO2 [Mio.t]")+
  ggtitle("World CO2 Emissions",
          subtitle = "Mio. t")
CO2_world<-CO2_world%>% subset(iso_code=="OWID_WRL")%>% select(year,co2)
CO2_world%>% mutate(led= lead(CO2))
CO2<- tibble(year=CO2_world$year,
           CO2=CO2_world$co2,
           CO2.ld=lead(CO2_world$co2))
CO2<-CO2%>% mutate(chg=-CO2+CO2.ld)
summary(CO2)
head(CO2)
CO2%>% na.omit%>% ggplot(aes(x=year,y=chg))+ # difference leading - act value
  geom_point(size=0.2)+
  geom_smooth(method ="gam",formula= y~s(x,k=20),col = "red")+
  labs(y= "change p.a.[Mio. t]" )+
  ggtitle("Worldwide CO2 Emissions ",
          subtitle = "Change per annum and trend")
