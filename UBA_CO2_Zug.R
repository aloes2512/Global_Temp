url_DEUB044<-"~/Desktop/Klima_Energiewende/Daten/CO2_STD_ZU_2002-2020"

require(tidyverse)
UB2019SCO2_Zug<- read_delim("~/Desktop/Klima_Energiewende/Daten/CO2_STD_ZU_2002-2020/UB2019SCO2_inv1SMW_20220512.csv",
                            delim = ";",quote = "'", escape_double = FALSE, trim_ws = TRUE)%>% subset(Wert > 0)
summary(UB2019SCO2_Zug)
head(UB2019SCO2_Zug,2)
require(lubridate)
UB2019SCO2_Zug<- UB2019SCO2_Zug%>% mutate(datetime= paste(Datum,Uhrzeit),
                                          datetime = ymd_hms(datetime),datetime=floor_date(datetime,unit = "hour"))
UB2019SCO2_Zug%>%ggplot(aes(x=datetime,y=Wert))+
  geom_point(size = 0.1,alpha=0.1)
# General function
"UB2019SCO2_inv1SMW_20220512.csv"
# "UB2019SCO2_inv1SMW_20220512.csv"
Zug_read<- function(yr){
  File<- paste0("UB",yr,"SCO2_inv1SMW_20220512",".csv")#CO2_STD_ZU_2002-2020
  path<- "~/Desktop/Klima_Energiewende/Daten/CO2_STD_ZU_2002-2020"
  dfr<-read_delim(file.path(path,File),show_col_types = F,
                  delim = ";",quote = "'", escape_double = FALSE, trim_ws = TRUE)%>% subset(Wert > 0)%>%
    mutate(datetime= paste(Datum,Uhrzeit),
           datetime = ymd_hms(datetime),datetime=floor_date(datetime,unit = "hour"))
  return(dfr)
}
Zug_file.list<-c(2002:2020)
Zug_CO2<- tibble()
for (yr in Zug_file.list) {
  tbl<- Zug_read(yr)
  Zug_CO2<-bind_rows(Zug_CO2,tbl)
}

Zug_CO2%>%head(1)
Zug_CO2<-Zug_CO2%>%dplyr::select("site"=Station,datetime,"CO2"=Wert)
Zug_CO2$lat<-47.421066
Zug_CO2$lon<-10.985365
Zug_CO2$alt<-2656
summary(Zug_CO2)
Zug_CO2%>%ggplot(aes(x=datetime,y=CO2))+
  geom_point(size=0.2,alpha= 0.2)
CO2_data.lst<- readRDS("~/projects/Global_Temp/data/NOAA_data.rds")
summary(CO2_data.lst)
CO2_data.lst$DEUB044<-Zug_CO2
saveRDS(CO2_data.lst,file = "~/projects/Global_Temp/data/NOAA_data.rds")
# use purrr package
require(purrr)
Zug_path<-"~/Desktop/Klima_Energiewende/Daten/CO2_STD_ZU_2002-2020"
Zug_files<-list.files(Zug_path,pattern = "csv")#19 files
# read in data
read_fnct2<- function(.){
  nm<-str_extract(.,"^UB(.){4}")
  fp<-file.path(Zug_path,.)
  df<-read.csv2(fp,na= c("-999","-999.9"),quote = "'")
  df<-df%>% mutate(Datum=ymd(Datum),name=nm)%>%
    dplyr::select(-Komponente)%>%rename("CO2"=Wert)
  return(df)
}
Zug_dat.m<-map(Zug_files,read_fnct2)
names(Zug_dat.m)<-Zug_files%>% str_extract("^UB(.){4}")%>% paste0("Zug")
summary(Zug_dat.m)
Zug_dat.m[[1]]%>% head()
# make df direct
Zug_dat.df<-map_df(Zug_files,read_fnct2)
