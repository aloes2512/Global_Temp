# NOAA South pole data
library(readxl)
library(lubridate)
dat_path<-"~/Desktop/Klima_Energiewende/Daten/CO2_STD_SC_1972-2019"
SPO_CO2_conti_1<- read_excel("~/Desktop/Klima_Energiewende/Daten/CO2_STD_SC_1972-2019/SPO_CO2_conti.xlsx",
col_types = c("text", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "text", "text","text","text","text","text","text","text","text","text","text"))
sel_nm<-names(SPO_CO2_conti_1)[c(1:5,9,13:15)]
SPO_CO2_h_2020.12<-SPO_CO2_conti_1%>%dplyr::select(all_of(sel_nm))
SPO_CO2_h_2020.12<-SPO_CO2_h_2020.12%>% mutate(datetime=paste(year,"-",month,"-",day," ",hour),
                               datetime= ymd_h(datetime),
                               CO2=as.numeric(value))%>% subset(CO2>0)
summary(SPO_CO2_h_2020.12)
#=========
SPO_CO2_conti_2<- read_xlsx(file.path("~/Downloads","SPO_CO2_conti.xlsx"),
                             sheet=4,
                             col_names = T,
                             col_types = c("text", "numeric", "numeric",
                                           "numeric", "numeric", "numeric",
                                           "numeric", "numeric", "text", "text","text","text","text","text","text","text","text","text","text"))
head(SPO_CO2_conti_2,2)
tail(SPO_CO2_conti_2,2)
sel_nm<-names(SPO_CO2_conti_2)[c(1:5,9,13:15)]
SPO_CO2_h_1980.1<-SPO_CO2_conti_2%>%dplyr::select(all_of(sel_nm))
SPO_CO2_h_1980.1<-SPO_CO2_h_1980.1%>% mutate(datetime=paste(year,"-",month,"-",day," ",hour),
                               datetime= ymd_h(datetime),
                               CO2=as.numeric(value))%>% subset(CO2>0)
summary(SPO_CO2_h_1980.1)

# Import Juni 1975
SPO_CO2_conti_3<- read_xlsx(file.path("~/Downloads","SPO_CO2_conti.xlsx"),
                             sheet=3,
                             col_types = NULL)
sel_nm<-names(SPO_CO2_conti)[c(1:5,9,13:15)]

                          
SPO_CO2_h_1975.6<-SPO_CO2_conti_3%>%dplyr::select(all_of(sel_nm))
SPO_CO2_h_1975.6<-SPO_CO2_h_1975.6%>% mutate(datetime=paste(year,"-",month,"-",day," ",hour),
                                             datetime= ymd_h(datetime),
                                             CO2=as.numeric(value))%>% subset(CO2>0)
summary(SPO_CO2_h_1975.6)
# mean =329.4
# SPO monthly averages
SPO_CO2_mnth<- read_excel("~/Desktop/Klima_Energiewende/Daten/SPO_CO2_mnth.xlsx",
                           sheet=5,
                           col_types = c("text", "numeric", "numeric",
                                         "numeric", "numeric", "numeric",
                                         "numeric", "numeric", "text", "text","text","text","text","text","text","text","text","text","text"))
SPO_CO2_mnth<-SPO_CO2_mnth%>%dplyr::select(all_of(sel_nm))
SPO_CO2_mnth<-SPO_CO2_mnth%>% mutate(datetime=paste(year,"-",month,"-",day," ",hour),
                                             datetime= ymd_h(datetime),
                                             CO2=as.numeric(value))%>% subset(CO2>0)
summary(SPO_CO2_mnth)
SPO_CO2_mnth%>% ggplot(aes(x=datetime, y=CO2))+
  geom_point(size= 0.2)
NOAA_CO2$SPO<-SPO_CO2_mnth
saveRDS(NOAA_CO2,file = "~/projects/Global_Temp/data/NOAA_data.rds")
path<-"~/projects/Global_Temp/data/"