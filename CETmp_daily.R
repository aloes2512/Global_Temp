library(tidyverse)

url <- "https://www.metoffice.gov.uk/hadobs/hadcet/cetdl1772on.dat"

df_raw <- read_tsv(url, col_names = FALSE)
head(df_raw,2)
class(df_raw)
names(df_raw)# "X1"
div_10 <- function(...){
  .../10
}
df_1772 <- df_raw %>% 
  separate(X1, into = c("year", "day", month.abb)) %>% 
  mutate_at(.vars = vars(year:Dec), as.double) %>% 
  mutate_at(.vars = vars(Jan:Dec), div_10) %>% 
  pivot_longer(cols = Jan:Dec, names_to = "month", values_to = "temp") %>% 
  filter( temp < 99) %>% 
  mutate(date = glue::glue("{year}-{month}-{day}"),
         date = as.Date(date, format = "%Y-%b-%d")) %>% 
  arrange(date)

df_1772%>% ggplot(aes(x=date))+
  geom_smooth(aes(y=temp))
names(df_1772)
df_1772<-df_1772%>%dplyr::select(-c(year,day,month))%>% mutate(Nr=1:NROW(df_1772))
#========
df_model.36<-gam(data=df_1772,formula=temp ~ s(Nr,k=36), method="REML")
AIC(df_model.36)#552204.4
df.model.24<-gam(data=df_1772,formula=temp ~ s(Nr,k=24), method="REML")
AIC(df.model.24)
require(broom)#552283.2
df.model.48<-gam(data=df_1772,formula=temp ~ s(Nr,k=48), method="REML")
AIC(df.model.48)#552147

DFTemp.48<-df.model.48%>%augment()%>% dplyr::select(Nr,Tmp=.fitted,se=.se.fit)
dim(DFTemp.48) #[1] 91370     3
dim(df_1772)#91370 3
head(df_1772)
tail(df_1772)#  7.4 2022-02-28 60370
DFTemp.48$datum<-df_1772$date
Datum<-seq(ymd("1772-01-01"),ymd("2022-02-28"),by ="1 day")
Date<- tibble(datum=Datum)
Date%>%head()
DFTemp<-Date%>% left_join(DFTemp,by= "datum")
head(DFTemp)
DFTemp.48%>%ggplot(aes(x=datum))+
  geom_line(aes(y=Tmp))+
  coord_cartesian(ylim = c(9.5,11.5))

