library(tidyverse)
install.packages("dslabs")
library(dslabs)



#Use the gapminder dataset that comes with dslabs as an example
glimpse(gapminder)

gapminder_nest <- gapminder %>% 
  group_by(country) %>% 
  nest()%>%
  mutate(splined =map(data, ~mgcv::gam(population ~ s(year, k=5, bs="tp"), data=.x))) %>%
  mutate(augment_spline= map(splined, broom::augment))%>%
  unnest(augment_spline)%>%
  dplyr::select(country, population,.fitted,.se.fit)
gapminder_nest%>%summary()
# select one country
german_pop<-gapminder%>% subset(country == "Germany")%>% dplyr::select(year,population)
german_pop<-german_pop%>% subset(year<2016)
pop_mdl<-german_pop%>% mgcv::gam(population ~ s(year, k=5, bs = "tp"),data= .)
require(broom)
pop_mdl%>% augment()%>% 
  dplyr::select(year,population,fit=.fitted, se= .se.fit)%>% ggplot(aes(x=year))+
  geom_line(aes(y= fit))+
  geom_line(aes(y=fit+se),col ="red",linetype= 3)+
  geom_line(aes(y=fit-se),col ="red",linetype= 3)
  
