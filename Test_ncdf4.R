head(nc_data_list["amt"])
df<-nc_data_list[["amt"]]%>% group_by(tnm)%>%nest()
df$tnm%>%length()#123354
df$tnm%>%as.POSIXct(origin="1970-01-01",tz="UTC")%>% head()
df[[1,2]]%>% summarise(CO2=mean(CO2))
df$data[[1]]%>%class()
for(i in 1:3)df[[i,2]]%>% print()
df%>% map_df(2)
rownames(df)<-df$tnm
map_df(df,2)$data%>% .$CO2%>% mean()
x<- vector("double")
for(i in 1:3)x[i]<- map_df(df,i)$data%>% .$CO2%>% mean()
x
summarise(df$data[[1]],CO2=mean(CO2))
sm<- function(i){summarise(df$data[[i]],CO2=mean(CO2,na.rm=TRUE))}
sm(1)
sm(2)
res<-vector("double",length= 123354)
for(i in 1:123354){
  res[i]= map_df(df,i)$data%>% .$CO2%>% mean()}
summary(res)
