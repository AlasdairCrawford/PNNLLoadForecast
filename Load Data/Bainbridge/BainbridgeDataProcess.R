BainbridgeLoad<-read_excel("Load Data\\Bainbridge\\Bainbridge Substation Load (PSE).xlsx", sheet="All Data",skip=2)%>%
  transmute(DateTime=`Date & Time`%>%force_tz,
            Load=Win+Mur)%>%
  mutate(DateTime=round_date(DateTime,"hour"))%>%
  group_by(DateTime)%>%
  summarise(Load=mean(Load))

BainbridgeWeather<-"C:\\Users\\craw038\\Documents\\PNNLLoadForecast\\Load Data\\Bainbridge\\BainbridgeWeather.feather"%>%
  read_feather()%>%
  mutate(Time=as.POSIXct(time,origin="1970-01-01"))

BainbridgeLoad$Temp<-approx(BainbridgeWeather$Time,BainbridgeWeather$temperature,BainbridgeLoad$DateTime)$y
BainbridgeLoad%>%mutate(Hour=hour(DateTime))%>%ggplot(aes(x=Temp,y=Load))+geom_point()+facet_wrap(.~Hour)

write_feather(BainbridgeLoad,"Load Data\\ProcessedData\\Bainbridge.feather")
