#Starting with a clean slate- delete everything
rm(list = ls())
cat('\014')


library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)
library(tidyverse)
library(lubridate)
library(data.table)
library(anytime)
library(zoo)
library(chron)
library(feather)

Tyndall<- read.csv("Load Data\\Tyndall\\TyndallLoad.csv", header = T,stringsAsFactors = FALSE)%>%
  transmute(DateTime=paste(ï..Date,Hour)%>%as.POSIXct(format="%m/%d/%y %H",tz="America/New_York"),
            Load=kW.Sum%>%as.numeric)


TyndallWeather<-"Load Data\\Tyndall\\TyndallWeather.feather"%>%
  read_feather()%>%
  mutate(Time=as.POSIXct(time,origin="1970-01-01"))


Tyndall$Temp<- approx(TyndallWeather$Time,TyndallWeather$temperature,Tyndall$DateTime)$y

Tyndall%>%
  group_by(DateTime)%>%
  summarise(Load=sum(Load),
            Temp=mean(Temp))%>%
  mutate(DateTime=round_date(DateTime,"1 hour"))%>%
  group_by(DateTime)%>%
  summarise_all(mean)%>%
  mutate(Hour=hour(DateTime))%>%ggplot(aes(x=Temp,y=Load))+geom_point()+facet_wrap(.~Hour,scales="free",nrow=4)
               

write_feather(Tyndall, "Load Data\\ProcessedData\\Tyndall.feather")
