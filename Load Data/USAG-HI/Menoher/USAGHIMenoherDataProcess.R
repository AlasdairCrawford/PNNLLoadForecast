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


MenoherLoad<-read.csv("Load Data//USAG-HI//Menoher//MenoherLoad.csv", header=T,stringsAsFactors = FALSE)%>%
  transmute(DateTime=as.POSIXct(paste(`ï..Date`,Time), format='%m/%d/%y %H:%M',tz="Pacific/Honolulu"),
            Load=Load%>%as.numeric)%>%
  mutate(DateTime=round_date(DateTime,"hour"))%>%
  group_by(DateTime)%>%
  summarise(Load=mean(Load))


USAGHIWeather<-"Load Data//USAG-HI//USAGHIWeather.feather"%>%
  read_feather%>%
  mutate(Time=as.POSIXct(time,origin="1970-01-01"))

MenoherLoad$Temp<- approx(USAGHIWeather$Time,USAGHIWeather$temperature,MenoherLoad$DateTime)$y

MenoherLoad%>%
  group_by(DateTime)%>%
  summarise(Load=sum(Load),
            Temp=mean(Temp))%>%
  mutate(DateTime=round_date(DateTime,"1 hour"))%>%
  group_by(DateTime)%>%
  summarise_all(mean)%>%
  mutate(Hour=hour(DateTime))%>%ggplot(aes(x=Temp,y=Load))+geom_point()+facet_wrap(.~Hour,scales="free")

write_feather(MenoherLoad, "Load Data\\ProcessedData\\Menoher.feather")
