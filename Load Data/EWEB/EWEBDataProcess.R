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


AdminLoad<-read_excel("Load Data\\EWEB\\EWEB.xlsx", sheet="Admin Building")
FleetLoad<-read_excel("Load Data\\EWEB\\EWEB.xlsx", sheet="Fleet Building")
WHLoad<-read_excel("Load Data\\EWEB\\EWEB.xlsx", sheet="Warehouse")

AdminLoad$DateTime<- as.POSIXct(AdminLoad$DateTime, format='%Y-%m-%d %H:%M:%S', tz="America/Los_Angeles")
FleetLoad$DateTime<- as.POSIXct(FleetLoad$DateTime, format='%Y-%m-%d %H:%M:%S', tz="America/Los_Angeles")
WHLoad$DateTime<- as.POSIXct(WHLoad$DateTime, format='%Y-%m-%d %H:%M:%S', tz="America/Los_Angeles")



EWEBWeather<-"C:\\Users\\craw038\\Documents\\PNNLLoadForecast\\Load Data\\EWEB\\EWEBWeather.feather"%>%
  read_feather()%>%
  mutate(Time=as.POSIXct(time,origin="1970-01-01"))


AdminLoad$Temp<- approx(EWEBWeather$Time,EWEBWeather$temperature,AdminLoad$DateTime)$y
FleetLoad$Temp<- approx(EWEBWeather$Time,EWEBWeather$temperature,FleetLoad$DateTime)$y
WHLoad$Temp<- approx(EWEBWeather$Time,EWEBWeather$temperature,WHLoad$DateTime)$y

bind_rows(AdminLoad,
          FleetLoad,
          WHLoad)%>%
  group_by(DateTime)%>%
  summarise(Load=sum(Load),
            Temp=mean(Temp))%>%
  mutate(DateTime=round_date(DateTime,"1 hour"))%>%
  group_by(DateTime)%>%
  summarise_all(mean)%>%
  mutate(Hour=hour(DateTime))%>%ggplot(aes(x=Temp,y=Load))+geom_point()+facet_wrap(.~Hour,scales="free")


write_feather(AdminLoad, "Load Data\\ProcessedData\\EWEBAdmin.feather")
write_feather(FleetLoad, "Load Data\\ProcessedData\\EWEBFleet.feather")
write_feather(WHLoad, "Load Data\\ProcessedData\\EWEBWH.feather")
