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

HGELoad<-read_excel("Load Data\\HG&E\\HG&ELoad.xlsx", sheet="Sheet1")%>%
  transmute(DateTime=as.POSIXct(Time, format='%Y/%m/%d %H:%M', tz="America/New_York"),
            Load)

HGEWeather<-"Load Data//HG&E//HGEWeather.feather"%>%
  read_feather%>%
  mutate(Time=as.POSIXct(time,origin="1970-01-01"))

HGELoad$Temp<- approx(HGEWeather$Time,HGEWeather$temperature,HGELoad$DateTime)$y

HGELoad%>%
  group_by(DateTime)%>%
  summarise(Load=sum(Load),
            Temp=mean(Temp))%>%
  mutate(DateTime=round_date(DateTime,"1 hour"))%>%
  group_by(DateTime)%>%
  summarise_all(mean)%>%
  mutate(Hour=hour(DateTime))%>%ggplot(aes(x=Temp,y=Load))+geom_point()+facet_wrap(.~Hour,scales="free")

write_feather(HGELoad, "Load Data\\ProcessedData\\HGELoad.feather")

