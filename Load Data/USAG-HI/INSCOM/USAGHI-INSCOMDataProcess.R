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


USAGHIINSCOMLoad<-read.csv("Load Data//USAG-HI//INSCOM//INSCOMLoad.csv", header=T,stringsAsFactors = FALSE)%>%
  transmute(DateTime=as.POSIXct(`ï..DateTime`, format='%m/%d/%y %H:%M',tz="Pacific/Honolulu"),
            Load)


USAGHIWeather<-"Load Data//USAG-HI//USAGHIWeather.feather"%>%
  read_feather%>%
  mutate(Time=as.POSIXct(time,origin="1970-01-01"))

USAGHIINSCOMLoad$Temp<- approx(USAGHIWeather$Time,USAGHIWeather$temperature,USAGHIINSCOMLoad$DateTime)$y

USAGHIINSCOMLoad%>%
  group_by(DateTime)%>%
  summarise(Load=sum(Load),
            Temp=mean(Temp))%>%
  mutate(DateTime=round_date(DateTime,"1 hour"))%>%
  group_by(DateTime)%>%
  summarise_all(mean)%>%
  mutate(Hour=hour(DateTime))%>%ggplot(aes(x=Temp,y=Load))+geom_point()+facet_wrap(.~Hour,scales="free")

write_feather(USAGHIINSCOMLoad, "Load Data\\ProcessedData\\INSCOM.feather")
