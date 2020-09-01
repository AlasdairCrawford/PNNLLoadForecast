library(lubridate)
library(feather)
library(dplyr)
library(jsonlite)

LastTimeObserved<-read_feather("Load Data\\Energy NW\\RichlandWeather.feather")$time%>%min%>%as_datetime()%>%with_tz%>%as.Date-1

Times<-seq(LastTimeObserved,"2015-01-01"%>%as.Date,by=-1)%>%
  as.character(format="%Y-%m-%dT00:00:00")

APIKey<-"C:\\Users\\craw038\\Documents\\DarkSkyAPI.txt"%>%read.table()%>%.$V1%>%as.character

SummaryTable<-"C:\\Users\\craw038\\Documents\\PNNLLoadForecast\\DataSummary.xlsx"%>%read_excel()

SummaryTable%>%
  filter(Site=="EnergyNW")%>%
  select(Lat,Long)%>%
  t%>%as.numeric->Location

data<-read_feather("RichlandWeather.feather")
for(i in Times){
  print(i)
  jsonfile<-paste0("https://api.darksky.net/forecast/",
                   APIKey,
                   "/",
                   paste0(Location,collapse=","),
                   ",",
                   i,
                   "?exclude=currently,flags,minutely,daily,alerts")%>%
    fromJSON()
  
  data<-jsonfile$hourly$data%>%bind_rows(data)
}

data%>%write_feather("Load Data\\Energy NW\\RichlandWeather.feather")