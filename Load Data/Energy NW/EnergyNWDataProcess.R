
library(dplyr)
library(reshape2)
library(readr)
library(feather)
library(tis)
library(lubridate)
library(readxl)

fileDirectory<-"Load Data\\Energy NW\\"

excelfiles<-fileDirectory%>%list.files(pattern="*.xlsx",full.names = TRUE)
excelfiles<-excelfiles[grepl("COR Hourly Meter Data",excelfiles)]

dfs<-NULL

for(i in 1:length(excelfiles)){
  
  df<-excelfiles[i]%>%
    read_excel(skip=32)%>%
    melt(id.vars="Interval Ending")%>%
    mutate(variable=as.character(variable))%>%
    filter(!grepl("kVar",variable))%>%
    mutate(Index=gsub("[.]","",variable),
           Index=parse_number(Index))
  
  
  tags<-excelfiles[i]%>%
    read_excel(skip=28)%>%
    head(1)%>%
    melt(id.vars="Total")%>%
    .$value%>%
    unique%>%
    sort
  
  tagdf<-data.frame(Tag=tags,Index=df$Index%>%unique%>%sort,stringsAsFactors = FALSE)
  
  df<-merge(df,tagdf)%>%
    select(-variable,-Index)
  
  dfs[[i]]<-df
}

EnergyNWLoad<-dfs%>%
  bind_rows%>%
  transmute(DateTime=`Interval Ending`%>%force_tz("America/Los_Angeles"),
         Load=value,
         Tag)%>%
  na.omit()

EnergyNWLoad%>%
  group_by(DateTime,Tag)%>%
  mutate(Load=(Load)/n())%>%
  group_by(DateTime)%>%
  mutate(Load=sum(Load,na.rm=TRUE))%>%
  filter(Tag==min(Tag))%>%
  select(-Tag)->EnergyNWLoad
RichlandWeather<-"C:\\Users\\craw038\\OneDrive - PNNL\\Documents2\\LoadForecast\\RichlandWeather.feather"%>%read_feather()%>%
  mutate(Time=as.POSIXct(time,origin="1970-01-01"))

EnergyNWLoad$Temp<-approx(RichlandWeather$Time,RichlandWeather$temperature,EnergyNWLoad$DateTime)$y

EnergyNWLoad%>%write_feather("Load Data\\ProcessedData\\EnergyNW.feather")