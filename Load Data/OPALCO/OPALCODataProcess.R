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

##Central Load
CentralLoad2016<-read_excel("Load Data\\OPALCO\\OPALCO_Decatur&Center_Load.xlsx", sheet="2016 Combined")
CentralLoad2017<-read_excel("Load Data\\OPALCO\\OPALCO_Decatur&Center_Load.xlsx", sheet="2017 - Combined")

CentralLoad<- rbind(CentralLoad2016, CentralLoad2017) 

l<- c("DateTime", "Load")
colnames(CentralLoad)<- l

CentralLoad$DateTime<- as.POSIXct(CentralLoad$DateTime, format='%Y-%m-%d %H:%M:%S', tz="UTC")

write_feather(CentralLoad, "Central.feather")


#OPALCO Total

OPALCOLoad<-NULL
for(i in 2007:2017){
  
  OPALCOLoad<-read_excel("Load Data\\OPALCO\\OPALCO_Total__Historical_Load.xlsx", sheet=i%>%as.character)%>%
    bind_rows(OPALCOLoad)
}

colnames(OPALCOLoad)<- l

OPALCOLoad%>%
  mutate(DateTime=DateTime%>%force_tz("America/Los_Angeles"))->OPALCOLoad


write_feather(OPALCOLoad, "Load Data\\ProcessedData\\OPALCOTotal.feather")
