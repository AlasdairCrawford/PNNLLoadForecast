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

write_feather(AdminLoad, "Load Data\\ProcessedData\\EWEBAdmin.feather")
write_feather(FleetLoad, "Load Data\\ProcessedData\\EWEBFleet.feather")
write_feather(WHLoad, "Load Data\\ProcessedData\\EWEBWH.feather")
