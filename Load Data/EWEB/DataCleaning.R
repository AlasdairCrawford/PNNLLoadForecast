#Starting with a clean slate- delete everything
rm(list = ls())
cat('\014')

#Setting Directory 
setwd("/Users/fote910/OneDrive - PNNL/Forecasting/Load Data/EWEB")


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


AdminLoad<-read_excel("EWEB.xlsx", sheet="Admin Building")
FleetLoad<-read_excel("EWEB.xlsx", sheet="Fleet Building")
WHLoad<-read_excel("EWEB.xlsx", sheet="Warehouse")

AdminLoad$DateTime<- as.POSIXct(AdminLoad$DateTime, format='%Y-%m-%d %H:%M:%S', tz="UTC")
FleetLoad$DateTime<- as.POSIXct(FleetLoad$DateTime, format='%Y-%m-%d %H:%M:%S', tz="UTC")
WHLoad$DateTime<- as.POSIXct(WHLoad$DateTime, format='%Y-%m-%d %H:%M:%S', tz="UTC")

write_feather(AdminLoad, "Admin.feather")
write_feather(FleetLoad, "Fleet.feather")
write_feather(WHLoad, "WH.feather")
