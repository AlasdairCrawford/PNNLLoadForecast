#Starting with a clean slate- delete everything
rm(list = ls())
cat('\014')

#Setting Directory 
setwd("/Users/fote910/OneDrive - PNNL/Forecasting/Load Data/OPALCO")


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
CentralLoad2016<-read_excel("OPALCO_Decatur&Center_Load.xlsx", sheet="2016 Combined")
CentralLoad2017<-read_excel("OPALCO_Decatur&Center_Load.xlsx", sheet="2017 - Combined")

CentralLoad<- rbind(CentralLoad2016, CentralLoad2017) 

l<- c("DateTime", "Load")
colnames(CentralLoad)<- l

CentralLoad$DateTime<- as.POSIXct(CentralLoad$DateTime, format='%Y-%m-%d %H:%M:%S', tz="UTC")

write_feather(CentralLoad, "Central.feather")


#OPALCO Total
OPALCO07<- read_excel("OPALCO_Total__Historical_Load.xlsx", sheet="2007")
OPALCO08<- read_excel("OPALCO_Total__Historical_Load.xlsx", sheet="2008")
OPALCO09<- read_excel("OPALCO_Total__Historical_Load.xlsx", sheet="2009")
OPALCO10<- read_excel("OPALCO_Total__Historical_Load.xlsx", sheet="2010")
OPALCO11<- read_excel("OPALCO_Total__Historical_Load.xlsx", sheet="2011")
OPALCO12<- read_excel("OPALCO_Total__Historical_Load.xlsx", sheet="2012")
OPALCO13<- read_excel("OPALCO_Total__Historical_Load.xlsx", sheet="2013")
OPALCO14<- read_excel("OPALCO_Total__Historical_Load.xlsx", sheet="2014")
OPALCO15<- read_excel("OPALCO_Total__Historical_Load.xlsx", sheet="2015")
OPALCO16<- read_excel("OPALCO_Total__Historical_Load.xlsx", sheet="2016")
OPALCO17<- read_excel("OPALCO_Total__Historical_Load.xlsx", sheet="2017")

OPALCO<- rbind(OPALCO07, OPALCO08, OPALCO09, OPALCO10, OPALCO11, OPALCO12, OPALCO13, OPALCO14, OPALCO15, OPALCO16, OPALCO17)

colnames(OPALCO)<- l

OPALCO$DateTime<- as.POSIXct(OPALCO$DateTime, format='%Y-%m-%d %H:%M:%S', tz="UTC")

write_feather(OPALCO, "OPALCOTotal.feather")
