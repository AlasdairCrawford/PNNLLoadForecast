#Starting with a clean slate- delete everything
rm(list = ls())
cat('\014')

#Setting Directory 
setwd("/Users/fote910/OneDrive - PNNL/Forecasting/Load Data/USAG-HI/INSCOM")


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


INSCOM<-read.csv("INSCOMLoad.csv", header=T)

INSCOM$DateTime<- as.POSIXct(INSCOM$DateTime, format='%m-%d-%Y %H:%M', tz="UTC")

write_feather(INSCOM, "INSCOM.feather")
