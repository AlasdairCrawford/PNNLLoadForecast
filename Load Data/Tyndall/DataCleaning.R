#Starting with a clean slate- delete everything
rm(list = ls())
cat('\014')

#Setting Directory 
setwd("/Users/fote910/OneDrive - PNNL/Forecasting/Load Data/Tyndall")


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

Tyndall<- read.csv("TyndallLoad.csv", header = T)

Tyndall$Hour<- Tyndall$Hour*3600
Tyndall$Hour<- strftime(as.POSIXct(Tyndall$Hour, origin='1900-01-01 00:00'),format="%H:%M",tz = "GMT")
Tyndall$DateTime <- as.POSIXct(as.character(paste(Tyndall$Date, Tyndall$Hour)), format="%m/%d/%y %H:%M")

Tyndall<- Tyndall[, c(3,4)]                           

colnames(Tyndall)<- c("Load", "DateTime")                               

write_feather(Tyndall, "Tyndall.feather")
