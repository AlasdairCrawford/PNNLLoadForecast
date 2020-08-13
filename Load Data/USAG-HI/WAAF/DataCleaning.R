#Starting with a clean slate- delete everything
rm(list = ls())
cat('\014')

#Setting Directory 
setwd("/Users/fote910/OneDrive - PNNL/Forecasting/Load Data/USAG-HI/WAAF")


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


WAAF<-read.csv("WAAF.csv", header=T)

WAAF$DateTime <- as.POSIXct(as.character(paste(WAAF$X.Date, WAAF$Time)), format="%m/%d/%y %H:%M")

WAAF<- WAAF[, c(3,4)]                           

colnames(WAAF)<- c("Load", "DateTime")  

write_feather(WAAF, "WAAF.feather")
