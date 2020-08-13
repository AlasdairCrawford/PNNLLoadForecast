#Starting with a clean slate- delete everything
rm(list = ls())
cat('\014')

#Setting Directory 
setwd("/Users/fote910/OneDrive - PNNL/Forecasting/Load Data/HG&E")


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

HGELoad<-read_excel("HG&ELoad.xlsx", sheet="Sheet1")

HGELoad$Time<- as.POSIXct(HGELoad$Time, format='%Y/%m/%d %H:%M', tz="UTC")

write_feather(HGELoad, "HGELoad.feather")

