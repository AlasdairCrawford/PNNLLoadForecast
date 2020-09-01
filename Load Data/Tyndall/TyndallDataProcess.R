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

Tyndall<- read.csv("Load Data\\Tyndall\\TyndallLoad.csv", header = T,stringsAsFactors = FALSE)%>%
  transmute(DateTime=paste(ï..Date,Hour)%>%as.POSIXct(format="%m/%d/%y %H",tz="America/New_York"),
            Load=kW.Sum%>%as.numeric)
               

write_feather(Tyndall, "Load Data\\ProcessedData\\Tyndall.feather")
