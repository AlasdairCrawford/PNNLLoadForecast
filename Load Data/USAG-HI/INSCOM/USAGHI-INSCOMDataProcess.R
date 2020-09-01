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


USAGHIINSCOMLoad<-read.csv("Load Data//USAG-HI//INSCOM//INSCOMLoad.csv", header=T,stringsAsFactors = FALSE)%>%
  transmute(DateTime=as.POSIXct(`ï..DateTime`, format='%m/%d/%y %H:%M',tz="Pacific/Honolulu"),
            Load)

write_feather(USAGHIINSCOMLoad, "Load Data\\ProcessedData\\INSCOM.feather")
