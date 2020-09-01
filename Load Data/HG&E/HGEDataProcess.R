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

HGELoad<-read_excel("Load Data\\HG&E\\HG&ELoad.xlsx", sheet="Sheet1")%>%
  transmute(DateTime=as.POSIXct(Time, format='%Y/%m/%d %H:%M', tz="America/New_York"),
            Load)


write_feather(HGELoad, "Load Data\\ProcessedData\\HGELoad.feather")

