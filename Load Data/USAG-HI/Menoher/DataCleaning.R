#Starting with a clean slate- delete everything
rm(list = ls())
cat('\014')

#Setting Directory 
setwd("/Users/fote910/OneDrive - PNNL/Forecasting/Load Data/USAG-HI/Menoher")


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


Menoher<-read.csv("MenoherLoad.csv", header=T)

Menoher$DateTime <- as.POSIXct(as.character(paste(Menoher$Date, Menoher$Time)), format="%m/%d/%y %H:%M")

Menoher<- Menoher[, c(3,4)]                           

colnames(Menoher)<- c("Load", "DateTime")  

write_feather(Menoher, "Menoher.feather")
