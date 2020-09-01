BainbridgeLoad<-read_excel("Load Data\\Bainbridge\\Bainbridge Substation Load (PSE).xlsx", sheet="All Data",skip=2)%>%
  transmute(DateTime=`Date & Time`%>%force_tz,
            Load=Win+Mur)

write_feather(BainbridgeLoad,"Load Data\\ProcessedData\\Bainbridge.feather")
