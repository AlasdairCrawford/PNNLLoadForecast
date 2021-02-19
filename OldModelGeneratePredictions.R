library(reshape2)

OldModelDurations<-NULL
for(i in dataSummary$Dataset%>%unique%>%last){

  print(i)
  dataSummary2<-dataSummary%>%
    filter(Dataset==i)
  
  
  LoadData<-NULL
  for(j in 1:nrow(dataSummary2)){
    
    LoadData<-paste0("Load Data\\ProcessedData\\",dataSummary2$Datafile[j])%>%
      read_feather%>%
      bind_rows(LoadData)
  }
  LoadData%>%
    group_by(DateTime)%>%
    filter(n()==nrow(dataSummary2))%>%
    summarise(Load=sum(Load),
              Temp=mean(Temp))%>%
    na.omit%>%
    mutate(DateTime=round_date(DateTime,"hour"))%>%
    group_by(DateTime)%>%
    summarise(Load=mean(Load),
              Temp=mean(Temp))%>%
    ungroup->LoadData
  
  
  FourierCoefs<-1:12
  
  dataFourier<-LoadData%>%
    mutate(Day=yday(DateTime),
           Year=year(DateTime),
           Hour=hour(DateTime),
           Weekday=wday(DateTime),
           Weekday=(Weekday-2)%%7+1, ##i want sunday at the end of the week
           Weekend=Weekday>=6,
           Holiday=DateTime%>%as.Date%>%isHoliday(board=TRUE,inaug=FALSE),
           Date=DateTime%>%as.Date)%>%
    group_by(Date)%>%
    mutate(HighTemp=max(Temp),
           LowTemp=min(Temp))%>%
    ungroup%>%
    transmute(DateTime,
              Load,
              Hour,
              Day,
              IsMon=(Weekday==1),
              IsFri=Weekday==5,
              IsSat=(Weekday==6)|Holiday,
              IsSun=(Weekday==7),
              ElapsedDays=date(DateTime)%>%as.numeric-first(date(DateTime))%>%as.numeric,
              Temp,
              HighTemp,
              LowTemp)
  
  for(j in FourierCoefs){
    dataFourier<-dataFourier%>%
      mutate(!!paste0("Day",j,"sin"):=sin((Day+Hour/24)*2*j*pi/365),
             !!paste0("Day",j,"cos"):=cos((Day+Hour/24)*2*j*pi/365),
             !!paste0("Hour",j,"sin"):=sin(Hour*2*j*pi/24),
             !!paste0("Hour",j,"cos"):=cos(Hour*2*j*pi/24))
  }
  
  WindowStart<-seq(LoadData$DateTime%>%min%>%as.Date+365,LoadData$DateTime%>%max%>%as.Date-n,by=1)
  
dataFourier2<-dataFourier%>%
  mutate(Temp=Temp-mean(Temp),
         Temp2=Temp^2,
         Temp3=Temp^3)
lags<-1:3

for(j in lags){
  
  dataFourier2<-dataFourier2%>%
    mutate(!!paste0("Temp.",j) := lag(Temp,j),
           !!paste0("Temp2.",j) := lag(Temp2,j),
           !!paste0("Temp3.",j) := lag(Temp3,j))
}


dataFourier2%>%
  select(DateTime,
         contains("Temp"))%>%
  melt(id.var="DateTime")->datamelted

smoothing<-c(0.15,0.2)

datamelted2<-NULL
for(j in smoothing){
  
  datamelted%>%
    group_by(variable)%>%
    mutate(Smoothed=rollapply(value,24,function(x){
      
      j*sum((1-j)^((length(x)-1):0)*x)
    },align="right",na.pad=TRUE))%>%
    ungroup%>%
    mutate(variable=paste0(variable,".",j))%>%
    bind_rows(datamelted2)->datamelted2
}

SmoothedTemps<-datamelted2%>%
  transmute(DateTime,
            variable,
            value=Smoothed)%>%
  dcast(DateTime~variable,fun.aggregate = mean)

dataFourier3<-dataFourier2%>%
  merge(SmoothedTemps)%>%
  mutate(Weekday=wday(DateTime),
         Month=month(DateTime),
         Weekday=(Weekday-2)%%7+1, ##i want sunday at the end of the week
         Holiday=DateTime%>%as.Date%>%isHoliday(board=TRUE,inaug=FALSE))
na.omit

Start<-Sys.time()

OldTempPredictions<<-NULL
for(j in seq(1,length(WindowStart),by=1)){
  
  
  print(paste(i,"Temp",WindowStart[j]))
  train<-dataFourier3%>%
    na.omit%>%
    mutate(ElapsedDays=ElapsedDays-max(ElapsedDays),
           #Wt=exp(ElapsedDays/((ExpWeighting)*365.25)),
           Date=DateTime%>%date)%>%
    filter(Date<=WindowStart[j]-1)
  
  
  oldformula<-"Load~DateTime+
       factor(Weekday)*factor(Hour)+
       Holiday+
       (factor(Hour)+
       factor(Month))*(Temp+
     Temp2+
     Temp3+
     Temp.0.15+
     Temp2.0.15+
     Temp3.0.15)"%>%as.formula
  
  
  
  
  model<-lm(formula=oldformula,
            data=train)
  
  
  test<-dataFourier3%>%
    na.omit%>%
    mutate(Date=DateTime%>%date)%>%
    filter(Date>WindowStart[j]-1,
           Date<=WindowStart[j]-1+n)
  
  trainmonths<-unique(train$Month)
  testmonths<-unique(test$Month)
  
  newmonths<-testmonths[!(testmonths %in% trainmonths)]
  
  x<-sin(trainmonths/12*2*pi)
  y<-cos(trainmonths/12*2*pi)
  
  for(k in newmonths)
  {
    
    testx<-sin(k/12*2*pi)
    testy<-cos(k/12*2*pi)
    closestmonth<-trainmonths[which.min((x-testx)^2+(y-testy)^2)]
    test<-test%>%
      mutate(Month=ifelse(Month==k,closestmonth,Month))->test
  }
  
  test%>%
    transmute(DateTime,
              Load,
              Hour,
              Predicted=predict(model,test),
              Error=Predicted-Load,
              PredictionDate=WindowStart[j],
              DaysAhead=date(DateTime)%>%as.numeric-PredictionDate%>%as.numeric)%>%
    bind_rows(OldTempPredictions)->>OldTempPredictions
  
}

DurationTemp<<-Sys.time()-Start
OldTempPredictions%>%write_feather(paste0("Predictions\\TempModelFactor\\",i,".feather"))


Start<-Sys.time()
OldClimatePredictions<-NULL
for(j in seq(1,length(WindowStart),by=1)){
  
  
  print(paste(i,"Climate",WindowStart[j]))
  train<-dataFourier3%>%
    na.omit%>%
    mutate(ElapsedDays=ElapsedDays-max(ElapsedDays),
           #Wt=exp(ElapsedDays/((ExpWeighting)*365.25)),
           Date=DateTime%>%date)%>%
    filter(Date<=WindowStart[j]-1)
  
  
  oldclimateformula<-"Load~DateTime+
       factor(Weekday)*factor(Hour)+
       Holiday+
       (factor(Hour)+
       factor(Month))"%>%as.formula
  
  
  
  
  model<-lm(formula=oldclimateformula,
            data=train)
  
  
  test<-dataFourier3%>%
    na.omit%>%
    mutate(Date=DateTime%>%date)%>%
    filter(Date>WindowStart[j]-1,
           Date<=WindowStart[j]-1+n)
  
  
  trainmonths<-unique(train$Month)
  testmonths<-unique(test$Month)
  
  newmonths<-testmonths[!(testmonths %in% trainmonths)]
  
  x<-sin(trainmonths/12*2*pi)
  y<-cos(trainmonths/12*2*pi)
  
  for(k in newmonths)
  {
    
    testx<-sin(k/12*2*pi)
    testy<-cos(k/12*2*pi)
    closestmonth<-trainmonths[which.min((x-testx)^2+(y-testy)^2)]
    test<-test%>%
      mutate(Month=ifelse(Month==k,closestmonth,Month))->test
  }
  
  test%>%
    transmute(DateTime,
              Load,
              Hour,
              Predicted=predict(model,test),
              Error=Predicted-Load,
              PredictionDate=WindowStart[j],
              DaysAhead=date(DateTime)%>%as.numeric-PredictionDate%>%as.numeric)%>%
    bind_rows(OldClimatePredictions)->>OldClimatePredictions
  
}

DurationClimate<<-Sys.time()-Start
OldClimatePredictions%>%write_feather(paste0("Predictions\\ClimateModelFactor\\",i,".feather"))

data.frame(Dataset=i,
           DurationTemp=DurationTemp,
           DurationClimate=DurationClimate)%>%
  bind_rows(OldModelDurations)->OldModelDurations

}