Start<-Sys.time()
TempMSE<<-NULL
ExpWeighting<-exp(x[1])
#TempSetPointWeighting<-exp(x[2])
glmalpha<-(tanh(x[2])+1)/2
glmlambda<-exp(x[3])
print(paste0(c(ExpWeighting,glmalpha,glmlambda),collapse=","))
if(min(ExpWeighting)<=7/52 | max(ExpWeighting)>=50){return(NA)}
OldTempPredictions<<-NULL

dataFourier2<-dataFourier%>%
  mutate(Temp=Temp-mean(Temp),
         Temp2=Temp^2,
         Temp3=Temp^3)
lags<-1:3

for(i in lags){
  
  dataFourier2<-dataFourier2%>%
    mutate(!!paste0("Temp.",i) := lag(Temp,i),
           !!paste0("Temp2.",i) := lag(Temp2,i),
           !!paste0("Temp3.",i) := lag(Temp3,i))
}


dataFourier2%>%
  select(DateTime,
         contains("Temp"))%>%
  melt(id.var="DateTime")->datamelted

smoothing<-c(0.15,0.2)

datamelted2<-NULL
for(i in smoothing){
  
  datamelted%>%
    group_by(variable)%>%
    mutate(Smoothed=rollapply(value,24,function(x){
      
      i*sum((1-i)^((length(x)-1):0)*x)
    },align="right",na.pad=TRUE))%>%
    ungroup%>%
    mutate(variable=paste0(variable,".",i))%>%
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



  OldTempPredictions<<-NULL
for(j in seq(1,length(WindowStart),by=1)){
  
  
  print(WindowStart[j])
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
     Temp.1+
     Temp2.1+
     Temp3.1+
     Temp.2+
     Temp2.2+
     Temp3.2+
     Temp.3+
     Temp2.3+
     Temp3.3+
     Temp.0.15+
     Temp.0.2+
     Temp.1.0.15+
     Temp.1.0.2+
     Temp.2.0.15+
     Temp.2.0.2+
     Temp.3.0.15+
     Temp.3.0.2+
     Temp2.0.15+
     Temp2.0.2+
     Temp2.1.0.15+
     Temp2.1.0.2+
     Temp2.2.0.15+
     Temp2.2.0.2+
     Temp2.3.0.15+
     Temp2.3.0.2+
     Temp3.0.15+
     Temp3.0.2+
     Temp3.1.0.15+
     Temp3.1.0.2+
     Temp3.2.0.15+
     Temp3.2.0.2+
     Temp3.3.0.15+
     Temp3.3.0.2)"%>%as.formula
  
  
  
  
  model<-lm(formula=oldformula,
                data=train)
  
  
  test<-dataFourier3%>%
    na.omit%>%
    mutate(Date=DateTime%>%date)%>%
    filter(Date>WindowStart[j]-1,
           Date<=WindowStart[j]-1+n)
  
  
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
print(DurationTemp)

Start<-Sys.time()
OldClimatePredictions<-NULL
for(j in seq(1,length(WindowStart),by=1)){
  
  
  print(WindowStart[j])
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
print(DurationClimate)

OldTempPredictions%>%
  mutate(Hour=hour(DateTime))%>%
  merge(LoadData)%>%
  ggplot(aes(x=Temp,y=Error))+geom_bar(stat='identity')+facet_wrap(.~Hour)

Start<-Sys.time()

OldTempPredictions2<<-NULL
for(j in seq(1,length(WindowStart),by=1)){
  
  
  print(WindowStart[j])
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
  
  
  test%>%
    transmute(DateTime,
              Load,
              Hour,
              Predicted=predict(model,test),
              Error=Predicted-Load,
              PredictionDate=WindowStart[j],
              DaysAhead=date(DateTime)%>%as.numeric-PredictionDate%>%as.numeric)%>%
    bind_rows(OldTempPredictions2)->>OldTempPredictions2
  
}

DurationTemp<<-Sys.time()-Start
print(DurationTemp)


Start<-Sys.time()

OldTempPredictions3<<-NULL
for(j in seq(1,length(WindowStart),by=1)){
  
  
  print(WindowStart[j])
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
     Temp3)"%>%as.formula
  
  
  
  
  model<-lm(formula=oldformula,
            data=train)
  
  
  test<-dataFourier3%>%
    na.omit%>%
    mutate(Date=DateTime%>%date)%>%
    filter(Date>WindowStart[j]-1,
           Date<=WindowStart[j]-1+n)
  
  
  test%>%
    transmute(DateTime,
              Load,
              Hour,
              Predicted=predict(model,test),
              Error=Predicted-Load,
              PredictionDate=WindowStart[j],
              DaysAhead=date(DateTime)%>%as.numeric-PredictionDate%>%as.numeric)%>%
    bind_rows(OldTempPredictions3)->>OldTempPredictions3
  
}

DurationTemp<<-Sys.time()-Start
print(DurationTemp)


Start<-Sys.time()

OldTempPredictions4<<-NULL
for(j in seq(1,length(WindowStart),by=1)){
  
  
  print(WindowStart[j])
  train<-dataFourier3%>%
    na.omit%>%
    mutate(ElapsedDays=ElapsedDays-max(ElapsedDays),
           #Wt=exp(ElapsedDays/((ExpWeighting)*365.25)),
           Date=DateTime%>%date)%>%
    filter(Date<=WindowStart[j]-1)
  
  
  oldformula<-"Load~DateTime+
       factor(Hour)*(Temp+
     Temp2+
     Temp3+
       factor(Month)+
  factor(Weekday)+
  Holiday)"%>%as.formula
  
  
  
  
  model<-lm(formula=oldformula,
            data=train)
  
  
  test<-dataFourier3%>%
    na.omit%>%
    mutate(Date=DateTime%>%date)%>%
    filter(Date>WindowStart[j]-1,
           Date<=WindowStart[j]-1+n)
  
  
  test%>%
    transmute(DateTime,
              Load,
              Hour,
              Predicted=predict(model,test),
              Error=Predicted-Load,
              PredictionDate=WindowStart[j],
              DaysAhead=date(DateTime)%>%as.numeric-PredictionDate%>%as.numeric)%>%
    bind_rows(OldTempPredictions4)->>OldTempPredictions4
  
}

DurationTemp<<-Sys.time()-Start
print(DurationTemp)



Start<-Sys.time()

OldTempPredictions5<<-NULL
for(j in seq(1,length(WindowStart),by=1)){
  
  
  print(WindowStart[j])
  train<-dataFourier3%>%
    na.omit%>%
    mutate(ElapsedDays=ElapsedDays-max(ElapsedDays),
           #Wt=exp(ElapsedDays/((ExpWeighting)*365.25)),
           Date=DateTime%>%date)%>%
    filter(Date<=WindowStart[j]-1)
  
  
  oldformula<-"Load~DateTime+
       factor(Hour)*(Temp+
     Temp2+
       factor(Month)+
  factor(Weekday)+
  Holiday)"%>%as.formula
  
  
  
  
  model<-lm(formula=oldformula,
            data=train)
  
  
  test<-dataFourier3%>%
    na.omit%>%
    mutate(Date=DateTime%>%date)%>%
    filter(Date>WindowStart[j]-1,
           Date<=WindowStart[j]-1+n)
  
  
  test%>%
    transmute(DateTime,
              Load,
              Hour,
              Predicted=predict(model,test),
              Error=Predicted-Load,
              PredictionDate=WindowStart[j],
              DaysAhead=date(DateTime)%>%as.numeric-PredictionDate%>%as.numeric)%>%
    bind_rows(OldTempPredictions5)->>OldTempPredictions5
  
}

DurationTemp<<-Sys.time()-Start
print(DurationTemp)