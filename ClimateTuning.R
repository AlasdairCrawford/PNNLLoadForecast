library(reshape2)
library(glmnetUtils)
library(glmnet)

ClimateTuningParameters<-NULL
for(i in dataSummary$Dataset%>%unique%>%.[-1]){
  if(TRUE){
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
  }
  
  climateformula<-paste0("Load~DateTime+(",
                         paste0(HourTerms,
                                collapse="+"),
                         ")*(",
                         paste0(DayTerms,collapse="+"),
                         ")+Day5sin+Day5cos")%>%as.formula
  
  
  
  
  model<-glmnet(formula=climateformula,
                data=dataFourier2,
                alpha=0.5)

  stepsize<<-5
  ElasticClimateMSE<-function(x){
    Start<-Sys.time()
    TempMSE<<-NULL
    ExpWeighting<-exp(x[1])
    #TempSetPointWeighting<-exp(x[2])
    glmalpha<-(tanh(x[2])+1)/2
    glmlambda<-exp(x[3])
    print(paste0(c(ExpWeighting,glmalpha,glmlambda),collapse=","))
    if(min(ExpWeighting)<=7/52 | max(ExpWeighting)>=50){return(NA)}
    ElasticClimatePredictions<<-NULL
    
    for(j in seq(1,length(WindowStart),by=stepsize)){
      
      
      #print(WindowStart[j])
      train<-dataFourier%>%
        na.omit%>%
        mutate(ElapsedDays=ElapsedDays-max(ElapsedDays),
               Wt=exp(ElapsedDays/((ExpWeighting)*365.25)),
               Date=DateTime%>%date)%>%
        filter(Date<=WindowStart[j]-1)
      
      
      climateformula<-paste0("Load~DateTime+(",
                             paste0(HourTerms,
                                    collapse="+"),
                             ")*(",
                             paste0(DayTerms,collapse="+"),
                             ")+Day5sin+Day5cos")%>%as.formula
      
      
      
      
      model<-glmnet(formula=climateformula,
                    data=train,
                    weights = Wt,
                    alpha=glmalpha,
                    lambda=glmlambda)
      
      
      test<-dataFourier%>%
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
        bind_rows(ElasticClimatePredictions)->>ElasticClimatePredictions
      
    }
    
    Duration<<-Sys.time()-Start
    print(Duration)
    ElasticClimatePredictions%>%
      filter(PredictionDate>=(LoadData$DateTime%>%date%>%min+365))%>%.$Error%>%.^2%>%mean%>%sqrt%T>%print%>%return
  }
  
  Optimised<-optim(c(log(0.5),0,log(model$lambda%>%median)),
                   ElasticClimateMSE,
                   control=list(maxit=100))
  

  stepsize<<-1
  ElasticClimateMSE(Optimised$par)
  ElasticClimatePredictions%>%write_feather(paste0("Predictions\\ClimateModelElastic\\",i,".feather"))
  
  MLRClimateMSE<-function(x){
    Start<-Sys.time()
    ExpWeighting<-exp(x[1])
    #TempSetPointWeighting<-exp(x[2])
    #glmalpha<-(tanh(x[2])+1)/2
    #glmlambda<-exp(x[3])
    print(paste0(c(ExpWeighting),collapse=","))
    if(min(ExpWeighting)<=7/52 | max(ExpWeighting)>=50){return(NA)}
    MLRClimatePredictions<<-NULL
    
    for(j in seq(1,length(WindowStart),by=stepsize)){
      
      
      #print(WindowStart[j])
      train<-dataFourier%>%
        na.omit%>%
        mutate(ElapsedDays=ElapsedDays-max(ElapsedDays),
               Wt=exp(ElapsedDays/((ExpWeighting)*365.25)),
               Date=DateTime%>%date)%>%
        filter(Date<=WindowStart[j]-1)
      
      
      climateformula<-paste0("Load~DateTime+(",
                             paste0(HourTerms,
                                    collapse="+"),
                             ")*(",
                             paste0(DayTerms,collapse="+"),
                             ")+Day5sin+Day5cos")%>%as.formula
      
      
      
      
      model<-lm(formula=climateformula,
                    data=train,
                    weights = Wt)
      
      
      test<-dataFourier%>%
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
        bind_rows(MLRClimatePredictions)->>MLRClimatePredictions
      
    }
    
    Duration<<-Sys.time()-Start
    print(Duration)
    MLRClimatePredictions%>%
      filter(PredictionDate>=(LoadData$DateTime%>%date%>%min+365))%>%.$Error%>%.^2%>%mean%>%sqrt%T>%print%>%return
  }
  
  MLRClimateMSE(Optimised$par[1])
  MLRClimatePredictions%>%write_feather(paste0("Predictions\\ClimateModelMLR\\",i,".feather"))
  
  data.frame(Dataset=i,
             Weighting=exp(Optimised$par[1]),
             alpha=(tanh(Optimised$par[2])+1)/2,
             lambda=exp(Optimised$par[3]))%>%
    bind_rows(ClimateTuningParameters)->ClimateTuningParameters
  
}

ClimateTuningParameters%>%write_feather("ClimateTuningParameters.feather")