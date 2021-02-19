library(reshape2)
library(glmnetUtils)
library(glmnet)
library(segmented)
library(tidyverse)
library(lubridate)

TempTuningParameters<-NULL
for(i in dataSummary$Dataset%>%unique%>%.[-(c(1,2))]){
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
  
  tempformula<-paste0("Load~DateTime+(",
                      paste0(HourTerms,
                             collapse="+"),
                      ")*(",
                      paste0(DayTerms,collapse="+"),
                      "+Temp+I(Temp^2)+I(Temp^3))+Day5sin+Day5cos")%>%as.formula
  
  
  
  
  model<-glmnet(formula=tempformula,
                data=dataFourier2,
                alpha=0.5)
  
  stepsize<<-5
  ElasticNetTempMSE2<-function(x){
    Start<-Sys.time()
    TempMSE<<-NULL
    ExpWeighting<-exp(x[1])
    TempSetPointWeighting<-exp(x[2])
    ElasticTempPredictions<<-NULL
    TempSmoothing<-(tanh(x[3])+1)/2
    glmalpha<-(tanh(x[4])+1)/2
    glmlambda<-exp(x[5])
    print(paste0(c(ExpWeighting,TempSetPointWeighting,glmalpha,glmlambda,TempSmoothing),collapse=","))
    if(min(ExpWeighting,TempSetPointWeighting)<=7/52 | max(ExpWeighting,TempSetPointWeighting)>=50){return(NA)}
    
    dataFourier2<-dataFourier%>%
      mutate(LongTemp=rollapply(Temp,24*8,function(x){
        
        TempSmoothing*sum((1-TempSmoothing)^((length(x)-1):0)*x)
      },align="right",na.pad=TRUE))
    
    for(j in seq(1,length(WindowStart),by=stepsize)){
      
      
      #print(WindowStart[j])
      train<-dataFourier2%>%
        na.omit%>%
        mutate(ElapsedDays=ElapsedDays-max(ElapsedDays),
               Wt=exp(ElapsedDays/((TempSetPointWeighting)*365.25)),
               Date=DateTime%>%date)%>%
        filter(Date<=WindowStart[j]-1)
      
      range<-max(train$HighTemp)>65 & min(train$LowTemp)<65
      if(range){
        
        piecewisemodel<-lm(data=train,
                           formula=Load~Temp+LongTemp,
                           weights=Wt)
        
        SegmentedModel<-tryCatch({segmented(piecewisemodel,
                                            seg.Z =~Temp+LongTemp,
                                            psi=list(Temp=mean(train$Temp),
                                                     LongTemp=mean(train$LongTemp)),
                                            control = seg.control(n.boot = 0))},
                                 error = function(err) {return(NA)})
        
        if(!is.na(SegmentedModel)){
          SegmentedModel$psi["psi1.Temp","Est."]->setPoint
          SegmentedModel$psi["psi1.LongTemp","Est."]->setPointLong
          
          
          tempformula<-paste0("Load~DateTime+(",
                              paste0(HourTerms,
                                     collapse="+"),
                              ")*(",
                              paste0(DayTerms,collapse="+"),
                              "+Temp:(Temp>0)+Temp+I(Temp^2)+I(Temp^3)+LongTemp:(LongTemp>0)+LongTemp+I(LongTemp^2)+I(LongTemp^3))+Day5sin+Day5cos")%>%as.formula
        }else{
          setPoint<-mean(train$Temp)
          train%>%
            mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)))->train
          
          
          tempformula<-paste0("Load~DateTime+(",
                              paste0(HourTerms,
                                     collapse="+"),
                              ")*(",
                              paste0(DayTerms,collapse="+"),
                              "+Temp+I(Temp^2)+I(Temp^3))+Day5sin+Day5cos")%>%as.formula
        }
        
        if(!is.numeric(setPoint)){setPoint<-mean(train$Temp)}
        
        train%>%
          mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)),
                 Temp=Temp-setPoint,
                 LongTemp=LongTemp-setPointLong)->train
        
      }else{
        
        train%>%
          mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)))->train
        setPoint<-65
        setPointLong<-65
        
        tempformula<-paste0("Load~DateTime+(",
                            paste0(HourTerms,
                                   collapse="+"),
                            ")*(",
                            paste0(DayTerms,collapse="+"),
                            "+Temp+I(Temp^2)+I(Temp^3))+Day5sin+Day5cos")%>%as.formula
      }
      
      
      
      
      
      model<-glmnet(formula=tempformula,
                    data=train,
                    weights = Wt,
                    alpha=glmalpha,
                    lambda=glmlambda)
      
      
      test<-dataFourier2%>%
        na.omit%>%
        mutate(Date=DateTime%>%date,
               Temp=Temp-setPoint,
               LongTemp=LongTemp-setPointLong)%>%
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
        bind_rows(ElasticTempPredictions)->>ElasticTempPredictions
      
    }
    
    Duration<<-Sys.time()-Start
    print(Duration)
    ElasticTempPredictions%>%
      filter(PredictionDate>=(LoadData$DateTime%>%date%>%min+365),
             DaysAhead<=10)%>%mutate(Weight=10-DaysAhead)%>%summarise(RMSE=weighted.mean(Error^2,Weight))%>%.$RMSE%>%sqrt%T>%print%>%return
  }
  
  Optimised<-optim(c(log(0.5),log(0.5),0.1,0,log(model$lambda%>%median)),
                   ElasticNetTempMSE2,
                   control=list(maxit=100))
  
  
  stepsize<<-1
  ElasticNetTempMSE2(Optimised$par)
  ElasticTempPredictions%>%write_feather(paste0("Predictions\\TempModelElastic\\",i,".feather"))
  
  MLRTempMSE2<-function(x){
    Start<-Sys.time()
    TempMSE<<-NULL
    ExpWeighting<-exp(x[1])
    TempSetPointWeighting<-exp(x[2])
    MLRTempPredictions<<-NULL
    TempSmoothing<-(tanh(x[3])+1)/2
    if(min(ExpWeighting,TempSetPointWeighting)<=7/52 | max(ExpWeighting,TempSetPointWeighting)>=50){return(NA)}
    
    dataFourier2<-dataFourier%>%
      mutate(LongTemp=rollapply(Temp,24*8,function(x){
        
        TempSmoothing*sum((1-TempSmoothing)^((length(x)-1):0)*x)
      },align="right",na.pad=TRUE))
    
    for(j in seq(1,length(WindowStart),by=stepsize)){
      
      
      #print(WindowStart[j])
      train<-dataFourier2%>%
        na.omit%>%
        mutate(ElapsedDays=ElapsedDays-max(ElapsedDays),
               Wt=exp(ElapsedDays/((TempSetPointWeighting)*365.25)),
               Date=DateTime%>%date)%>%
        filter(Date<=WindowStart[j]-1)
      
      range<-max(train$HighTemp)>65 & min(train$LowTemp)<65
      if(range){
        
        piecewisemodel<-lm(data=train,
                           formula=Load~Temp+LongTemp,
                           weights=Wt)
        
        SegmentedModel<-tryCatch({segmented(piecewisemodel,
                                            seg.Z =~Temp+LongTemp,
                                            psi=list(Temp=mean(train$Temp),
                                                     LongTemp=mean(train$LongTemp)),
                                            control = seg.control(n.boot = 0))},
                                 error = function(err) {return(NA)})
        
        if(!is.na(SegmentedModel)){
          SegmentedModel$psi["psi1.Temp","Est."]->setPoint
          SegmentedModel$psi["psi1.LongTemp","Est."]->setPointLong
          
          
          tempformula<-paste0("Load~DateTime+(",
                              paste0(HourTerms,
                                     collapse="+"),
                              ")*(",
                              paste0(DayTerms,collapse="+"),
                              "+Temp:(Temp>0)+Temp+I(Temp^2)+I(Temp^3)+LongTemp:(LongTemp>0)+LongTemp+I(LongTemp^2)+I(LongTemp^3))+Day5sin+Day5cos")%>%as.formula
        }else{
          setPoint<-mean(train$Temp)
          train%>%
            mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)))->train
          
          
          tempformula<-paste0("Load~DateTime+(",
                              paste0(HourTerms,
                                     collapse="+"),
                              ")*(",
                              paste0(DayTerms,collapse="+"),
                              "+Temp+I(Temp^2)+I(Temp^3))+Day5sin+Day5cos")%>%as.formula
        }
        
        if(!is.numeric(setPoint)){setPoint<-mean(train$Temp)}
        
        train%>%
          mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)),
                 Temp=Temp-setPoint,
                 LongTemp=LongTemp-setPointLong)->train
        
      }else{
        
        train%>%
          mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)))->train
        setPoint<-65
        setPointLong<-65
        
        tempformula<-paste0("Load~DateTime+(",
                            paste0(HourTerms,
                                   collapse="+"),
                            ")*(",
                            paste0(DayTerms,collapse="+"),
                            "+Temp+I(Temp^2)+I(Temp^3))+Day5sin+Day5cos")%>%as.formula
      }
      
      
      
      
      
      model<-lm(formula=tempformula,
                    data=train,
                    weights = Wt)
      
      
      test<-dataFourier2%>%
        na.omit%>%
        mutate(Date=DateTime%>%date,
               Temp=Temp-setPoint,
               LongTemp=LongTemp-setPointLong)%>%
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
        bind_rows(MLRTempPredictions)->>MLRTempPredictions
      
    }
    
    Duration<<-Sys.time()-Start
    print(Duration)
    MLRTempPredictions%>%
      filter(PredictionDate>=(LoadData$DateTime%>%date%>%min+365),
             DaysAhead<=10)%>%mutate(Weight=10-DaysAhead)%>%summarise(RMSE=weighted.mean(Error^2,Weight))%>%.$RMSE%>%sqrt%T>%print%>%return
  }
  
  
  MLRTempMSE2(Optimised$par[1:3])
  MLRTempPredictions%>%write_feather(paste0("Predictions\\TempModelMLR\\",i,".feather"))
  
  data.frame(Dataset=i,
             Weighting=exp(Optimised$par[1]),
             TempSetPt=exp(Optimised$par[2]),
             TempSmooth=exp(Optimised$par[3]),
             alpha=(tanh(Optimised$par[4])+1)/2,
             lambda=exp(Optimised$par[5]))%>%
    bind_rows(TempTuningParameters)->TempTuningParameters
  
}

TempTuningParameters%>%write_feather("TempTuningParameters.feather")