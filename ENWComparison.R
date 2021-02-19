
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


DurationClimateOld<<-Sys.time()-Start

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

DurationTempOld<<-Sys.time()-Start

ElasticNetTempMSE2<-function(x){
  Start<-Sys.time()
  TempMSE<<-NULL
  ExpWeighting<-exp(x[1])
  TempSetPointWeighting<-exp(x[2])
  ElasticTempPredictions2<<-NULL
  TempSmoothing<-(tanh(x[3])+1)/2
  glmalpha<-(tanh(x[4])+1)/2
  glmlambda<-exp(x[5])
  print(paste0(c(ExpWeighting,TempSetPointWeighting,glmalpha,glmlambda,TempSmoothing),collapse=","))
  if(min(ExpWeighting,TempSetPointWeighting)<=7/52 | max(ExpWeighting,TempSetPointWeighting)>=50){return(NA)}
  
  dataFourier2<-dataFourier%>%
    mutate(LongTemp=rollapply(Temp,24*8,function(x){
      
      TempSmoothing*sum((1-TempSmoothing)^((length(x)-1):0)*x)
    },align="right",na.pad=TRUE))
  
  for(j in seq(1,length(WindowStart),by=1)){
    
    
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
      bind_rows(ElasticTempPredictions2)->>ElasticTempPredictions2
    
  }
  
  Duration<<-Sys.time()-Start
  print(Duration)
  ElasticTempPredictions2%>%
    filter(PredictionDate>=(LoadData$DateTime%>%date%>%min+365),
           DaysAhead<=10)%>%mutate(Weight=10-DaysAhead)%>%summarise(RMSE=weighted.mean(Error^2,Weight))%>%.$RMSE%>%sqrt%T>%print%>%return
}

ElasticNetTempMSE2(c(log(0.671035390933689),log(3.09711965038416),-1.016639,0.08850173,log(6.93429465979809)))
ElasticNetDuration<-Duration

MLRTempMSE2<-function(x){
  Start<-Sys.time()
  TempMSE<<-NULL
  ExpWeighting<-exp(x[1])
  TempSetPointWeighting<-exp(x[2])
  MLRTempPredictions2<<-NULL
  TempSmoothing<-(tanh(x[3])+1)/2
  #glmalpha<-(tanh(x[4])+1)/2
  #glmlambda<-exp(x[5])
  print(paste0(c(ExpWeighting,TempSetPointWeighting,TempSmoothing),collapse=","))
  if(min(ExpWeighting,TempSetPointWeighting)<=7/52 | max(ExpWeighting,TempSetPointWeighting)>=50){return(NA)}
  
  dataFourier2<-dataFourier%>%
    mutate(LongTemp=rollapply(Temp,24*8,function(x){
      
      TempSmoothing*sum((1-TempSmoothing)^((length(x)-1):0)*x)
    },align="right",na.pad=TRUE))
  
  for(j in seq(1,length(WindowStart),by=1)){
    
    
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
      bind_rows(MLRTempPredictions2)->>MLRTempPredictions2
    
  }
  
  Duration<<-Sys.time()-Start
  print(Duration)
  MLRTempPredictions2%>%
    filter(PredictionDate>=(LoadData$DateTime%>%date%>%min+365),
           DaysAhead<=10)%>%mutate(Weight=10-DaysAhead)%>%summarise(RMSE=weighted.mean(Error^2,Weight))%>%.$RMSE%>%sqrt%T>%print%>%return
}

MLRTempMSE2(c(log(0.671035390933689),log(3.09711965038416),-1.016639))

MLRDuration<-Duration

bind_rows(ElasticClimatePredictions%>%mutate(Method="Elastic Net",Type="Climate"),
          MLRClimatePredictions%>%mutate(Method="MLR",Type="Climate"),
          OldClimatePredictions%>%mutate(Method="Old",Type="Climate"),
          ElasticTempPredictions2%>%mutate(Method="Elastic Net",Type="Temperature"),
          MLRTempPredictions2%>%mutate(Method="MLR",Type="Temperature"),
          OldTempPredictions2%>%mutate(Method="Old",Type="Temperature")
)%>%
  group_by(DaysAhead,Method,Type)%>%
  filter(DaysAhead>0,!(Type=="Temperature" & DaysAhead>10))%>%
  summarise(RMSE=Error%>%.^2%>%mean%>%sqrt,
            Median=Error%>%abs%>%median)%>%
  ggplot(aes(x=DaysAhead,y=RMSE,colour=Method))+geom_line(size=1.2)+facet_wrap(.~Type,scales="free")+theme_bw()+
  scale_color_brewer(palette = "Set1")#+scale_y_continuous(limits = c(0,NA))
