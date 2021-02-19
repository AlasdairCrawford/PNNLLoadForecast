ElasticTempMSE<-function(x){
  Start<-Sys.time()
  TempMSE<<-NULL
  ExpWeighting<-exp(x[1])
  TempSetPointWeighting<-exp(x[2])
  glmalpha<-(tanh(x[3])+1)/2
  glmlambda<-exp(x[4])
  print(paste0(c(ExpWeighting,TempSetPointWeighting,glmalpha,glmlambda),collapse=","))
  if(min(ExpWeighting,TempSetPointWeighting)<=7/52 | max(ExpWeighting,TempSetPointWeighting)>=50){return(NA)}
  ElasticTempPredictions<<-NULL
  
  for(j in seq(1,length(WindowStart),by=1)){
    
    
    #print(WindowStart[j])
    train<-dataFourier%>%
      na.omit%>%
      mutate(ElapsedDays=ElapsedDays-max(ElapsedDays),
             Wt=exp(ElapsedDays/((TempSetPointWeighting)*365.25)),
             Date=DateTime%>%date)%>%
      filter(Date<=WindowStart[j]-1)
    
    range<-max(train$HighTemp)>65 & min(train$LowTemp)<65
    if(range){
      
      piecewisemodel<-lm(data=train,
                         formula=Load~HighTemp+LowTemp,
                         weights=Wt)
      
      SegmentedModel<-tryCatch({segmented(piecewisemodel,
                                          seg.Z =~LowTemp+HighTemp,
                                          psi=list(LowTemp=mean(train$LowTemp),
                                                   HighTemp=mean(train$HighTemp)),
                                          control = seg.control(n.boot = 0))},
                               error = function(err) {return(NA)})
      
      if(!is.na(SegmentedModel)){
        SegmentedModel$psi["psi1.HighTemp","Est."]->setPointHigh
        SegmentedModel$psi["psi1.LowTemp","Est."]->setPointLow
        
        
        tempformula<-paste0("Load~DateTime+(",
                            paste0(HourTerms,
                                   collapse="+"),
                            ")*(",
                            paste0(DayTerms,collapse="+"),
                            "+HighTemp:(HighTemp>0)+HighTemp+LowTemp:(LowTemp>0)+LowTemp)+Day5sin+Day5cos")%>%as.formula
      }else{
        setPointHigh<-0
        setPointLow<-0
        train%>%
          mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)))->train
        
        
        tempformula<-paste0("Load~DateTime+(",
                            paste0(HourTerms,
                                   collapse="+"),
                            ")*(",
                            paste0(DayTerms,collapse="+"),
                            "+HighTemp+LowTemp)+Day5sin+Day5cos")%>%as.formula
      }
      
      if(!is.numeric(setPointLow)){setPointLow<-0}
      
      if(!is.numeric(setPointHigh)){setPointHigh<-0}
      
      train%>%
        mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)),
               HighTemp=HighTemp-setPointHigh,
               LowTemp=LowTemp-setPointLow)->train
      
    }else{
      
      train%>%
        mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)))->train
      
      
      tempformula<-paste0("Load~DateTime+(",
                          paste0(HourTerms,
                                 collapse="+"),
                          ")*(",
                          paste0(DayTerms,collapse="+"),
                          "+HighTemp+LowTemp)+Day5sin+Day5cos")%>%as.formula
    }
    
    
    
    
    
    model<-glmnet(formula=tempformula,
                  data=train,
                  weights = Wt,
                  alpha=glmalpha,
                  lambda=glmlambda)
    
    
    test<-dataFourier%>%
      na.omit%>%
      mutate(Date=DateTime%>%date,
             HighTemp=HighTemp-setPointHigh,
             LowTemp=LowTemp-setPointLow)%>%
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
           DaysAhead<=10)%>%.$Error%>%.^2%>%mean%>%sqrt%T>%print%>%return
}

ElasticTempMSE(c(log(0.616851054548242),log(0.340970080187513),-0.1244682,log(0.108290466209456)))

MLRTempMSE<-function(x){
  Start<-Sys.time()
  TempMSE<<-NULL
  ExpWeighting<-exp(x[1])
  TempSetPointWeighting<-exp(x[2])
  print(paste0(c(ExpWeighting,TempSetPointWeighting),collapse=","))
  if(min(ExpWeighting,TempSetPointWeighting)<=7/52 | max(ExpWeighting,TempSetPointWeighting)>=50){return(NA)}
  MLRTempPredictions<<-NULL
  
  for(j in seq(1,length(WindowStart),by=1)){
    
    
    #print(WindowStart[j])
    train<-dataFourier%>%
      na.omit%>%
      mutate(ElapsedDays=ElapsedDays-max(ElapsedDays),
             Wt=exp(ElapsedDays/((TempSetPointWeighting)*365.25)),
             Date=DateTime%>%date)%>%
      filter(Date<=WindowStart[j]-1)
    
    range<-max(train$HighTemp)>65 & min(train$LowTemp)<65
    if(range){
      
      piecewisemodel<-lm(data=train,
                         formula=Load~HighTemp+LowTemp,
                         weights=Wt)
      
      SegmentedModel<-tryCatch({segmented(piecewisemodel,
                                          seg.Z =~LowTemp+HighTemp,
                                          psi=list(LowTemp=mean(train$LowTemp),
                                                   HighTemp=mean(train$HighTemp)),
                                          control = seg.control(n.boot = 0))},
                               error = function(err) {return(NA)})
      
      if(!is.na(SegmentedModel)){
        SegmentedModel$psi["psi1.HighTemp","Est."]->setPointHigh
        SegmentedModel$psi["psi1.LowTemp","Est."]->setPointLow
        
        
        tempformula<-paste0("Load~DateTime+(",
                            paste0(HourTerms,
                                   collapse="+"),
                            ")*(",
                            paste0(DayTerms,collapse="+"),
                            "+HighTemp:(HighTemp>0)+HighTemp+LowTemp:(LowTemp>0)+LowTemp)+Day5sin+Day5cos")%>%as.formula
      }else{
        setPointHigh<-0
        setPointLow<-0
        train%>%
          mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)))->train
        
        
        tempformula<-paste0("Load~DateTime+(",
                            paste0(HourTerms,
                                   collapse="+"),
                            ")*(",
                            paste0(DayTerms,collapse="+"),
                            "+HighTemp+LowTemp)+Day5sin+Day5cos")%>%as.formula
      }
      
      if(!is.numeric(setPointLow)){setPointLow<-0}
      
      if(!is.numeric(setPointHigh)){setPointHigh<-0}
      
      train%>%
        mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)),
               HighTemp=HighTemp-setPointHigh,
               LowTemp=LowTemp-setPointLow)->train
      
    }else{
      
      train%>%
        mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)))->train
      
      
      tempformula<-paste0("Load~DateTime+(",
                          paste0(HourTerms,
                                 collapse="+"),
                          ")*(",
                          paste0(DayTerms,collapse="+"),
                          "+HighTemp+LowTemp)+Day5sin+Day5cos")%>%as.formula
    }
    
    
    
    
    
    model<-lm(formula=tempformula,
                  data=train,
                  weights = Wt)
    
    
    test<-dataFourier%>%
      na.omit%>%
      mutate(Date=DateTime%>%date,
             HighTemp=HighTemp-setPointHigh,
             LowTemp=LowTemp-setPointLow)%>%
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
           DaysAhead<=10)%>%.$Error%>%.^2%>%mean%>%sqrt%T>%print%>%return
}

MLRTempMSE(c(log(0.616851054548242),log(0.340970080187513)))


bind_rows(ElasticTempPredictions%>%mutate(Method="Elastic Net"),
          MLRTempPredictions%>%mutate(Method="MLR"))%>%
  group_by(DaysAhead,Method)%>%
  filter(DaysAhead>0,DaysAhead<10)%>%
  summarise(RMSE=Error%>%.^2%>%mean%>%sqrt)%>%
  ggplot(aes(x=DaysAhead,y=RMSE,colour=Method))+geom_line()

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
  
  for(j in seq(1,length(WindowStart),by=1)){
    
    
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

ElasticClimateMSE(ElasticClimatePars)



MLRClimateMSE<-function(x){
  Start<-Sys.time()
  TempMSE<<-NULL
  ExpWeighting<-exp(x[1])
  #TempSetPointWeighting<-exp(x[2])
  #glmalpha<-(tanh(x[2])+1)/2
  #glmlambda<-exp(x[3])
  print(paste0(c(ExpWeighting),collapse=","))
  if(min(ExpWeighting)<=7/52 | max(ExpWeighting)>=50){return(NA)}
  MLRClimatePredictions<<-NULL
  
  for(j in seq(1,length(WindowStart),by=1)){
    
    
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

MLRClimateMSE(ElasticClimatePars[1])


bind_rows(ElasticClimatePredictions%>%mutate(Method="Elastic Net",Type="Climate"),
          MLRClimatePredictions%>%mutate(Method="MLR",Type="Climate"),
          OldClimatePredictions%>%mutate(Method="Old",Type="Climate"),
          ElasticTempPredictions2%>%mutate(Method="Elastic Net",Type="Temperature"),
          #MLRTempPredictions%>%mutate(Method="MLR",Type="Temperature"),
          MLRTempPredictions2%>%mutate(Method="MLR",Type="Temperature"),
          OldTempPredictions2%>%mutate(Method="Old",Type="Temperature"),
          #OldTempPredictions%>%mutate(Method="Old",Type="Temperature"),
          #OldTempPredictions3%>%mutate(Method="Old No Smooth No Lag",Type="Temperature"),
          #OldTempPredictions4%>%mutate(Method="Old No Smooth No Lag Same Interaction",Type="Temperature"),
          #OldTempPredictions5%>%mutate(Method="Old No Smooth No Lag Same Interaction No T3",Type="Temperature")
          )%>%
  group_by(DaysAhead,Method,Type)%>%
  filter(DaysAhead>0,!(Type=="Temperature" & DaysAhead>10))%>%
  summarise(RMSE=Error%>%.^2%>%mean%>%sqrt,
            Median=Error%>%abs%>%median)%>%
  ggplot(aes(x=DaysAhead,y=RMSE,colour=Method))+geom_line(size=1.2)+facet_wrap(.~Type,scales="free")+theme_bw()+scale_color_brewer(palette = "Set1")#+scale_y_continuous(limits = c(0,NA))

expand.grid(alpha=c(-2,0,2),
            lambda=c(3,5,7,9))->df

explore2<-NULL
for(i in 1:nrow(df)){
  
  RMSE<-ElasticClimateMSE(c(log(0.493900349939979),df$alpha[i],df$lambda[i]))
  
  data.frame(alpha=(tanh(df$alpha[i])+1)/2,
             lambda=exp(df$lambda[i]),
             RMSE=RMSE)%>%
    bind_rows(explore2)->explore2
}

explore2%>%ggplot(aes(x=lambda,y=RMSE,colour=factor(alpha)))+geom_line()+scale_x_log10()+scale_y_log10()


MLRTempMSE2<-function(x){
  Start<-Sys.time()
  TempMSE<<-NULL
  ExpWeighting<-exp(x[1])
  TempSetPointWeighting<-exp(x[2])
  print(paste0(c(ExpWeighting,TempSetPointWeighting),collapse=","))
  if(min(ExpWeighting,TempSetPointWeighting)<=7/52 | max(ExpWeighting,TempSetPointWeighting)>=50){return(NA)}
  MLRTempPredictions2<<-NULL
  TempSmoothing<-(tanh(x[3])+1)/2
  print(paste0(c(ExpWeighting,TempSetPointWeighting,glmalpha,glmlambda,TempSmoothing),collapse=","))
  if(min(ExpWeighting,TempSetPointWeighting)<=7/52 | max(ExpWeighting,TempSetPointWeighting)>=50){return(NA)}
  
  ElasticTempPredictions<<-NULL
  dataFourier2<-dataFourier%>%
    mutate(LongTemp=rollapply(Temp,24*8,function(x){
      
      TempSmoothing*sum((1-TempSmoothing)^((length(x)-1):0)*x)
    },align="right",na.pad=TRUE))
  
  for(j in seq(1,length(WindowStart),by=1)){
    
    
    print(WindowStart[j])
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
           DaysAhead<=10)%>%.$Error%>%.^2%>%mean%>%sqrt%T>%print%>%return
}

MLRTempMSE2(c(log(0.616851054548242),log(0.340970080187513)))

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
