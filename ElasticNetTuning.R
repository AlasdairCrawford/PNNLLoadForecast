library(glmnet)
library(glmnetUtils)

Terms<-expand.grid(a=c("Day","Hour"),b=1:12)%>%mutate(c=paste0(a,b,"sin+",a,b,"cos"))%>%.$c
Terms<-Terms[(parse_number(Terms) %in% c(1,2,3,4,5) & grepl("Hour",Terms))|
               (parse_number(Terms) %in% c(1,2,3) & grepl("Day",Terms)) ]
Terms<-c(Terms,
         "IsMon",
         "IsFri",
         "IsSat",
         "IsSun")

DayTerms<-Terms[grepl("Day",Terms)|grepl("Is",Terms)]
HourTerms<-Terms[grepl("Hour",Terms)]

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
  
  for(j in seq(1,length(WindowStart),by=5)){
    
    
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

Optimised<-optim(c(log(0.774),log(0.247),0,log(0.1)),ElasticTempMSE)

ElasticTempMSE(c(log(0.774),log(0.247),0,log(10)))

expand.grid(alpha=seq(0,1,by=1/3),
            lambda=-5:-3)->df

explore<-NULL
for(i in 1:nrow(df)){
  
  RMSE<-ElasticTempMSE(c(log(0.774),log(0.247),df$alpha[i],df$lambda[i]))
  
  data.frame(alpha=df$alpha[i],
             lambda=exp(df$lambda[i]),
             RMSE=RMSE)%>%
    bind_rows(explore)->explore
}

explore%>%ggplot(aes(x=lambda,y=RMSE,colour=factor(alpha)))+geom_line()+scale_x_log10()

ElasticTempMSE(c(log(0.774),log(0.247),1,0))

ElasticTempPredictions%>%
  merge(dataFourier%>%select(DateTime,Temp))%>%
  ggplot(aes(x=Temp,y=Error))+geom_point()+facet_wrap(.~Hour)+stat_smooth()


ElasticTempPredictions%>%
  merge(dataFourier%>%select(DateTime,HighTemp,LowTemp))%>%
  mutate(AvgTemp=(HighTemp+LowTemp)/2)%>%
  ggplot(aes(x=AvgTemp,y=Error))+geom_point()+facet_wrap(.~Hour)+stat_smooth()

ElasticTempPredictions%>%
  mutate(Hour=hour(DateTime))%>%
  group_by(Hour)%>%
  summarise(meanError=mean(Error))%>%
  ggplot(aes(x=Hour,y=meanError))+geom_bar(stat='identity')

ElasticTempPredictions%>%
  mutate(Date=date(DateTime))%>%
  group_by(Date)%>%
  summarise(meanError=mean(Error))%>%
  ggplot(aes(x=Date,y=meanError))+geom_line()

ElasticTempPredictions%>%
  mutate(Weekday=wday(DateTime,label=TRUE))%>%
  group_by(Weekday)%>%
  summarise(meanError=mean(Error))%>%
  ggplot(aes(x=Weekday,y=meanError))+geom_bar(stat='identity')

ElasticTempMSE2<-function(x){
  Start<-Sys.time()
  TempMSE<<-NULL
  ExpWeighting<-exp(x[1])
  TempSetPointWeighting<-exp(x[2])
  glmalpha<-(tanh(x[3])+1)/2
  glmlambda<-exp(x[4])
  TempSmoothing<-(tanh(x[5])+1)/2
  print(paste0(c(ExpWeighting,TempSetPointWeighting,glmalpha,glmlambda,TempSmoothing),collapse=","))
  if(min(ExpWeighting,TempSetPointWeighting)<=7/52 | max(ExpWeighting,TempSetPointWeighting)>=50){return(NA)}
  ElasticTempPredictions<<-NULL
  dataFourier2<-dataFourier%>%
    mutate(LongTemp=rollapply(Temp,24*8,function(x){
      
      TempSmoothing*sum((1-TempSmoothing)^((length(x)-1):0)*x)
    },align="right",na.pad=TRUE))
  for(j in seq(1,length(WindowStart),by=5)){
    
    
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
        SegmentedModel$psi["psi1.Temp","Est."]->setPointTemp
        SegmentedModel$psi["psi1.LongTemp","Est."]->setPointLongTemp
        
        
        
        tempformula<-paste0("Load~DateTime+(",
                            paste0(HourTerms,
                                   collapse="+"),
                            ")*(",
                            paste0(DayTerms,collapse="+"),
                            "+Temp:(Temp>0)+Temp+LongTemp:(LongTemp>0)+LongTemp)+Day5sin+Day5cos")%>%as.formula
      }else{
        setPointTemp<-0
        setPointLongTemp<-0
        train%>%
          mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)))->train
        
        
        tempformula<-paste0("Load~DateTime+(",
                            paste0(HourTerms,
                                   collapse="+"),
                            ")*(",
                            paste0(DayTerms,collapse="+"),
                            "+Temp+LongTemp)+Day5sin+Day5cos")%>%as.formula
      }
      
      if(!is.numeric(setPointTemp)){setPointTemp<-0}
      if(!is.numeric(setPointTemp)){setPointLongTemp<-0}
      
      
      train%>%
        mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)),
               Temp=Temp-setPointTemp,
               LongTemp=Temp-setPointLongTemp)->train
      
    }else{
      
      train%>%
        mutate(Wt=exp(ElapsedDays/((ExpWeighting)*365.25)))->train
      
      
      tempformula<-paste0("Load~DateTime+(",
                          paste0(HourTerms,
                                 collapse="+"),
                          ")*(",
                          paste0(DayTerms,collapse="+"),
                          "+Temp+LongTemp)+Day5sin+Day5cos")%>%as.formula
    }
    
    
    
    
    
    model<-glmnet(formula=tempformula,
                  data=train,
                  weights = Wt,
                  alpha=glmalpha,
                  lambda=glmlambda)
    
    
    test<-dataFourier2%>%
      na.omit%>%
      mutate(Date=DateTime%>%date,
             Temp=Temp-setPointTemp,
             LongTemp=LongTemp-setPointLongTemp)%>%
      filter(Date>WindowStart[j]-1,
             Date<=WindowStart[j]-1+n)
    
    
    test%>%
      transmute(DateTime,
                Load,
                Hour,
                Predicted=predict(model,test),
                Error=Predicted-Load,
                PredictionDate=WindowStart[j])%>%
      bind_rows(ElasticTempPredictions)->>ElasticTempPredictions
    
  }
  
  Duration<<-Sys.time()-Start
  print(Duration)
  ElasticTempPredictions%>%
    filter(PredictionDate>=(LoadData$DateTime%>%date%>%min+365))%>%.$Error%>%.^2%>%mean%>%sqrt%T>%print%>%return
}

ElasticTempMSE2(c(log(0.774),log(0.247),1,0,-1))

Optimised<-optim(c(log(0.774),log(0.247),0,log(0.1),-1),ElasticTempMSE2)

