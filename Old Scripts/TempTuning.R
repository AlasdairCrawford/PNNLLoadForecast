library(tis)
library(segmented)
library(zoo)
library(tidyverse)
library(readxl)
library(feather)
library(lubridate)
dataSummary<-"DataSummary.xlsx"%>%read_excel()%>%
  mutate(Duration=(End%>%as.numeric-Start%>%as.numeric)/60/60/24/365.25)%>%
  filter(Duration>1)


n<-31


Terms<-expand.grid(a=c("Day","Hour"),b=1:12)%>%mutate(c=paste0(a,b,"sin+",a,b,"cos"))%>%.$c
Terms<-Terms[(parse_number(Terms) %in% c(1,2,3,4,5) & grepl("Hour",Terms))|
               (parse_number(Terms) %in% c(1,2,3) & grepl("Day",Terms)) ]
Terms<-c(Terms,
         "IsFri",
         "IsSat",
         "IsSun")

DayTerms<-Terms[grepl("Day",Terms)|grepl("Is",Terms)]
HourTerms<-Terms[grepl("Hour",Terms)]


tempformula<-paste0("Load~DateTime+(",
                    paste0(HourTerms,
                           collapse="+"),
                    ")*(",
                    paste0(DayTerms,collapse="+"),
                    "+LongTemp:(LongTemp>0)+LongTemp+Temp:(Temp>0)+Temp)+Day5sin+Day5cos+I(LongTemp^3)+I(LongTemp^2)")%>%as.formula

SmoothingFactorsTemp<-NULL

for(i in dataSummary$Dataset%>%unique){
  
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
  
  TempMSE<-function(x){
    Start<-Sys.time()
    TempMSE<<-NULL
    ExpWeighting<-exp(x[1])
    TempSetPointWeighting<-exp(x[2])
    print(paste0(c(ExpWeighting,TempSetPointWeighting),collapse=","))
    if(min(ExpWeighting,TempSetPointWeighting)<=7/52 | max(ExpWeighting,TempSetPointWeighting)>=50){return(NA)}
    TempPredictions<<-NULL

    for(j in 1:length(WindowStart)){
      
      
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
                                        HighTemp=mean(train$LowTemp)))},
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
      

     
      
      
      model<-train%>%
        lm(data=.,
           formula=tempformula,
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
                  PredictionDate=WindowStart[j])%>%
        bind_rows(TempPredictions)->>TempPredictions
      
    }
    
    Duration<<-Sys.time()-Start
    print(Duration)
    TempPredictions%>%
      filter(PredictionDate>=(LoadData$DateTime%>%date%>%min+365))%>%.$Error%>%.^2%>%mean%>%sqrt%T>%print%>%return
  }
  
  SmoothingFactors%>%
    filter(Dataset==i)%>%
    .$Weight->TimeWeighIni
  Optimised<-optim(log(c(0.4,0.4)),TempMSE)
  
  TempPredictions%>%write_feather(paste0("Predictions\\TempModel\\",i,".feather"))
  
  data.frame(Dataset=i,
             Duration=Duration%>%as.numeric,
             Weight=exp(Optimised$par[1]),
             TempWeight=exp(Optimised$par[2]),
             Start=WindowStart%>%min,
             End=WindowStart%>%max)%>%
    bind_rows(SmoothingFactorsTemp)->SmoothingFactorsTemp
  
}

ClimatePredictions<-read_feather(paste0("Predictions\\ClimateModel\\","OPALCO",".feather"))

ClimatePredictions%>%.$Error%>%summary