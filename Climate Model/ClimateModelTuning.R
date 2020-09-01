library(tis)


dataSummary<-"DataSummary.xlsx"%>%read_excel()

n<-31


Terms<-expand.grid(a=c("Day","Hour"),b=1:12)%>%mutate(c=paste0(a,b,"sin+",a,b,"cos"))%>%.$c
Terms<-Terms[(parse_number(Terms) %in% c(1,2,3,4,5) & grepl("Hour",Terms))|
               (parse_number(Terms) %in% c(1,2,3) & grepl("Day",Terms)) ]
Terms<-c(Terms,
         "IsFri",
         "IsSat",
         "IsSun")

DayTermsC<-Terms[grepl("Day",Terms)|grepl("Is",Terms)]
HourTerms<-Terms[grepl("Hour",Terms)]


climateformula<-paste0("Load~DateTime+(",
                       paste0(HourTerms,
                              collapse="+"),
                       ")*(",
                       paste0(DayTermsC,collapse="+"),")")%>%as.formula

SmoothingFactors<-NULL

for(i in dataSummary$Dataset%>%unique%>%.[5]){
  
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
    summarise(Load=sum(Load))%>%
    na.omit%>%
    mutate(DateTime=round_date(DateTime,"hour"))%>%
    group_by(DateTime)%>%
    summarise(Load=mean(Load))%>%
    ungroup->LoadData
  
  
  FourierCoefs<-1:12
  
  dataFourier<-LoadData%>%
    mutate(Day=yday(DateTime),
           Year=year(DateTime),
           Hour=hour(DateTime),
           Weekday=wday(DateTime),
           Weekday=(Weekday-2)%%7+1, ##i want sunday at the end of the week
           Weekend=Weekday>=6,
           Holiday=DateTime%>%as.Date%>%isHoliday(board=TRUE,inaug=FALSE))%>%
    transmute(DateTime,
              Load,
              Hour,
              Day,
              IsFri=Weekday==5,
              IsSat=(Weekday==6)|Holiday,
              IsSun=(Weekday==7),
              ElapsedDays=date(DateTime)%>%as.numeric-first(date(DateTime))%>%as.numeric)
  
  for(j in FourierCoefs){
    dataFourier<-dataFourier%>%
      mutate(!!paste0("Day",j,"sin"):=sin((Day+Hour/24)*2*j*pi/365),
             !!paste0("Day",j,"cos"):=cos((Day+Hour/24)*2*j*pi/365),
             !!paste0("Hour",j,"sin"):=sin(Hour*2*j*pi/24),
             !!paste0("Hour",j,"cos"):=cos(Hour*2*j*pi/24))
  }
  
  WindowStart<-seq(LoadData$DateTime%>%min%>%as.Date+n,LoadData$DateTime%>%max%>%as.Date-n,by=1)
  
  ClimateMSE<-function(x){
  Start<-Sys.time()
  ClimatePredictions<<-NULL
  print(paste0(i,"-",exp(x)))
  for(j in 1:length(WindowStart)){
    
    
    #print(WindowStart[j])
    train<-dataFourier%>%
      na.omit%>%
      mutate(Wt=exp(ElapsedDays/(exp(x)*24*365.25)),
             Date=DateTime%>%date)%>%
      filter(Date<=WindowStart[j]-1)
    
    
    model<-train%>%
      lm(data=.,
         formula=climateformula,
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
                PredictionDate=WindowStart[j])%>%
      bind_rows(ClimatePredictions)->>ClimatePredictions
    
  }
  
  Duration<<-Sys.time()-Start
  print(Duration)
  ClimatePredictions%>%
    filter(PredictionDate>=(LoadData$DateTime%>%date%>%min+180))%>%.$Error%>%.^2%>%mean%>%sqrt%T>%print%>%return
  }
  
  Optimised<-optim(log(0.4),ClimateMSE,method="Brent",lower=log(1/365),upper=log(50))
  
  ClimatePredictions%>%write_feather(paste0("Predictions\\ClimateModel\\",i,".feather"))
  
  data.frame(Dataset=i,
             Duration=Duration%>%as.numeric,
             Weight=exp(Optimised$par),
             Start=WindowStart%>%min,
             End=WindowStart%>%max)%>%
    bind_rows(SmoothingFactors)->SmoothingFactors
  
}

ClimatePredictions<-read_feather(paste0("Predictions\\ClimateModel\\","OPALCO",".feather"))

ClimatePredictions%>%.$Error%>%summary