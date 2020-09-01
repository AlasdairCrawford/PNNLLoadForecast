dataSummary<-"DataSummary.xlsx"%>%read_excel()

n<-31

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
    summarise(Load=sum(Load))%>%
    na.omit
  
  
  FourierCoefs<-1:12
  

  
  WindowStart<-seq(LoadData$DateTime%>%min%>%as.Date+31,LoadData$DateTime%>%max%>%as.Date-n,by=1)
  
  for(j in 1:length(WindowStart)){
    
    
  }
  
  
  
  
}