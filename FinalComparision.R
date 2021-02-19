Models<-expand.grid(c("Climate","Temp"),c("Elastic","MLR","Factor"))%>%mutate(Name=paste0(Var1,"Model",Var2))%>%.$Name

Comparision<-NULL
for(i in dataSummary$Dataset%>%unique){
  
  for(j in Models)
  {
    if(paste0("Predictions\\",j,"\\",i,".feather")%>%file.exists){
    paste0("Predictions\\",j,"\\",i,".feather")%>%
      read_feather%>%
      group_by(DaysAhead)%>%
      summarise(RMSE=Error%>%.^2%>%mean%>%sqrt,
                SD=Load%>%sd)%>%
      mutate(Dataset=i,
             Model=j,
             RMSE=RMSE/SD)%>%
      bind_rows(Comparision)->Comparision
    }
  }
  
}

Comparision%>%
  mutate(Forecast=ifelse(grepl("Climate",Model),"Climate","Temp"))%>%
  group_by(Forecast)%>%
  mutate(Model=gsub(Forecast,"",Model),
         Model=gsub("Model","",Model))%>%
  filter(!(Dataset=="Bainbridge"),
         !(Forecast=="Temp" & DaysAhead>10))%>%
  ggplot(aes(x=DaysAhead,y=RMSE,colour=Model))+
  facet_wrap(Dataset~Forecast,scales="free",ncol=2)+
  geom_line(size=1.2)+
  theme_bw()+
  scale_color_brewer(palette = "Set1")