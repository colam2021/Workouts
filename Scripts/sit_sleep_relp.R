# btw line 300 & 305

## Might move to next session
#' The longer the Sedentary Min, the fewer hours of asleep 
ggplot(data=df_DEA)+
  geom_smooth(mapping = aes(x=SedentaryHrs, y=TotalHrsAsleep)) +
  geom_point(mapping = aes(x=SedentaryHrs, y=TotalHrsAsleep))

# cbind the 4 DEA
df_DEA_revised = cbind(df_DEA_Inso,df_DEA_SlpHr,df_DEA_Sit, df_DEA_Calo)
df_DEA_revised <- df_DEA_revised [ , -c(4,5,7, 8, 10, 11)]
names(df_DEA_revised)[names(df_DEA_revised)== "Id...1" ]<- "Id"
names(df_DEA_revised)[names(df_DEA_revised)== "week...2" ]<- "week"
