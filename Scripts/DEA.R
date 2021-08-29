#' ### Part B: Data Exploration Analysis - It's about Sleeping! Move More and Sit Less
#' 
#' According to the National Sleep Foundation, It’s normal to take **10 to 20 minutes** 
#' to fall asleep once a person climbs into bed.  The Foundation also suggests the
#' **Recommended Hours of Sleep is 7-9 hours** for adults (aged 18-60). 
#' According to the 2018 Physical Activity Guidelines for Americans, 2nd edition, 
#' from the US CDC, adults should have 150 minutes physical activities per week. 
#' 
#+
df_DEA = master %>% select(Id, date, week, insomnia, TotalHrsAsleep, SedentaryMinutes, 
                           Calories, TotalSteps, TotalDistance) %>%
                          mutate(SedentaryHrs = SedentaryMinutes/60)

df_DEA = df_DEA [ !duplicated(df_DEA), ]
df_DEA <- df_DEA[complete.cases(df_DEA), ]
n_distinct(df_DEA$Id)
summary(df_DEA)


#' **Minutes to Fall Asleep (Suffer from insomnia)**
#+  
skim(df_DEA$insomnia)
ggplot(df_DEA, aes(x=insomnia))+ geom_histogram() +
  stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)

#' **How many cases suffered from insomnia every week?**
#' Origninal
#+
   df_DEA_Inso = df_DEA %>% group_by(Id, week) %>% summarise(avginso = mean(insomnia))
#'  
#' Filter by more than 25 minutes 
#+
df_insomnia = df_DEA %>% filter(insomnia > 25)
n_distinct(df_insomnia$Id)  

df_insomnia = df_insomnia %>% group_by(Id, week) %>% 
  summarise(avginsomnia = mean(insomnia))
 
#' remove the extreme cases
#+
df_insomnia_1 = df_insomnia %>% filter (avginsomnia < 100)

ggplot(data =df_insomnia_1, mapping = aes(x= week, y = avginsomnia))+ 
  geom_boxplot()

#' Sleep Pattern
#' **Hours of sleep**
#' 
#+
skim(df_DEA$TotalHrsAsleep)
ggplot(df_DEA, aes(x=TotalHrsAsleep))+ geom_histogram() +
  stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)

df_DEA_SlpHr = df_DEA %>% group_by(Id, week) %>% summarise(avgslphr = mean(TotalHrsAsleep))
summary(df_DEA_2$avgslphr)
skim(df_DEA_2$avgslphr)
ggplot(data =df_DEA_2, mapping = aes(x= week, y = avgslphr))+ geom_boxplot()


#' 
#' 
#' **Exercise at all?**
#' **SedentaryMinutes - Sitting or inactive**
#+
skim(df_DEA$SedentaryHrs)
ggplot(df_DEA, aes(x=SedentaryHrs))+ geom_histogram() +
  stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)
df_DEA_Sit = df_DEA %>% group_by(Id, week) %>% summarise(avgsit = mean(SedentaryHrs))
skim(df_DEA_Sit$avgsit)
summary(df_DEA_Sit$avgsit)
ggplot(data =df_DEA_Sit, mapping = aes(x=week, y = avgsit))+ geom_boxplot()


#' **Exercise-Steps**
#' **the CDC recommend that most adults aim for 10,000 steps per day.** 
#' **For most people, this is the equivalent of about 8 kilometers, or 5 miles.**
#' 
#+
skim(df_DEA$TotalSteps)
ggplot(df_DEA, aes(x=TotalSteps))+ geom_histogram() +
  stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)
df_DEA_Steps = df_DEA %>% group_by(Id, week) %>% summarise(avgsteps = mean(TotalSteps))
skim(df_DEA_Steps$avgsteps)
summary(df_DEA_Steps$avgsteps)
ggplot(data =df_DEA_Steps, mapping = aes(x=week, y = avgsteps))+ geom_boxplot()

#' **Exercise-Distance**
#+
skim(df_DEA$TotalDistance)
ggplot(df_DEA, aes(x=TotalDistance))+ geom_histogram() +
  stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)
df_DEA_Dist = df_DEA %>% group_by(Id, week) %>% summarise(avgdist = mean(TotalDistance))
skim(df_DEA_Dist$avgdist)
summary(df_DEA_Dist$avgdist)
ggplot(data =df_DEA_Dist, mapping = aes(x=week, y = avgdist))+ geom_boxplot()


#' How many **Calories Burned** among these people?
#+
skim(df_DEA$Calories)
df_DEA_Calo = df_DEA %>% group_by(Id, week) %>% summarise(avgcal = mean(Calories))
skim(df_DEA_a$avgcal)
summary(df_DEA_a$avgcal)
ggplot(data =df_DEA_a, mapping = aes(x=week, y = avgcal))+ geom_boxplot()


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










