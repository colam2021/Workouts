#' ### Part D: Analysis - Workouts Patterns 
#' **Which weekdays exercise more?** 
#+ 
df_I = df_relp %>% group_by(days) %>% 
  summarise(avgIntensity = mean(TotalIntensity))

df_S = df_relp %>% group_by(days) %>% 
  summarise(avgSteps = mean(TotalSteps))

df_D = df_relp %>% group_by(days) %>% 
  summarise(avgDist = mean(TotalDistance))

df_gpA = cbind(df_I, df_S[!names(df_S) %in% 
                                        names(df_I)])

df_gpA = cbind(df_gpA, df_D[!names(df_D) %in% 
                                 names(df_gpA)])

df_gpA <- df_gpA %>% gather(wkout, value, 
                             c(avgIntensity, avgDist))

ggplot(df_gpA) +
  geom_col(mapping = aes(x=days, y=value)) + 
  facet_wrap(~wkout)

#' We see a cycle that begin on Tuesday and losing the momentum till Fri.
#' Then regain the momentum on Sat.
#' On Sunday and Monday, users spent the least time on exercises.  
#' Maybe users need to rest or do other activities.  
#' 
#'
#' **Will the intensity of exercises differ in different weekdays?**
#' 
#+
df_avgVAD = df_relp %>% group_by(days) %>% 
  summarise(avgVAD = mean(VeryActiveDistance))

df_avgMAD = df_relp %>% group_by(days) %>% 
  summarise(avgMAD = mean(ModeratelyActiveDistance))

df_avgLAD = df_relp %>% group_by(days) %>% 
  summarise(avgLAD = mean(LightActiveDistance))

df_avgSAD = df_relp %>% group_by(days) %>% 
  summarise(avgSAD = mean(SedentaryActiveDistance))


df_gpB = cbind(df_avgVAD, df_avgMAD[!names(df_avgMAD) %in% 
                                      names(df_avgVAD)])

df_gpB = cbind(df_gpB, df_avgLAD[!names(df_avgLAD) %in% 
                                   names(df_gpB)])

df_gpB = cbind(df_gpB, df_avgSAD[!names(df_avgSAD) %in% 
                                   names(df_gpB)])

df_gpB <- df_gpB %>% gather(intense, value, 
                             c(avgVAD:avgLAD))

ggplot(df_gpB) +
  geom_col(mapping = aes(x=days, y=value)) + 
  facet_wrap(~intense)

ggplot(df_gpB) +
  geom_col(mapping = aes(x=days, y=avgSAD))

#' 
#' **Which hours are likely to exercise?**
#+ 

df_Int_24hrs = df_relp %>% group_by(inten_24hour) %>% 
  summarise(avg_Inten24hrs = mean(TotalIntensity))

ggplot(df_Int_24hrs) +
  geom_col(mapping = aes(x=inten_24hour, y=avg_Inten24hrs))


df_Step_24hrs = df_relp %>% group_by(step_24hour) %>% 
  summarise(avg_Step24hrs = mean(StepTotal))

ggplot(df_Step_24hrs) +
  geom_col(mapping = aes(x=step_24hour, y=avg_Step24hrs))










