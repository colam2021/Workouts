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
