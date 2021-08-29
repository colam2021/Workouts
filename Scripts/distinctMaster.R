#' ### Part C: Analysis - Relationships Between Sleep & Exercise
#+
df_relp = master [ !duplicated(master), ]
df_relp <- df_relp[complete.cases(df_relp), ]
n_distinct(df_relp$Id)
summary(df_relp)

## Dont need these two
## ggplot(data=df_relp) +
##   geom_smooth(mapping = aes(x=TotalMinutesAsleep, y=TotalTimeInBed))+ 
##   geom_point(mapping = aes(x=TotalMinutesAsleep, y=TotalTimeInBed))

## ggplot(data=df_relp) +
##   geom_point(aes(x=TotalMinutesAsleep, y=TotalTimeInBed))+ 
## geom_smooth(method =lm, aes(x=TotalMinutesAsleep, y=TotalTimeInBed))

# select relevant var 
df_relp_1 = df_relp %>% select( c( 14, 2, 3, 10, 11, 12, 13, 22, 23))

# Exercises	Sleephrs     2 3 10-14 20  22 23 
# Exercises	Fallasleep
# Exercises	Calories
# Exercises	Sed

library(ggcorrplot)  
corr <- round(cor(df_relp_1), 1) 
ggcorrplot(corr, hc.order = TRUE, type = "upper",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))

# Let's look into several interesting relps
#  "Calories"             "TotalSteps"           "TotalDistance"       
# VeryActiveMinutes"    "FairlyActiveMinutes"  "LightlyActiveMinutes"
#  "SedentaryMinutes"     "insomnia"             "TotalHrsAsleep"


ggplot(data=df_relp_1) +
  geom_point(aes(x=Calories, y=VeryActiveMinutes))+ 
  geom_smooth(method =lm, aes(x=Calories, y=VeryActiveMinutes))

ggplot(data=df_relp_1) +
  geom_point(aes(x=SedentaryMinutes, y=TotalHrsAsleep))+ 
  geom_smooth(method =lm, aes(x=SedentaryMinutes, y=TotalHrsAsleep))


#' The longer the Sedentary Min, the fewer hours of asleep 
## ggplot(data=df_DEA)+
##   geom_smooth(mapping = aes(x=SedentaryMinutes, y=TotalHrsAsleep)) +
##   geom_point(mapping = aes(x=SedentaryMinutes, y=TotalHrsAsleep))




## Proportion of goog sleeps and insomania 













