#' ---
#' title: "Capstone Project"
#' ---

#+
library(easypackages)
libraries("tidyverse", "lubridate", "skimr", "DataExplorer", "ggcorrplot")

options(scipen = 999)

#' ### About the data
#' The data came from FitBit Fitness Tracker Data, which includes 18 data files.
#' According to the Case 2 project instructions, there are about 30 users.  
#+
# Import data files
  dailyActivity <- read_csv("Data/dailyActivity_merged.csv")
  sleepDay <- read_csv("Data/sleepDay_merged.csv")
  hourlySteps <- read_csv("Data/hourlySteps_merged.csv")
  hourlyIntensities <- read_csv("Data/hourlyIntensities_merged.csv")


#' ### Part A: Data Preparation
#' After I look at the data files, I think there are four data sets will help me 
#' to do the project. The questions are: 
#' 
#'  * Are all files have 30 users?
#'  * Which file should I use as the primary file (the foundation)? 
#' 
#' So here is my data preparation plan:
#'  
#'   * To see if all files have 30 users.
#'   * To determine the primary file, I need to see which file has the most cases,
#'     and the most useful variables.
#'   * Then, I will join the primary file and the other 3 with left join.
#'   * Once I have the master dataset, I will check the data.
#'
#+
#    Number of variables in each file
     dim(dailyActivity)
     dim(sleepDay)
     dim(hourlySteps)
     dim(hourlyIntensities)
     
#+
#   Number of unique cases in each file
     n_distinct(dailyActivity$Id)
     n_distinct(sleepDay$Id)
     n_distinct(hourlySteps$Id)
     n_distinct(hourlyIntensities$Id)
#' 
#' #### Create a master data file with all needed variables
#' Since dailyActivity has the most relevant variables and all 33 cases, I 
#' decided to use it as the foundation file and build the master from it. 
#' The primary keys will be Id and Date (created from field with date info).
#' For the sake of joining files and analyzing data, I need to handle the date
#' variable in each file first. This procedure includes:
#'  
#' * Convert the existing date field from char to date
#' * Extract the weekdays from the new date field
#' * Find out the number of week
#' 
#+ 
# Handle the dailyActivity file date field and order the weekdays 
    dailyActivity$date <- as.Date(dailyActivity$ActivityDate, "%m/%d/%Y")
    dailyActivity$days <- weekdays(dailyActivity$date) 
    dailyActivity$week <- strftime(dailyActivity$date, format = "%V")
    
    dailyActivity$days <- factor(dailyActivity$days, 
                                 c("Saturday", "Sunday", "Monday", "Tuesday",
                                   "Wednesday","Thursday", "Friday"))
#+
# Handle the sleepDay file date field
    sleepDay$date = as.Date(sleepDay$SleepDay, "%m/%d/%Y %I:%M:%S %p")

#'  Create a new variable "insomenia" in the sleepDay file for analysis
#'  The large the value means that the longer time to fall asleep, which may be  
#'  caused by many reasons like **stress**. Thus, we use it as an indicator of
#'  stress.
#'  
#+    
    sleepDay <- sleepDay %>% 
      mutate (insomnia = TotalTimeInBed - TotalMinutesAsleep)

#' We would also like to convert the TotalMinutesAsleep minutes into hours. 
#+    
    sleepDay <- sleepDay %>% 
       mutate (TotalHrsAsleep = round(TotalMinutesAsleep / 60)) 
    
#+
# Handle the hourlySteps date field    
   hourlySteps$date <- as.Date(hourlySteps$ActivityHour, "%m/%d/%Y %I:%M:%S %p")
   hourlySteps$hr <- parse_datetime(hourlySteps$ActivityHour, '%m/%d/%Y %I:%M:%S %p')
   hourlySteps$hour <- hour(hourlySteps$hr) 
   
#+
# Handle the hourlyIntensities date field    
   hourlyIntensities$date <- as.Date(hourlyIntensities$ActivityHour, "%m/%d/%Y %I:%M:%S %p")
   hourlyIntensities$hr <- parse_datetime(hourlyIntensities$ActivityHour, '%m/%d/%Y %I:%M:%S %p')
   hourlyIntensities$hour <- hour(hourlyIntensities$hr)    
   
#+
# Since dailyActivity has the most obserations, I use it as the primary data set
# and left join other data sets to it so that most info will be preserved. If I
# use "merge", only cases found in both datasets will be kept, which is not I 
# prefer.
   master  <- dailyActivity %>% left_join(sleepDay, by=c("Id","date"))
   master  <- master  %>% left_join(hourlySteps, by=c("Id", "date"))
   master  <- master  %>% left_join(hourlyIntensities, by=c("Id", "date"))
  
#' #### Data Cleaning
#' In terms of **data type**, let's check the structure to find out which one needs 
#' to be changed.
#+
   str(master)
#' Based on the result, "master" is a data.table, not data.frame. It's better to 
#' convert it to data.frame.
#+ 
   master <- as.data.frame(master)
#' Next, Id is the only variable I need to change from numeric to character. 
#+ 
   master$Id <- as.character(master$Id)
#' 
#' Also, I need to **remove the redundant variables**, which is a result after joining
#' the datasets. In the dailyActivity df, ActivityDate is a character. Since I have created a 
#' date field "date" to replace it, and also checked "date" has the same nrow and
#' no NA in this field, it is safe to remove the "dailyActivity".
#+  
   master = master[ , -2]
#'
#' In the master df, the ActivityHour.x is the same as hr.x. Both came from the 
#' hourlySteps file. hr.x is a date time data type. Same with ActivityHour.y and
#' hr.y. Therefore, I will remove the ActivityHour.x and ActivityHour.y
#+
   master = master[ -c(24, 28)]
   
#' I will also **rename** couple variables. hr.x, hr.y, hour.x, and hour.y.
#' ".x" is for steps and ".y" is for intensities.
#+ 
  names(master)[names(master)== "hr.x"] <- "stephr"
  names(master)[names(master)== "hour.x"] <- "step_24hour"
  names(master)[names(master)== "hr.y"] <- "intenhr"
  names(master)[names(master)== "hour.y"] <- "inten_24hour"

#' Deal with **Missing values (NA)**  
#' To handle missing values, we need to determine where the missing values come
#' from. Let's see the four individual data sets first.
#+
   table(is.na(dailyActivity))
   table(is.na(sleepDay))
   table(is.na(hourlyIntensities))
   table(is.na(hourlySteps))

#' There is no missing value in each of these data sets. How about in the master
#' dateframe?
#+ 
   table(is.na(master))

#'  In the master dataframe, there are lots of missing values. All of these 
#'  missing values come from 3 data sets. It is understandable because we used 
#'  left join to join dailyActivity with the 3 data sets. Since dailyActivity 
#'  contains more observations than the rest, it is reasonable to see missing 
#'  values from the 3 data set. So should we keep completed cases only? 
#'  The answer is **no**.
#'  
#'  It is because the sample size of the original data set is small (33 cases).
#'  If we only keep the completed cases, the sample size will be shrink even 
#'  further. Therefore, I prefer to keep all cases. But ignore or remove NA 
#'  in certain analytical topics.
#' 
#' 
#' Also, we noticed that there are some **outliers** in the data set, e.g.
#' TotalSteps, TotalDistance, VeryActiveDistance, etc. Will it be an issue?
#' Let's identify them first based on the maximum value.
#+    
   m <- master %>% summarise(m1 = max(TotalSteps), m2 = max(TotalDistance), 
                        m3 = max(VeryActiveDistance))

   master %>% select(Id, TotalSteps) %>% group_by (Id) %>%
      filter(TotalSteps == m$m1) %>% summarise(n_distinct(n()))
   
   master %>% select(Id, TotalDistance) %>% group_by (Id) %>%
      filter(TotalDistance >= m$m2) %>% summarise(n_distinct(n()))
   
   master %>% select(Id, VeryActiveDistance) %>% group_by (Id) %>%
      filter(VeryActiveDistance >= m$m3 ) %>% summarise(n_distinct(n()))
   
#' The results show that these extreme cases are the same person. It is reasonable
#' to think that these values are not resulted from typo. Instead, they are the
#' actual records of the same person. Therefore, we will not remove these extreme
#' cases but will be cautious when analyzing the data. 
#'   
#' After checking these aspects, the master data set is ready to use.
   
   
 ## master_test2 = master_test2[!duplicated(master_test2), ]
   
   
   
   
   
   
   
   
   
   
                     
