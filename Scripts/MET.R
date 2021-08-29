#+
# Handle the MET date field - Need a "week" field because
# the American Heart Association recommends 500 METs/week 
# for optimal cardiovascular health.
MET$date <- as.Date(MET$ActivityMinute, "%m/%d/%Y %I:%M:%S %p")
MET$week <- week(MET$date)