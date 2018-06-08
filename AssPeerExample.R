library(dplyr)
# Loading and preprocessing the data
# Assume the source file is in the current directory.
# If it is in another folder, setwd() can be used in the R script below (added the commented line for clarity).

#setwd("C:/work/data_science/Course5_Reprod/data")
act <- read.csv("activity.csv")
act$date <- as.Date(act$date, '%Y-%m-%d')
head(act, 20)
# What is mean total number of steps taken per day?
#         Get the total steps per each day - ignoring NAs are per the assignment:
        
        tsteps <- aggregate(steps ~ date, data = act, sum, na.rm = TRUE)
head(tsteps)
# Draw a histogram of the steps taken per day:
        
        hist(tsteps$steps
             , xlab = "Steps"
             , main = "Histogram of steps per day (NAs excluded)"
             , breaks = 25
             , col = "red"
        )
# Get the Mean:
        
        mean(tsteps$steps)
# Get the Median:
        
        median(tsteps$steps)
# What is the average daily activity pattern?
#         Plot average steps per interval:
        
        agg <- aggregate(steps ~ interval, data = act, mean, na.rm = TRUE)
plot(agg$interval
     ,agg$steps
     ,type = "l"
     ,xlab = "Interval"
     ,ylab = "Total steps"
     ,main = "Average steps per interval"
)
# Show the interval with max number of steps:
        
        agg[which.max(agg$steps),1]
# Imputing missing values
# Total number of missing values (NAs):
        
        sum(is.na(act))
# To impute missing values, use an average per interval and plug into the main data frame:
        
        act_agg <- merge(act, agg, by = "interval")
act_agg$steps <- act_agg$steps.x
suppressWarnings(
        act_agg$steps[is.na(act_agg$steps.x) == TRUE] <- act_agg$steps.y[is.na(act_agg$steps.x) == TRUE]
)
# Create new dataset with missing data filled in:
        
        act_clean <- arrange(select(act_agg, steps, date, interval), date, interval)
head(act_clean, 20)
Histogram with the clean dataset:
        
        tsteps <- aggregate(steps ~ date, data = act_clean, sum, na.rm = TRUE)
hist(tsteps$steps
     , xlab = "Steps"
     , main = "Histogram of steps per day (NAs imputed)"
     , breaks = 25
     , col = "green"
)
# Get the Mean:
        
        mean(tsteps$steps)
# Get the Median:
        
        median(tsteps$steps)
# There is no significant impact of imputing missing values on the estimates.

# Are there differences in activity patterns between weekdays and weekends?
#         Create a new factor variable we_fact to indicate weekend vs weekday.

act_clean$dow <- weekdays(act_clean$date)
act_clean$we_flag <- (act_clean$dow %in% c("Sunday", "Saturday"))
act_clean$we_fact <- "weekday"
act_clean[act_clean$we_flag,]$we_fact <- "weekend"
act_clean$we_fact <- as.factor(act_clean$we_fact)
str(act_clean)
# Calculate averages weekends vs weekdays

act_weekday <- act_clean[act_clean$we_flag == F,]
act_weekend <- act_clean[act_clean$we_flag == T,]
act_weekday_avg <- aggregate(steps ~ interval, data = act_weekday, mean)
act_weekend_avg <- aggregate(steps ~ interval, data = act_weekend, mean)
head(act_weekday_avg)
head(act_weekend_avg)

# Draw panel plots

par(mfrow=c(1,2))
plot(act_weekend_avg$interval, act_weekend_avg$steps
     , type = "l"
     , col = "green"
     , xlab = "Interval"
     , ylab = "Steps"
     , main = "Steps per interval (weekends)"
)
plot(act_weekday_avg$interval, act_weekday_avg$steps
     , type = "l"
     , col = "red"
     , xlab = "Interval"
     , ylab = "Steps"
     , main = "Steps per interval (weekdays)"
)