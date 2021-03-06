---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

This assignment analyzes two months of fitness data for an individual.  It tracks the number of steps taken in 5 minute intervals during October and November 2012.

The data is located in this repository, by unzipping the 'activity.zip' file.  The variables in the dataset are:

-  steps: Number of steps taken in the 5-minute interval (there are some missing values, denoted NA)
-  date: the date on which the measurement was taken
-  interval: an indentifier for the 5 minute interval the measurement was taken in

## Loading and preprocessing the data
The data is in csv format.  The date column is converted from a character type to a Date type.  A time stamp column is added by converting the interval column into a time format.  Finally the date and time columns are combined into a POSIX time object.

```r
#load the data from activity.csv from working directory
activityData <- read.csv("activity.csv", na.strings=c("NA"), header=TRUE)
#convert date column to Date type (instead of character)
activityData$date <- as.Date(activityData$date)
#define a function to convert the interval field to a more familiar HH:MM format
#this is from a suggestion by Jerry Kurata on https://class.coursera.org/repdata-012/forum/thread?thread_id=36
intToHHMM <- function (i) {  sprintf("%02d:%02d", i %/% 100, i %% 100)}
#add a new column with the 24hr time of the measurement, convert it to a factor
activityData$time <-  intToHHMM(activityData$interval)
activityData$time <- factor(activityData$time)
#for completeness, add another column that combines the date and time fields into a POSIX date time object
activityData$dateTime <- strptime(paste(activityData$date, activityData$time), format="%Y-%m-%d %H:%M")
```

## What is mean total number of steps taken per day?
The activity data for each day are combined using the aggregate function.  The number of steps per interval is summed.  

```r
totalSteps <- aggregate(activityData["steps"], by=activityData["date"], FUN=sum, na.rm=TRUE)
hist(totalSteps$steps, breaks=20, xlab="Total Steps per day", main="Histogram of Total Steps per day")
```

![plot of chunk totals](figure/totals-1.png) 

```r
meanSteps<-mean(totalSteps$steps)
medianSteps<-median(totalSteps$steps)
sprintf("The mean number of steps per day is %.1f and the median is %.1f", meanSteps, medianSteps)
```

```
## [1] "The mean number of steps per day is 9354.2 and the median is 10395.0"
```

The histogram shows the distribution in the daily total number of steps.  However, there are a large number of days with 0 total steps.  This is due to a large number of missing (NA) values in the data.  The mean number of steps per day is 9354.2 and the median is 10395.0.

## What is the average daily activity pattern?
By averaging the number of steps during the same 5 minute interval everyday, an activity profile over the course of a day can be obtained.  The data is aggregated by the time interval, and the mean of the steps taken in every time interval is obtained.


```r
dailyPattern <- aggregate(activityData["steps"], by=activityData["time"], FUN=mean, na.rm=TRUE)
colnames(dailyPattern)[2] <- "averageSteps"

plot(x=strptime(dailyPattern$time, format="%H:%M"),y=dailyPattern$averageSteps, type='l', xlab="Time", ylab="Average steps")
```

![plot of chunk averageDaily](figure/averageDaily-1.png) 

The time interval that has the highest mean number of steps is found to occur at 8:35 PM.

```r
#index of time interval with maximum steps
index <- which.max(dailyPattern$averageSteps)
#time of maximum steps
#maxTime <- strptime(dailyPattern[index,1], format="%H:%M")
maxTime <- dailyPattern[index,1]
sprintf("The 5 minute interval with the highest average number of steps is at %s", strftime(strptime( maxTime,format="%H:%M"), format="%H:%M %p"))
```

```
## [1] "The 5 minute interval with the highest average number of steps is at 08:35 AM"
```



## Inputing missing values
The number of missing values makes the data appear to have more inactive periods than it actually does (shown in the histrogram above as a large number of zero days.)  The bias is alleviated by using the average number of steps during each interval, and filling in NA values with this interval average.

```r
#find the total number of missing values
numMissing <- sum(is.na(activityData$steps))
sprintf('There are %s missing values.', numMissing)
```

```
## [1] "There are 2304 missing values."
```

```r
#merge average number of steps by interval into activityData
activityData <- merge(activityData, dailyPattern, by="time")
#sort activityData so the rows are in chronological order
activityData<-activityData[order(activityData$date),]
#if the steps value is NA, replace it with the interval average (in column 'averageSteps')
activityData$steps[is.na(activityData$steps)] <- activityData$averageSteps[is.na(activityData$steps)]

totalStepsFilled <- aggregate(activityData["steps"], by=activityData["date"], FUN=sum, na.rm=TRUE)
hist(totalStepsFilled$steps, breaks=20, xlab="Total Steps per day", main="Histogram of Total Steps per day")
```

![plot of chunk fillMissing](figure/fillMissing-1.png) 

This histogram shows has a more unbiased distribution than the one above, and doesn't have a large number of apparent zero step days.


```r
meanSteps<-mean(totalStepsFilled$steps)
medianSteps<-median(totalStepsFilled$steps)
meanSteps
```

```
## [1] 10766.19
```

```r
medianSteps
```

```
## [1] 10766.19
```

```r
sprintf("After filling in missing values with inteval averages, the mean number of steps per day is %.1f and the median is %.1f", meanSteps, medianSteps)
```

```
## [1] "After filling in missing values with inteval averages, the mean number of steps per day is 10766.2 and the median is 10766.2"
```


## Are there differences in activity patterns between weekdays and weekends?
The activity patterns of weekdays vs. weekends can be compared by labeling the observations as belonging to a weekend or a weekday, and aggregrating the data by both the interval and the day type.

```r
#define weekend day classes
weekendDays = c('Saturday', 'Sunday')
#initialize dayType column, first by giving every day the weekend label
activityData$dayType <- 'weekday'
#if the weekday matches the weekend day pattern, give it the weekend label
activityData$dayType[weekdays(activityData$date) %in% weekendDays] <- 'weekend'
#aggregate the data by both the interval timestamp and the day type
dailyPattern2<-aggregate(list(aveSteps=activityData$steps), by=list(time=activityData$time,dayType=activityData$dayType),FUN=mean)
library(lattice)
xyplot(dailyPattern2$aveSteps ~ as.POSIXct(strptime(dailyPattern2$time, format="%H:%M")) | dailyPattern2$dayType, 
       layout=c(1,2), type='l', xlab='Time', ylab="Number of steps",
       scales=list(format="%H:%M"))
```

![plot of chunk weekdayCompare](figure/weekdayCompare-1.png) 

The weekday data shows a larger number of steps earlier in the morning (between 6am and 11am), and a lower and relativly constant number of steps during the day, which drops off around 7pm.  The weekend data suggests strong activity starts later in the morning (about 9am) and a higher level of activity during the day, and drops off around 9pm.
