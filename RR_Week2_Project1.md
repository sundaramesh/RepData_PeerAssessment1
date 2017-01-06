# RR_Week2_Project1
Ramesh Sundaram  
January 5, 2017  
Includes
---
# Coursera - Reproducible Research

## Week 2- Project Assessment 1

## Loading and preprocessing the data



```r
# Setting the current path to input data location
setwd("C:/CourseRa/Course 5 - Reproducible Research/Project1")
actvyData <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
library(ggplot2)
# Calculate the total number of steps taken per day
totalSteps <- tapply(actvyData$steps, actvyData$date, FUN=sum, na.rm=TRUE)
# Histogram for the Total Steps
hist(totalSteps, main="Histogram of Total Steps", xlab="Total number of steps taken each day")
```

![](RR_Week2_Project1_files/figure-html/drawHist1-1.png)<!-- -->

```r
# Calculate and report the mean and median of the total number of steps taken per day
meanVal <- mean(totalSteps, na.rm=TRUE)
medianVal <- median(totalSteps, na.rm=TRUE)
```
The mean value for the # of steps taken is 9354.2295082

The median value for the # of steps taken is 10395

## What is the average daily activity pattern?

The plot below shows the Average Daily Activity Pattern


```r
library(ggplot2)
dailyAvg <- aggregate(x=list(steps=actvyData$steps), by=list(interval=actvyData$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=dailyAvg, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("Average # of steps taken")
```

![](RR_Week2_Project1_files/figure-html/PlotSteps-1.png)<!-- -->
From the Data set the 5-minute interval contains the maximum number of steps

## Maximum of Steps in 5-Minute Interval


```r
dailyAvg[which.max(dailyAvg$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values


```r
missingValue <- sum(is.na(actvyData$steps))
```
Total number of missing values 2304

All of the missing values are filled in with mean value for that 5-minute interval.

# Replace each missing value with the mean value of its 5-minute interval

```r
replValue <- function(steps, interval) {
    compData <- NA
    if (!is.na(steps))
        compData <- c(steps)
    else
        compData <- (dailyAvg[dailyAvg$interval==interval, "steps"])
    return(compData)
}

compData <- actvyData
compData$steps <- mapply(replValue, compData$steps, compData$interval)
```
Histogram below shows the Total Number of steps taken each day


```r
totalSteps <- tapply(compData$steps, compData$date, FUN=sum)
hist(totalSteps, main="Histogram of Total Steps - No NA's", xlab="Total number of steps taken each day")
```

![](RR_Week2_Project1_files/figure-html/drawHist2-1.png)<!-- -->

```r
meanTotSteps <- mean(totalSteps)
medianTotSteps <- median(totalSteps)
```
The mean value for the # of total steps taken without NA's is 1.0766189\times 10^{4}

The median value for the # of total steps taken without NA's is 1.0766189\times 10^{4}


## Are there differences in activity patterns between weekdays and weekends?


```r
weekDayOrEnd <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}

compData$date <- as.Date(compData$date)
compData$day <- sapply(compData$date, FUN=weekDayOrEnd)
```


```r
dailyAvg <- aggregate(steps ~ interval + day, data=compData, mean)
ggplot(dailyAvg, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5 Minute Interval") + ylab("# of steps")
```

![](RR_Week2_Project1_files/figure-html/stepsforWeek-1.png)<!-- -->

