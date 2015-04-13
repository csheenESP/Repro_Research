---
title: "Reproducible Research: Peer Assessment 1"
author: "Carlos Guardia"
output: html_document
---

## Overview
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data
The data for this assignment can be downloaded from the course web site:

- Dataset: Activity monitoring data [52K]

The variables included in this dataset are:

- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- **date**: The date on which the measurement data was taken in YYYY-MM-DD format

- **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Assignment

### Loading and preprocessing the data

1. Load the data


```r
# Load the libraries
library(knitr)
library(lattice)
library(plyr)
# Set the working dir
setwd("c:/carlos/cursosjh/reprores/ejercicios/")
# Load the data
dat <- read.csv("activity.csv", header = TRUE, sep = ",")
# Check data structure
str(dat)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
## Convert the date to Date type
dat$date<-as.character(dat$date)
dat$date<-as.Date(dat$date)
# Factorize interval
dat$interval <- as.factor(dat$interval)
# Check data structure
str(dat)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

### What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
# Get the total number of steps ignoring the missing values in the dataset
tot_steps <- aggregate(dat$steps, by=list(dat$date), FUN=sum, na.rm=TRUE)
# Rename the columns 
colnames(tot_steps)<-c("date","steps")
# Print total number of steps for first days
head(tot_steps)
```

```
##         date steps
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

2. Make a histogram of the total number of steps taken each day


```r
# Make a histogram of the total number of steps taken each day
with(tot_steps, hist(steps,col="green",xlab="Total Steps in 1 day",breaks=22,xlim=c(0,25000)))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
# Mean of the total number of steps taken per day
mean_steps<-mean(tot_steps$steps)
print(mean_steps)
```

```
## [1] 9354.23
```

```r
# Median of the total number of steps taken per day
med_steps<-median(tot_steps$steps)
print(med_steps)
```

```
## [1] 10395
```

### What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# Mean of steps per interval
avg_steps <- aggregate(dat$steps, by=list(dat$interval), FUN=mean, na.rm=TRUE)
# Rename the columns 
colnames(avg_steps)<-c("interval","steps")
# Time series plot 
xyplot(steps ~ interval, avg_steps, type="l", lwd=2, xlab="Interval", 
       ylab="Steps", main="Average Daily Activity",col.line="green",
       scales=list(y=list(tick.number=10)))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?



```r
# Which 5-minute interval contains the maximum number of steps?
inte <-avg_steps[avg_steps$steps==max(avg_steps$steps),]
print(inte)
```

```
##     interval    steps
## 104      835 206.1698
```

The 5-minute interval, on average across all the days, is: 835

### Imputing missing values

1. Calculate and report the total number of missing values in the dataset 


```r
## Number of missing vals
tot_na<- sum(is.na(dat$steps))
```

The total number of missing values is: 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
## Fill missing with avg steps for that interval
datnew<-dat
for (i in 1:length(dat$steps)) {
    if (is.na(dat$steps[i])) {
        ##Search for the avg steps in that interval
        inter <- dat$interval[i]
        aver <-  avg_steps[avg_steps$interval==inter,]
        ##Assign it to na values
        datnew$steps[i]<- aver$steps
    }
}
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
## New dataset
head(datnew)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
## New dataset
# Get the total number of steps for the modified data
tot_stepsnew <- ddply(datnew,~date,summarise,total=sum(steps))
# Rename the columns 
colnames(tot_stepsnew)<-c("date","steps")
# Make histogram
with(tot_stepsnew, hist(steps,col="blue",xlab="Total Steps in 1 day",breaks=22,xlim=c(0,25000)))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
# New mean
mean_stepsnew <-mean(tot_stepsnew$steps)
print(mean_stepsnew)
```

```
## [1] 10766.19
```

```r
# New median
med_stepsnew<-median(tot_stepsnew$steps)
print(med_stepsnew)
```

```
## [1] 10766.19
```

We can see that both values the mean (1.0766189 &times; 10<sup>4</sup>) and the median (1.0766189 &times; 10<sup>4</sup>) are the same and bigger than the ones computed with NA data, as expected.


### Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
# Calculate type of day
datnew$day <- " " 
for (i in 1:length(datnew$date)) {
    wd <- weekdays(datnew$date[i])
    if (wd=="sábado" || wd=="domingo" ) datnew$day[i]<-"weekend"
    else datnew$day[i] <- "weekday"        
}
# Factorize the variable
datnew$day<-as.factor(datnew$day)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
# Compute the averages per type of day and interval
avg_stepsnew <- aggregate(datnew$steps, by=list(datnew$day, datnew$interval), mean)
# Rename the columns 
colnames(avg_stepsnew)<-c("day","interval", "steps")
# Show the data
head(avg_stepsnew)
```

```
##       day interval      steps
## 1 weekday        0 2.25115304
## 2 weekend        0 0.21462264
## 3 weekday        5 0.44528302
## 4 weekend        5 0.04245283
## 5 weekday       10 0.17316562
## 6 weekend       10 0.01650943
```

```r
# plot the results
xyplot(steps ~ interval | day, avg_stepsnew, type="l", lwd=2, xlab="Interval", 
       col.line="blue", ylab="Steps", main="Average Daily Activity", layout=c(1,2),
       scales=list(y=list(tick.number=10)))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

We can see in the charts that steps activity during the weekdays seem to be higher at lower time intervals, and a bit lower during the mid part of the days.
