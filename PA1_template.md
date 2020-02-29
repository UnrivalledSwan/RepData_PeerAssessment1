---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
### unzip the file

```r
unzip('activity.zip')
```

### Load the data (i.e. read.csv())

```r
activity <- read.csv("activity.csv",na.strings="NA")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

### Process/transform the data (if necessary) into a format suitable for your analysis

```r
total_steps <- with(activity, aggregate(x = steps, by = list(date), FUN = "sum", na.rm = TRUE))
names(total_steps) <- c("date","total_steps")
```

## What is mean total number of steps taken per day?
### Make a histogram of the total number of steps taken each day

```r
hist(total_steps$total_steps,
     breaks=8, 
     main="Histogram of Total Steps per Day",
     xlab = "Total Steps", 
     col="darkmagenta")
```

![](PA1_template_files/figure-html/hists-1.png)<!-- -->

```r
png("./figures/plot1.png", width=1000, height=600)
hist(total_steps$total_steps,
     breaks=8, 
     main="Histogram of Total Steps per Day",
     xlab = "Total Steps", 
     col="darkmagenta")
dev.off()
```

```
## png 
##   2
```

### Calculate and report the mean and median of the total number of steps taken per day

```r
paste("The mean is", round(mean(total_steps$total_steps),2), sep=" ")
```

```
## [1] "The mean is 9354.23"
```

```r
paste("The median is", round(median(total_steps$total_steps),2), sep=" ")
```

```
## [1] "The median is 10395"
```

## What is the average daily activity pattern?
### Process/transform the data (if necessary) into a format suitable for your analysis

```r
average_activity <- with(activity, aggregate(x = steps, by = list(interval), FUN = "mean", na.rm = TRUE))
names(average_activity) <- c("interval","average_steps")
```

### Make a time series plot (type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot(average_activity$interval, average_activity$average_steps, type = "l", xlab="interval", ylab="Average number of steps", main="Average number of steps per interval")
```

![](PA1_template_files/figure-html/stepsPlot-1.png)<!-- -->

```r
png("./figures/plot2.png", width=1000, height=600)
plot(average_activity$interval, average_activity$average_steps, type = "l", xlab="interval", ylab="Average number of steps", main="Average number of steps per interval")
dev.off()
```

```
## png 
##   2
```

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
Load the sqldf library to complete this step

```r
library(sqldf)
```

```
## Loading required package: gsubfn
```

```
## Loading required package: proto
```

```
## Loading required package: RSQLite
```

```r
sqldf("select interval from average_activity where average_steps = (select max(average_steps) from average_activity)")
```

```
##   interval
## 1      835
```

## Imputing missing values
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc
Add a new column to activity data frame that is the average for each time interval from the average_activity data


```r
activity$imputed_steps <- ifelse(is.na(activity$steps), round(average_activity$average_steps[match(activity$interval, average_activity$interval)],0), activity$steps)
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
new_activity <- data.frame(steps=activity$imputed_steps, date=activity$date, interval=activity$interval)
new_total_steps <- with(new_activity, aggregate(x = steps, by = list(date), FUN = "sum", na.rm = TRUE))
names(new_total_steps) <- c("date","total_steps")
```

### Make a histogram of the total number of steps taken each day

```r
hist(new_total_steps$total_steps,
     breaks=8, 
     main="Histogram of Total Steps per Day (with Imputed Data)",
     xlab = "Total Steps", 
     col="yellow")
```

![](PA1_template_files/figure-html/newhist-1.png)<!-- -->

```r
png("./figures/plot3.png", width=1000, height=600)
hist(new_total_steps$total_steps,
     breaks=8, 
     main="Histogram of Total Steps per Day (with Imputed Data)",
     xlab = "Total Steps", 
     col="yellow")
dev.off()
```

```
## png 
##   2
```

### Calculate and report the mean and median total number of steps taken per day

```r
paste("The New mean is", round(mean(new_total_steps$total_steps),2), sep=" ")
```

```
## [1] "The New mean is 10765.64"
```

```r
paste("The New median is", round(median(new_total_steps$total_steps),2), sep=" ")
```

```
## [1] "The New median is 10762"
```

```r
newMeanTotalSteps <- round(mean(new_total_steps$total_steps),2)
newMedTotalSteps <- round(median(new_total_steps$total_steps),2)

oldMeanTotalSteps <- round(mean(total_steps$total_steps),2)
oldMedTotalSteps <- round(median(total_steps$total_steps),2)

paste("The Old mean is", round(mean(total_steps$total_steps),2), sep=" ")
```

```
## [1] "The Old mean is 9354.23"
```

```r
paste("The Old median is", round(median(total_steps$total_steps),2), sep=" ")
```

```
## [1] "The Old median is 10395"
```

### Do the values differ from the estimates from the first part of the assignment?
When we compare the original values where the NA values were included and the new values where the NA values are replaced with average steps for the time interval, we can see that the orginal values and the new values differ.

### What is the impact of imputing missing data on the estimates of the total daily number of steps?
The impact of imputing the missing data on the estimates of the total daily number of steps is that the imputed values of the mean and median are greater than the non-imputed values.


## Are there differences in activity patterns between weekdays and weekends?
### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activity$date <- as.Date(activity$date, format="%Y-%m-%d")
activity$Day <- weekdays(activity$date, abbr = TRUE)
activity$dayType <- ""
Weekend <- c("Sat","Sun")
activity$dayType <- ifelse(activity$Day %in% Weekend,activity$dayType <- "Weekend",activity$dayType <- "Weekday")
```

### Make a panel plot containing a time series plot (type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
# Create a data frame for weekday activity
dayType_avg_activity <- with(activity, aggregate(x = steps, by = list(interval,dayType), FUN = "mean", na.rm = TRUE))
names(dayType_avg_activity) <- c("interval","dayType","average_steps")

# plot the time series for weekdays and weekends
library(lattice) 
xyplot(average_steps~interval|dayType,
		data=dayType_avg_activity, type="l",  
		layout = c(1,2),
		main="Average steps per 5-minute Interval: Weekdays vs. Weekends", 
		ylab="Average Steps", xlab="Interval")
```

![](PA1_template_files/figure-html/dtAverageActivity-1.png)<!-- -->

```r
png("./figures/plot4.png", width=1000, height=600)
xyplot(average_steps~interval|dayType,
		data=dayType_avg_activity, type="l",  
		layout = c(1,2),
		main="Average steps per 5-minute Interval: Weekdays vs. Weekends", 
		ylab="Average Steps", xlab="Interval")

dev.off()
```

```
## png 
##   2
```
