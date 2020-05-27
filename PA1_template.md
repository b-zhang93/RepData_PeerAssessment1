---
title: "Reproducible Research: Course Project 1"
output: 
  html_document: 
    keep_md: true
---


## Loading and preprocessing the data

Unzip and load the data:

```r
unzip("activity.zip")
csvdata <- read.csv("activity.csv")
str(csvdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Convert date field from characters to date class:

```r
csvdata[,"date"] <- as.Date(csvdata[,"date"])
class(csvdata$date)
```

```
## [1] "Date"
```

## What is mean total number of steps taken per day?

Calculate the Total Steps per day

```r
daily_steps <- tapply(csvdata$steps, csvdata$date, sum)
```

Histogram for total steps by day

```r
hist(daily_steps, xlab = "Daily Steps Taken", main = "Histogram of Daily Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Calculate mean and median steps per day

```r
mean(daily_steps, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(daily_steps, na.rm = T)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

Create a time series plot with 5 minute interval on the x-axis and  average number of steps taken, averaged across all days (y-axis):

```r
interval_steps <- tapply(csvdata$steps, csvdata$interval, mean, na.rm = T)
interval <- as.numeric(names(interval_steps))

plot(interval, interval_steps, 
     type = "l",
     col = "blue",
     lwd = 1.9,
     xlab = "5 Minute Interval", 
     ylab = "Steps", 
     main = "Average Daily Activity")
```

![](PA1_template_files/figure-html/timeplot-1.png)<!-- -->

Which 5 minute interval contains the maximum number of steps? 

```r
maxinterval <- subset(interval_steps, interval_steps == max(interval_steps))
paste("The interval with max number of steps is:", names(maxinterval))
```

```
## [1] "The interval with max number of steps is: 835"
```


## Imputing missing values

Check how many NAs there are per column with summary

```r
summary(csvdata)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
na_total <- sum(is.na(csvdata$steps))
```
*Steps* is the only variable to have NAs. **There are 2304 NA values**


**Impute strategy:** We will impute the NAs with the mean of total steps for that day

```r
# convert our previous calculation of interval step averages into a data frame
interval_df <- data.frame(interval_steps)
interval_df$interval <- rownames(interval_df)
names(interval_df) <- c("avg_steps", "interval")

# merge the data into one dataset
activity <- merge(csvdata, interval_df, by = "interval", all = T)

# finds the indices of all nas
nas <- which(is.na(activity$steps))

# loop through all the NA values and replace it with the corresponding average value by date
for(i in nas){
        activity$steps[[i]] <- activity$avg_steps[[i]]
        
}

# remove the extra column 
activity <- subset(activity, select = -avg_steps)
```

Replot histogram for total steps by day with imputed data

```r
d_steps <- tapply(activity$steps, activity$date, sum)
hist(d_steps, xlab = "Daily Steps Taken", main = "Histogram of Daily Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Calculate mean and median steps per day

```r
mean(d_steps)
```

```
## [1] 10766.19
```

```r
median(d_steps)
```

```
## [1] 10766.19
```

**Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

The imputed data seem to have made the mean and median closer together, making the distribution more uniform. The values however have not changed much, and neither has the shape of the histogram. The frequency has risen as expected however. 


## Are there differences in activity patterns between weekdays and weekends?

Create new factor variable indicating if the date is a weekday or weekend

```r
activity$day <- weekdays(activity$date)
activity$day <- factor(activity$day,
                       levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                       labels = c(rep("weekday",5),rep("weekend",2)))

head(activity)
```

```
##   interval    steps       date     day
## 1        0 1.716981 2012-10-01 weekday
## 2        0 0.000000 2012-11-23 weekday
## 3        0 0.000000 2012-10-28 weekend
## 4        0 0.000000 2012-11-06 weekday
## 5        0 0.000000 2012-11-24 weekend
## 6        0 0.000000 2012-11-15 weekday
```


Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
library(lattice)

# group the data by interval and day and average the steps taken
activity2 <- aggregate(steps~interval + day, activity, mean)

# plot the graph using lattice plot
xyplot(steps ~ interval | day, data=activity2, 
       type = 'l',
       main="Average Steps Taken across 5-minute Intervals",
       xlab="5-Minute Interval",
       ylab="Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/panel-1.png)<!-- -->

**Are there differences in patterns?** There seems to more overall average activity during the weekend than weekdays, however weekdays have a higher maximum peak movement during one of its days
