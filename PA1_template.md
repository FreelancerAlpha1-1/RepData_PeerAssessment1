# Reproducible Research: Peer Assessment 1


```r
library(ggplot2)
library(data.table)
library(plyr)
```


## Loading and preprocessing the data

> Load the data (i.e. read.csv())


```r
setwd('~/Workspace/coursera/reproducible_research/RepData_PeerAssessment1/')
unzip('./activity.zip')
dt <- fread('activity.csv')
void <- file.remove('activity.csv'); # We don't need unzipped file anymore
str(dt)
```

```
## Classes 'data.table' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```


## What is mean total number of steps taken per day?

> Make a histogram of the total number of steps taken each day


```r
total.steps.by.day <- dt[, list(steps = sum(steps, na.rm = TRUE)), by = list(date)]
ggplot(total.steps.by.day, aes(x = steps)) + geom_histogram(binwidth = 1000)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


> Calculate and report the mean and median total number of steps taken per day

```r
paste("Mean:", mean(total.steps.by.day$steps, na.rm = TRUE))
```

```
## [1] "Mean: 9354.22950819672"
```

```r
paste("Median:", median(total.steps.by.day$steps, na.rm = TRUE))
```

```
## [1] "Median: 10395"
```


## What is the average daily activity pattern?

> Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
average.steps.by.interval <- dt[!is.na(steps), list(steps = mean(steps)), by = list(interval)]
ggplot(average.steps.by.interval, aes(x = interval, y = steps)) + geom_line()
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


> Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
paste("Interval which contain on average maximum number of steps:", average.steps.by.interval[which.max(average.steps.by.interval$steps), 
    ]$interval)
```

```
## [1] "Interval which contain on average maximum number of steps: 835"
```


## Imputing missing values

> Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
paste("Total number of missing values:", sum(!complete.cases(dt)))
```

```
## [1] "Total number of missing values: 2304"
```


> Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

NA values can be replaced by median values for given interval acros all days.

> Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r

dt.filled  <- arrange(merge(
    dt,
    dt[, list(steps.median=median(steps, na.rm=TRUE)), by=list(interval)], # Compute median number of steps by interval
    by=c('interval')
    )[, list(interval=interval, date=date, steps=ifelse(!is.na(steps), steps, steps.median))], # Replace NA with computed median
    date, interval) # Sort by date and interval
```


> Make a histogram of the total number of steps taken each day.


```r

total.steps.by.day.filled <- dt.filled[, list(steps = sum(steps)), by = list(date)]
ggplot(total.steps.by.day.filled, aes(x = steps)) + geom_histogram(binwidth = 1000)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

> Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
paste("Mean after filling missing values:", mean(total.steps.by.day.filled$steps))
```

```
## [1] "Mean after filling missing values: 9503.86885245902"
```

```r
paste("Median after filling missing values:", median(total.steps.by.day.filled$steps))
```

```
## [1] "Median after filling missing values: 10395"
```


Mean number of steps is higher than before but median stays the same.

## Are there differences in activity patterns between weekdays and weekends?

> Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
dt.filled$type.of.day <- factor(ifelse(weekdays(strptime(dt.filled$date, "%Y-%m-%d")) %in% 
    c("Saturday", "Sunday"), "weekend", "weekday"), levels = c("weekend", "weekday"))
```

> Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r

average.steps.by.interval.and.type.of.day <- dt.filled[, list(steps.average = mean(steps), 
    type.of.day = type.of.day, interval = interval), by = list(interval, type.of.day)]

ggplot(average.steps.by.interval.and.type.of.day, aes(x = interval, y = steps.average)) + 
    geom_line() + facet_grid(type.of.day ~ .)
```
