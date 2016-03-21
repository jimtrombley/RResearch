# RResearch Project1
JT  
March 13, 2016  

# Reproducible Research: Peer Assessment 1

Setup knitr options

```r
library(knitr)
opts_chunk$set(message = FALSE, warning=FALSE, fig.width = 6, fig.height = 6)
```
Load libraries and 'activity'data file

```r
library(ggplot2)
library(plyr)
activity <- read.csv("source data/activity.csv", colClasses = c("numeric", "Date", "numeric"))
```
### Histogram of the total number of steps taken each day

```r
dailysteps <- aggregate(steps ~ date, activity, sum, na.action = na.omit)
par(mar=c(5,4,1,1), las=1)
ggplot(dailysteps, aes(x = steps)) + geom_histogram(binwidth = 2500, colour = "black",
fill = "blue") + labs(title = "Steps Taken per Day", x = "Number of Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Mean and median number of steps taken each day

```r
x <- mean(dailysteps$steps)
m <- median(dailysteps$steps)
```

* The mean of total number steps per day is 10766  
* The median of total number steps per day is 10765

### Time series plot of the average number of steps taken

```r
activity_interval <- aggregate(steps ~ interval, activity, mean, na.action = na.omit)
ggplot(activity_interval, aes(x = interval, y = steps)) + geom_line(color = "blue") + labs(title = "Average of Steps taken Daily", x = "Interval", y = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### The 5-minute interval that, on average, contains the maximum number of steps

```r
Interval_steps <- aggregate(steps ~ interval, activity, sum, na.action = na.omit)
Max<-subset(Interval_steps,steps == max(Interval_steps$steps))
max <- Max[1,1]
```
The 5 minute interval containing the maximum number of steps is 835

### Code to describe and show a strategy for imputing missing data

```r
steps.impute <- adply(activity, 1, function(x) if (is.na(x$steps))
{x$steps = round(activity_interval[activity_interval$interval == x$interval, 2])
x}
else {x})
```
### Histogram of the total number of steps taken each day after missing values are imputed

```r
byDay.impute <- aggregate(steps ~ date, steps.impute, sum)
byDay.impute <- cbind(byDay.impute, label = rep("without.na", nrow(byDay.impute)))
aggsteps <- aggregate(steps ~ date, activity, sum, na.action = na.pass)
aggsteps <- cbind(aggsteps, label = rep("with.na", nrow(aggsteps)))
byDay.all <- rbind(aggsteps, byDay.impute)
levels(byDay.all$label) <- c("With NA", "Without NA")
ggplot(byDay.impute, aes(x = steps)) + geom_histogram(binwidth = 2500, colour = "black",
fill = "blue") + labs(title = "Steps Taken per Day", x = "Number of Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
ggplot(byDay.all, aes(x = steps, fill = label)) + geom_histogram(binwidth = 2500, colour = "black", alpha = 1) + labs(title = "Steps Taken per Day", x = "Number of Steps", y = "Frequency") + theme(legend.position = "bottom")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
data.weekend <- subset(steps.impute, weekdays(date) %in% c("Saturday", "Sunday"))
data.weekday <- subset(steps.impute, !weekdays(date) %in% c("Saturday", "Sunday"))
data.weekend <- aggregate(steps ~ interval, data.weekend, mean)
data.weekday <- aggregate(steps ~ interval, data.weekday, mean)
data.weekend <- cbind(data.weekend, day = rep("weekend"))
data.weekday <- cbind(data.weekday, day = rep("weekday"))
data.week <- rbind(data.weekend, data.weekday)
levels(data.week$day) <- c("Weekend", "Weekday")
ggplot(data.week, aes(x = interval, y = steps)) + geom_line(color="red") + facet_grid(day ~ .) + labs(x = "Interval", y = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
