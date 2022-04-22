---
title: "Reproducible Research_Project1"
author: "eagle9711"
date: '2022-04-21'
output: html_document
---

### Loading and preprocessing the data

#### Show any code that is needed to

##### 1. Load the data 


```r
  activity<- read.csv("~/Documents/Data Science Specialization/Course5_Reproducible Research/C5-Project1/activity.csv")
```

##### 2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
  head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
  str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
  activity$date<- as.Date(activity$date, "%Y-%m-%d")
  
  str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
  summary(activity)
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
    
### What is mean total number of steps taken per day?

#### For this part of the assignment, you can ignore the missing values in the dataset.

##### 1. Calculate the total number of steps taken per day


```r
  stepsPerDay <- aggregate(steps~date, activity, sum, na.rm=TRUE)
```

##### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
     hist(stepsPerDay$steps, 30,   
          ylab = "Frequency",
          xlab = "Number of steps",
          main = "Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png)

##### 3. Calculate and report the mean and median of the total number of steps taken per day


```r
    mean(stepsPerDay$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
    median(stepsPerDay$steps, na.rm = TRUE)   
```

```
## [1] 10765
```

### What is the average daily activity pattern?

##### 1. Make a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
    stepsPerInterval <- aggregate(steps~interval, activity, mean, na.rm=TRUE)
    
    library(ggplot2)
    ggplot(stepsPerInterval, aes(interval, steps)) +
      geom_line() +
      labs(title = "Average number of steps per interval", x = "Interval", y = "Steps")
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26-1.png)

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
    stepsPerInterval[which.max(stepsPerInterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

### Imputing missing values

#### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

##### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
    sum(is.na(activity))
```

```
## [1] 2304
```

##### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
     
   The strategy is to fill the missing values of steps with the mean of the 5-minute intervals.

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
    library(dplyr)

    newactivity <- activity %>% 
                   group_by(interval) %>% 
                   mutate(steps= ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps)) 
```

##### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?    


```r
    n_stepsPerDay <- aggregate(steps~date, newactivity, sum)
    
    hist(n_stepsPerDay$steps, 30,   
         ylab = "Frequency",
         xlab = "Number of steps",
         main = "Total number of steps taken each day after imputing")
```

![plot of chunk unnamed-chunk-30](figure/unnamed-chunk-30-1.png)

```r
    mean(n_stepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
    median(n_stepsPerDay$steps)
```

```
## [1] 10766.19
```
The updated plot is very similar to the original plot. And the mean and median of the steps became exactly same after imputing missing data.

### Are there differences in activity patterns between weekdays and weekends?

#### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

##### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
    weekday <- ifelse(weekdays(newactivity$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

    newactivity$weekday<- as.factor(weekday)
    
    head(newactivity)
```

```
## # A tibble: 6 × 4
## # Groups:   interval [6]
##    steps date       interval weekday
##    <dbl> <date>        <int> <fct>  
## 1 1.72   2012-10-01        0 weekday
## 2 0.340  2012-10-01        5 weekday
## 3 0.132  2012-10-01       10 weekday
## 4 0.151  2012-10-01       15 weekday
## 5 0.0755 2012-10-01       20 weekday
## 6 2.09   2012-10-01       25 weekday
```

##### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval(x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.  


```r
    s_weekday <- aggregate(steps~interval + weekday, newactivity, mean)

    ggplot(s_weekday, aes(interval, steps)) +
      geom_line() + 
      facet_wrap(.~weekday, nrow = 2, ncol = 1) + 
      labs(title = "Average steps of the 5 mins interval across weekday or weekend", 
           x = "Interval", 
           y = "Steps")
```

![plot of chunk unnamed-chunk-32](figure/unnamed-chunk-32-1.png)
    
