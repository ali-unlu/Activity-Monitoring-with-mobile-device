---
output: 
  html_document: 
    keep_md: yes
---
# Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.  

This exercise makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.  

__Dataset:__ Activity monitoring data that comes from the course repository

### The variables included in this dataset are:

__steps:__ Number of steps taking in a 5-minute interval (missing values are coded as NA)  
__date:__ The date on which the measurement was taken in YYYY-MM-DD format  
__interval:__ Identifier for the 5-minute interval in which measurement was taken  

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

# Research questions  

1. What is mean total number of steps taken per day?  
2. What is the average daily activity pattern?  
3. Are there differences in activity patterns between weekdays and weekends?  

# Data loading 




```r
# open zip file
file <- unzip("activity.zip", files = NULL, list = FALSE, overwrite = TRUE,
              junkpaths = FALSE, exdir = ".", unzip = "internal",
              setTimes = FALSE)
# load data
activity <- read.csv("activity.csv", sep = ",", header = TRUE)

# check the first rows
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



# What is mean total number of steps taken per day?

For this part of the exercise, we can ignore the missing values in the dataset.


```r
# Convert date variable from factor to date, and remove NAs
activity <- na.omit(activity)
activity$date <- as.Date(activity$date)
```

1- We are now calculating the total number of steps taken per day


```r
# dplyr library
library(dplyr)

# calculating steps per day
steps.day <- activity %>% 
        group_by(date) %>% 
        summarize(total.steps=sum(steps))

mean(steps.day$total.steps)
```

```
## [1] 10766.19
```

2- To compare the difference between days, we use histogram that shows the total number of steps taken each day


```r
hist(steps.day$total.steps, col="orange", 
     ylab="Count", 
     xlab="Total number of steps taken each day",
     main="Histogram of total number of steps taken each day" )
```

![](activity_monitoring_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3- In this step, we are calculating and report the mean and median of the total number of steps taken per day


```r
mean(steps.day$total.steps)
```

```
## [1] 10766.19
```

```r
median(steps.day$total.steps)
```

```
## [1] 10765
```


# What is the average daily activity pattern?  

1- We are now making a time series plot (i.e. type = "l" type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps.avarage <- activity %>% 
        group_by(interval) %>% 
        summarize(days.avarage=mean(steps))
    

plot(steps.avarage$interval, steps.avarage$days.avarage, col="orange",
     type="l",
     xlab="Interval",
     ylab="Average steps taken",
     main="Average steps taken during 5 minute interval")
```

![](activity_monitoring_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2- And now we are looking for which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max.step <- steps.avarage$interval[which.max(steps.avarage$days.avarage)]
max.step
```

```
## [1] 835
```

# Imputing missing values  

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data. For that reason, we impute missing data. 


```r
# recalling original data
activity2 <- read.csv("activity.csv", sep = ",", header = TRUE)
```

1- We are not calculating and reporting the total number of missing values in the dataset (i.e. the total number of rows with NAs). 
2 - We use a simple strategy for filling in all of the missing values in the dataset. For example, we could use the mean/median for that day, or the mean for that 5-minute interval, etc. 


```r
# missing calculation
mean(is.na(activity2))  # proportion
```

```
## [1] 0.04371585
```

```r
table(is.na(activity2)) # numbers
```

```
## 
## FALSE  TRUE 
## 50400  2304
```

3- We are now creating a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# creating new data
activity3 <- activity2

# NA replacement with column mean
activity3$steps[is.na(activity3$steps)] <- mean(activity3$steps, na.rm = TRUE)

# check the results
table(is.na(activity3))
```

```
## 
## FALSE 
## 52704
```


4- We plot a histogram for the total number of steps taken each day. To do so, we calculate and report the mean and median total number of steps taken per day. The main points here that we want to explore are: 

-Do these values differ from the estimates from the first part of the assignment? 
-What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# calculating steps per day with the missing replacement
steps.day3 <- activity3 %>% 
        group_by(date) %>% 
        summarize(total.steps=sum(steps))

mean(steps.day3$total.steps)
```

```
## [1] 10766.19
```

```r
median(steps.day3$total.steps)
```

```
## [1] 10766.19
```

```r
hist(steps.day3$total.steps, col="red", 
     ylab="Count", 
     xlab="Total number of steps taken each day",
     main="Histogram of total number of steps taken each day" )
```

![](activity_monitoring_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Our results show that the mean score does not change but the median score slightly increased due to missing replacement. 

# Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. we use the dataset with the filled-in missing values for this part.

1- We create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
# create a new data for computation
activity4 <- activity3

# assing days for dates
activity4$days <- weekdays(as.Date(activity4$date))

# create a new variable and fill it withh weekdays
activity4$weekfactor <- " weekdays"

# convert fill weekdays with weekend
activity4$weekfactor[activity4$days %in% c("Saturday", "Sunday")] <- "weekend"
```


2- We are making a panel plot containing a time series plot (i.e. type = "l" type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
# Group data by 5 minute interval and 
# summarize the average number of steps in that interval
dayaverage <- activity4 %>%
        group_by(weekfactor, interval) %>%
        summarize(AverageSteps=mean(steps))
```

```
## `summarise()` has grouped output by 'weekfactor'. You can override using the `.groups` argument.
```

```r
# plot
library(ggplot2)
ggplot(dayaverage , aes(x = interval , y = AverageSteps, color=`weekfactor`)) +
    geom_line() +
    labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + 
    facet_wrap(~`weekfactor` , ncol = 1, nrow=2)
```

![](activity_monitoring_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

