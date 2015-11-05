# Reproducible Research: Peer Assessment 1


First of all we need to load and preprocess the data.  The date is stored as a factor so needs to be changed to a date


```r
setwd("~/GitHub/RepData_PeerAssessment1")
if(!file.exists("./activity/activity.csv")){
       unzip("activity.zip")} 
#load first file into a variable if not already present
if(!"activity" %in% ls()){
    activity <- read.csv("./activity/activity.csv")}

activity$date <- as.Date( activity$date, format = '%Y-%m-%d' )
```

# What is mean total number of steps taken per day?
A histogram of the number the steps per day shows the variation in activity

```r
totsteps<-aggregate(activity$steps,list(activity$date),FUN="sum",rm.na=T)
hist(totsteps$x, xlab="Number of Steps", main="Histogram of Daily Steps", ylab="Number of Days",ylim=c(0,30))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 




The average number of steps are:

```r
summary(totsteps$x)['Median']
```

```
## Median 
##  10770
```

```r
summary(totsteps$x)['Mean']
```

```
##  Mean 
## 10770
```
## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
