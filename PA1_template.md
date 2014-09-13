# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv("~/Documents/reprod_res/RepData_PeerAssessment1/activity.csv", stringsAsFactors=FALSE)

        ##removing steps=0 while keeping steps=NA
        dt1<-activity[(activity$steps>0)|is.na(activity$steps),]
```

## What is mean total number of steps taken per day?

```r
total_steps<-aggregate(dt1$steps, by=list(dt1$date),sum)
hist(total_steps[,2], xlab="total steps", main="Histogram of steps")
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

```r
mean_steps<-aggregate(dt1$steps, by=list(dt1$date), mean)
median_steps<-aggregate(dt1$steps, by=list(dt1$date), median)
```

## What is the average daily activity pattern?

```r
mean_int<-aggregate(activity$steps, by=list(activity$interval), na.rm=T, mean)
plot(mean_int[,1],mean_int[,2],type="l", xlab="5 min interval", ylab="mean")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

```r
        ##Which interval contains the maxmum number of steps?
        mean_int[which.max(mean_int[,2]),][,1]
```

```
## [1] 835
```

## Imputing missing values

```r
dim(activity[is.na(activity[,1]),])[1] ##Number of NAs
```

```
## [1] 2304
```

```r
        ##Devise a strategy for filling in all of the NAs.
                ##fill with the mean for 5-minute interval
                activity2<-activity
                for(i in 1:length(activity2$steps)){
                        if(is.na(activity2$steps[i])){
                        activity2$steps[i]<-as.double(mean_int[mean_int[,1]==activity2$interval[i],][2])
                }
                }
        ##Make a histogram of the total number of steps.Calculate and report the mean and median.
        total_steps2<-aggregate(activity2$steps,by=list(activity2$date),sum)
        hist(total_steps2[,2], xlab="total # steps",main="Histogram of total steps (-NA)")
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

## Are there differences in activity patterns between weekdays and weekends?

```r
activity2$date<-as.POSIXct(activity2$date)
  weekDayEnd<-factor(weekdays(activity2$date))
  ## Replace "Fri", "Mon", "Sat", "Sun", "Thu", "Tue" and "Wed" with "weekday" or "weekend".
  levels(weekDayEnd)<-c("weekday","weekday","weekend","weekend","weekday","weekday","weekday")
  activity3<-cbind(activity2, weekDayEnd)
        ##Make a panel plot containing a time series plot (i.e. type = "l") 
        ## of the 5-minute interval (x-axis) and the average number of steps taken, 
        ## averaged across all weekday days or weekend days (y-axis).
  meanWeekDayEnd<-aggregate(activity3[,1],by=list(activity3$interval, activity3$weekDayEnd),mean)
        par(mfrow=c(1,2))
        plot(meanWeekDayEnd[,1][meanWeekDayEnd[,2]=="weekday"],meanWeekDayEnd[,3][meanWeekDayEnd[,2]=="weekday"],xlab="5 min interval",ylab="average of steps (weekday)",type="l")
        plot(meanWeekDayEnd[,1][meanWeekDayEnd[,2]=="weekend"],meanWeekDayEnd[,3][meanWeekDayEnd[,2]=="weekend"],xlab="5 min interval",ylab="average of steps (weekend)",type="l")
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-5.png) 

