# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
data <- read.csv(unz("activity.zip", "activity.csv"))
data <- transform(data, date = as.Date(date, "%Y-%m-%d"))
```

## What is mean total number of steps taken per day?

```r
good <- data[complete.cases(data), ]
subdate <- split(good, good$date)
total_step_per_day <- sapply(subdate, function(x){
        sum(x$steps)
})
hist(total_step_per_day, main="Histogram of total number of steps taken each day", xlab="steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean_step <- mean(total_step_per_day)
median_step <- median(total_step_per_day)
```

## What is the average daily activity pattern?

```r
subinterval <- split(good, good$interval)
ave_step_allday <- sapply(subinterval, function(x){
        mean(x$steps)
})
plot(unique(good$interval), ave_step_allday, type="l", xlab="5-minute interval", ylab="average number of steps", main = "Average daily activity pattern")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
ave_step_allday[max(ave_step_allday)]
```

```
## 1705 
## 56.3
```


## Imputing missing values

```r
total_NA <- nrow(data) - nrow(good)
new_data <- data.frame()
for (i in 1:nrow(data)){
        temp_row <- data[i, ]
        if(is.na(temp_row$steps)){
                temp_interval <- temp_row$interval/5 + 1
               temp_step <- ave_step_allday[temp_interval]
               temp_row$steps <- temp_step
        }
        new_data <- rbind(new_data, temp_row)     
}

new_subdate <- split(new_data, new_data$date)
new_total_step_per_day <- sapply(new_subdate, function(x){
        sum(x$steps)
})
hist(new_total_step_per_day, main="Histogram of new total number of steps taken each day", xlab="steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
new_mean_step <- mean(new_total_step_per_day)
new_median_step <- median(new_total_step_per_day)
```

## Are there differences in activity patterns between weekdays and weekends?

```r
new_data$weektime <- as.factor(ifelse(weekdays(data$date) %in% c("Saturday","Sunday"),"weekend", "weekday"))
par(mfrow=c(2,1))

data_weekday <- new_data[new_data$weektime=="weekday",]
subweekday <- split(data_weekday, data_weekday$interval)
weekday_step <- sapply(subweekday, function(x){
        mean(x$steps)
})

data_weekdend <- new_data[new_data$weektime=="weekdend",]
subweekend <- split(data_weekdend, data_weekdend$interval)
weekend_step <- sapply(subweekend, function(x){
        mean(x$steps)
})

plot(unique(data_weekday$interval), weekday_step)
plot(unique(data_weekdend$interval), weekend_step)
```

```
## Warning: no non-missing arguments to min; returning Inf
## Warning: no non-missing arguments to max; returning -Inf
## Warning: no non-missing arguments to min; returning Inf
## Warning: no non-missing arguments to max; returning -Inf
```

```
## Error: need finite 'xlim' values
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 