# Reproducible Research: Peer Assessment 1
Ptitmatheux  
September 13, 2017  



## Loading and preprocessing the data

Loading data, removing missing values and converting the dataframe into a tibble
for easy further manipulation:


```r
library(dplyr)
library(ggplot2)
data <- read.csv(file="activity.csv")
data_tbl <- as.tbl(data) %>% filter(complete.cases(.))
```
   
## What is mean total number of steps taken per day?
   

```r
# summing up the number of steps in a day, for each day:
data_day <- group_by(data_tbl, date) %>%
    mutate(dailysteps=sum(steps)) %>%
    select(date, dailysteps) %>%
    distinct()

# plotting the histogram:
hist(data_day$dailysteps,
     main="histogram of daily number of steps",
     xlab="daily number of steps",
     breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
dailysteps_mean <- round(mean(data_day$dailysteps))
dailysteps_median <- round(median(data_day$dailysteps))
```

The mean daily number of steps is 1.0766\times 10^{4} whereas the median is
1.0765\times 10^{4}.

## What is the average daily activity pattern?


```r
# averaging all the time-slots accross all days:
data_ts <- group_by(data_tbl, interval) %>%
    mutate(slotAverage=mean(steps)) %>%
    select(interval, slotAverage) %>%
    distinct()

# plotting the time-series:
plot(x=data_ts$interval, y=data_ts$slotAverage, type="l", xlab = "time slot", ylab="number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# getting the time slot with highest number of steps
peak_slot <- data_ts$interval[which.max(data_ts$slotAverage)]
```

The time slot with, on average, the highest number of steps is 835, corresponding thus to 08:35 am.

## Imputing missing values

One possibility for imputing missing values is to consider, for each time-slot, the 
mean number of steps for that time-slots, over all days. Each missing value in a time-slot
is then attributed that mean value.


```r
# Imputing missing values from the mean of the corresponding time-slot over all days:
data_tbl <- as.tbl(data)

data_filled <- group_by(data_tbl, interval) %>%
    mutate(slotMean=mean(steps, na.rm=TRUE)) %>%
    mutate(steps=ifelse(is.na(steps), slotMean, steps))
    
data_filled_day <- group_by(data_filled, date) %>%
    mutate(dailysteps=sum(steps)) %>%
    select(date, dailysteps) %>%
    distinct()
    
hist(data_filled_day$dailysteps,
     main="histogram of daily number of steps (NAs imputed)",
     xlab="daily number of steps",
     breaks=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
dailysteps_filled_mean <- round(mean(data_filled_day$dailysteps))
dailysteps_filled_median <- round(median(data_filled_day$dailysteps))
```

The mean daily number of steps with imputed missing values is 1.0766\times 10^{4} whereas the median is
1.0766\times 10^{4}.
If we compare with the mean and median values obtained from the dataset with 
missing values just removed, we see that the values are almost unchanged. Thus,
the imputation of missing values does not have significant impact.

## Are there differences in activity patterns between weekdays and weekends?


```r
# Adding a new factor variable discriminating weekends from weekdays:
data_filled <- ungroup(data_filled) %>%
    mutate(weekend=ifelse(as.character(weekdays(as.Date(data_filled$date))) %in% c("Saturday", "Sunday"), "weekend", "weekday")) %>%
    mutate(weekend=as.factor(weekend))

# Computing the average time-series for weekdays and weekends:
data_filled_ts <- group_by(data_filled, interval, weekend) %>%
    mutate(slotMean=mean(steps, na.rm=TRUE)) %>%
    select(interval, slotMean, weekend) %>%
    distinct()

#plotting
p <- ggplot(data_filled_ts, aes(x=interval, y=slotMean))+geom_line()+facet_grid(~weekend)
p
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

As we can see, there is a visible difference in the pattern especially for the time-slots corresponding to the morning; basically during the week-end, physical activity starts later. 



