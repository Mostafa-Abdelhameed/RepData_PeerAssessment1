---
title: "PA1_template.Rmd"
date: "2/22/2020"
output: html_document
---


```{r setup, include = FALSE, cache = FALSE}
knitr::opts_chunk$set(error = TRUE)
```


##  Reading the data set
```{r , echo=TRUE}
path<- "C:/Users/Mabdelhameed/Documents/repdata_data_activity/activity.csv"
Activity_Data <- read.csv(path)
```



## Mean of steps taken per day 

### Calculate the total number of steps taken per day 


```{r , echo=TRUE}
 Steps_Day <- aggregate(steps~date, Activity_Data, sum)
``` 

## Histogram of the total number of steps taken each day 

```{r , echo=TRUE}

hist(Steps_Day$steps, xlab="Total Number of Steps per day", ylab="Number of Days", main="Total Number of Steps taken each day")
``` 
# Mean and median number of steps taken each day

```{r , echo=TRUE}
  mean_steps<- mean (Steps_Day$steps)
  median_steps<-median(Steps_Day$steps)
```
# The average daily activity pattern

## time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r , echo=TRUE}
  Steps_Interval<-aggregate(steps~interval, Activity_Data, mean)
  with(Steps_Interval, plot(interval, steps, type = "l"))
```
## The 5-minute interval that, on average, contains the maximum number of steps

```{r , echo=TRUE}

  max_steps_5min <-Steps_Interval[which.max(Steps_Interval[,"steps"]),1]
```

## imputing missing data

# total number of missing values in the dataset

```{r , echo=TRUE}
  NA_Values <- is.na(Activity_Data[,1])
  sum_NA_Values <- sum(is.na(Activity_Data[,1]==TRUE))
```

# Finding the mean number of steps per Interval:
```{r , echo=TRUE}

 mean_intervals<-mean(Steps_Interval$steps)

```


# A new dataset that is equal to the original dataset but with the NAs values = mean of intervals.

```{r , echo=TRUE}
 Activity_Data_2<-Activity_Data
  Activity_Data_2[NA_Values ,1]<- mean_intervals
```
# the total number of steps each day after missing values are imputed
```{r }
  Steps_Day1<-aggregate(steps~date,  Activity_Data_2, sum)
  hist( Steps_Day1$steps, xlab="Class of Total Number of Steps per day", ylab="Number of Days", main="Number of Steps taken after missing values are imputed")
```

# Mean and Median after imputing NAs
```{r }
  mean_afterImput<-mean( Steps_Day1$steps)
  median_afterImput<-median(Steps_Day1$steps)
```

# Differences in activity patterns between weekdays and weekends


## Create variable with date in correct format
```{r , echo=TRUE}
Activity_Data_2$RealDate <- as.Date(Activity_Data_2$date, format = "%Y-%m-%d")
```
## create a variable with weekdays name
```{r , echo=TRUE}
Activity_Data_2$weekday <- weekdays(Activity_Data_2$RealDate)
```
## create a new variable indicating weekday or weekend
```{r , echo=TRUE}
 Activity_Data_2$DayType <- ifelse(Activity_Data_2$weekday=='Saturday' |Activity_Data_2$weekday=='Sunday', 'weekend','weekday')
  head(Activity_Data_2, n=10)
```


# create table with steps per time across weekdaydays or weekend days
```{r , echo=TRUE}
StepsPerTimeDT <- aggregate(steps~interval+DayType,data=Activity_Data_2,FUN=mean,na.action=na.omit)
```
# variable time (more comprensible for the graph axis)
```{r , echo=TRUE}
 StepsPerTimeDT$time <- Steps_Interval$interval/100
```

# draw the line plot

```{r , echo=TRUE}
library(lattice)
with( StepsPerTimeDT, 
      xyplot(steps ~ interval | DayType, 
      type = "l",      
      main = "Total Number of Steps within Intervals by dayType",
      xlab = "Daily Intervals",
      ylab = "Average Number of Steps"))
```

