---
title: "<center>Reproducible Research: Peer Assignment 1</center>"
output: html_document
---

### Data
The data for this assignment [activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip), and the variables included in the dataset are:

 * steps: Number of steps taking in a 5-minute interval (missing values are coded as _NA_)  
 * date: The date on which the measurement was taken in YYYY-MM-DD format  
 * interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### Aims
In this assignment, I am going to answer the four main parts of questions:  

* What is mean total number of steps taken per day?
* What is the average daily activity pattern?
* Imputing missing values
* Are there difference in activity patterns between weekdays and weekdays?

### Preparation: Load the data, packages, and several exploratory
```{r}
library(ggplot2)
library(lattice)
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
data_clean <- na.omit(data)
## rownames(data_clean) <- 1:nrow(data_clean) ## or it will begin with 289
summary(data_clean)
```

### Section 1: What is mean total number of steps taken per day?
#### 1. Calculate the total number of steps taken per day:
```{r}
steps_data_total <- aggregate(steps ~ date, data = data_clean, FUN = sum)
```
  
#### 2. Make a histogram of the total number of steps taken each day:
```{r}
g <- ggplot(data_clean, aes(date, steps))
g <- g + geom_bar(stat = "identity", colour = "orange", fill = "orange", width = 0.8)
g <- g + labs(title = "Figure 1: Histogram of Total Steps per Day", x = "", y = "Total steps")
g <- g + geom_line(data = steps_data_total, aes(x=date, y=steps), size = 0.8, col = "brown")
g
```

#### 3. Calculate and report the mean and median of the total number of steps: taken per day
```{r}
mean(steps_data_total$steps)
median(steps_data_total$steps)
```

### Section 2: What is the average daily activity pattern?
#### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):
```{r}
steps_data_mean <- aggregate(x = list(steps = data_clean$steps), by = list(interval = as.numeric(as.character(data_clean$interval))), FUN = mean)

g <- ggplot(data = steps_data_mean, aes(x = interval, y = steps))
g <- g + geom_line(color = "brown", size = 0.8)
g <- g + xlab("5-minute interval")
g <- g + ylab("Average Steps")
g <- g + ggtitle("Figure 2: Time Series Plot of the 5-minute Interval")
g
```
  
#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
steps_data_mean[which.max(steps_data_mean$steps), ]
```

### Section 3: Imputing missing values
#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs):
```{r}
sum(is.na(data)) ## or directly 17568 - 15264 = 2304
```
#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
In the following part, I will use the mean for that 5-minute interval to fill in all of the missing values in the dataset.  
  
#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
data_clean_2 <- data
for (i in 1:nrow(data_clean_2)) {
    if (is.na(data_clean_2$steps[i])) {
        data_clean_2$steps[i] <- steps_data_mean[which(data_clean_2$interval[i] == steps_data_mean$interval), ]$steps
    }
}
## Several exploratory of the new dataset
head(data_clean_2)
```

#### 4-1. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.  
```{r}
steps_data_total_2 <- aggregate(steps ~ date, data = data_clean_2, FUN = sum)

g <- ggplot(data_clean_2, aes(date, steps))
g <- g + geom_bar(stat = "identity", colour = "orange", fill = "orange", width = 0.8)
g <- g + labs(title = "Figure 3: Histogram of Total Steps per Day (without missing values)", x = "", y = "Total steps")
g <- g + geom_line(data = steps_data_total_2, aes(x=date, y=steps), size = 0.8, col = "brown")
g
```
  
#### 4-2. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
mean(steps_data_total_2$steps)
median(steps_data_total_2$steps)
```
So, we can see the mean doesn't change, but the median is change to the same value as mean.

### Section 4: Are there differences in activity patterns between weekdays and weekends?
#### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r}
data_clean_2$days_of_week <- factor(format(data_clean_2$date, "%A"))
levels(data_clean_2$days_of_week) <- list( weekday = c("Monday", "Tuesday","Wednesday", "Thursday", "Friday"), weekend = c("Saturday", "Sunday"))
table(data_clean_2$days_of_week)
```
#### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r}
steps_data_mean_2 <- aggregate(x = list(steps = data_clean_2$steps), by = list(interval = as.numeric(as.character(data_clean_2$interval)), weekday = data_clean_2$days_of_week), FUN = mean)

xyplot(steps_data_mean_2$steps ~ steps_data_mean_2$interval | steps_data_mean_2$weekday, layout = c(1, 2), type = "l", xlab = "5-minute interval", ylab = "Average Steps", main = "Figure 4: Time Series Plot of the 5-minute Interval (seperated by weekday)")
```