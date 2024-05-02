---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
**1. Unzip the "activity.zip" archive**

```r
unzip(zipfile = "activity.zip", exdir = "activity")
```

**2. Read .csv file**

```r
data <- read.csv("activity/activity.csv")
```

**3. Pre-processing of the data**
The first step is to change the class from character to date for the values of the column date.

```r
library(dplyr)
data$date <- as.Date(data$date)
```

For the first question, the "NA" values are excluded. We create a new dataset excluding the NA values. This new dataset is called "data1"

```r
data1 <- na.exclude(data)
```

**4. Create a directory to store the figures**

```r
dir.create("figure")
```


## 1. What is mean total number of steps taken per day?
First to answer this question, we create a new dataset calculating the number of steps per day called "steps_day".

```r
steps_day <- data1 %>%
        group_by(date) %>%
        summarise(Total.Steps = sum(steps))
```

Next, we make a histogram of the total number of steps taken each day. This histogram displays the total number of steps per day on the x-axis and the y axis is the frequency of this total number of steps per day. It helps visualizing the mean and/or median. The number of bins was set to 22 for a better visualization of the data.

```r
plot1 <- hist(steps_day$Total.Steps, xlab = "Total number of steps per day", main="Distribution of the total number of steps per day", breaks = seq(0, max(steps_day$Total.Steps), length.out = 22), col="#1b98e0")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

We save the plot1 as a png file in the "figure" folder:

```r
dev.print(png, "figure/plot1.png", width = 480, height = 480, units = "px")
```

```
## quartz_off_screen 
##                 2
```

Finally, we can calculate the mean and median for the total number of steps taken per day and check if it matches the distribution observed in the histogram.

```r
mean.plot1 <- mean(steps_day$Total.Steps)
print(mean.plot1)
```

```
## [1] 10766.19
```

```r
median.plot1 <- median(steps_day$Total.Steps)
print(median.plot1)
```

```
## [1] 10765
```



## 2. What is the average daily activity pattern?
First, we can visualize the daily activity using a line plot. To do so, we have to calculate the average number of steps taken across all day - because several intervals have NA values, we have to remove the NAs from the dataset to calculate the average:

```r
data2 <- data %>%
        group_by(interval) %>%
        summarise(Avg.steps = mean(steps, na.rm = TRUE))
```

We use ggplot2 to plot the line graph:

```r
library(ggplot2)
plot2 <- ggplot(data2, aes(x=interval, y=Avg.steps)) 
plot2 +  geom_line() + labs(y = "average steps per day", title = "Average daily activity pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

We save the plot2 as a png file in the "figure" folder:

```r
dev.copy(png, file = "figure/plot2.png")
```

```
## quartz_off_screen 
##                 3
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```

To identify the 5-minute interval that contains the maximum number of steps on average across all the days in the dataset, we can:  
1. First calculate the maximum average number of steps  
2. Using the which function, we can identify the row matching the max value calculated  
3. Then, print the value corresponding to the interval column from the identified row:  

```r
max <- max(data2$Avg.steps)
max.interval.row <- which(data2$Avg.steps == max, arr.ind = TRUE)
max.interval <-as.numeric(data2[max.interval.row, 1])
print(max.interval)
```

```
## [1] 835
```


## 3. Imputing missing values

**1. Calculation of the missing values**  
First, we run the summary() function to identify wich column has NAs values. 

```r
summary(data)
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

Since only the "steps column has NAs values, we can calculate the total number of missing values in this column

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```


**2. Strategy for filling in all of the missing values in the dataset**  
We could fill the missing value NA at the indicated interval/day by the average value of the number of steps at this interval accross all day. This will use a similar table as previously generated

```r
data3 <- data %>%
        group_by(interval) %>%
        summarise(Avg.steps = mean(steps, na.rm = TRUE))
```

Then, we can add a new column to the original dataset "data" by merging the two datasets

```r
data3 <-merge(data, data3, by = c("interval"))
```

Next, we create a new dataset with the missing values in the "steps" column filled with the average steps calculated at the same interval

```r
library(dplyr)    
data3 <- data3 %>% 
        mutate(steps = ifelse(is.na(steps), Avg.steps,steps)) %>%
        select(steps, date, interval, -Avg.steps) 
data3$steps <- round(data3$steps, digits=2)
```


**3. Histogram of the total number of steps taken**  
First, we calculate the number of total steps per day, similar to the first part of the assignment:

```r
data3_steps_day <- data3 %>%
        group_by(date) %>%
        summarise(Total.Steps = sum(steps))
```

Next, we create a histogram plot, again, similar to the plot generated for the first part of the assignment:

```r
plot3 <- hist(data3_steps_day$Total.Steps, xlab = "Total number of steps per day", main="Distribution of the total number of steps per day", breaks = seq(0, max(data3_steps_day$Total.Steps), length.out = 22), col="#1b98e0")
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

We can save it into our "figure" folder:

```r
dev.copy(png, file = "figure/plot3.png")
```

```
## quartz_off_screen 
##                 3
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```

**4. Mean and median total number of steps taken per day**. 
We can calculate the mean and median values:

```r
mean.plot3 <- mean(data3_steps_day$Total.Steps)
print(mean.plot3)
```

```
## [1] 10766.18
```

```r
median.plot3 <- median(data3_steps_day$Total.Steps)
print(median.plot3)
```

```
## [1] 10766.13
```

To compare the mean and median value with or without imputing the missing value, we can generate a new table

```r
comparison.table <- data.frame(mean=c(mean.plot1, mean.plot3), median=c(median.plot1, median.plot3), row.names = c("no imputing NA", "with imputing NA"))
print(comparison.table)
```

```
##                      mean   median
## no imputing NA   10766.19 10765.00
## with imputing NA 10766.18 10766.13
```

We can see that imputing the missing data did not impact the estimates of the total daily number of steps.


## 4. Are there differences in activity patterns between weekdays and weekends?
We first create a new dataset called "data4" that include a column with the variables "weekend" or "weekday

```r
data4 <- data3 %>%
        mutate(days = weekdays(date))
library(dplyr)
data4$days <- case_when(
        data4$days == "Monday" ~ "weekday",
        data4$days == "Tuesday" ~ "weekday",
        data4$days == "Wednesday" ~ "weekday",
        data4$days == "Thursday" ~ "weekday",
        data4$days == "Friday" ~ "weekday",
        data4$days == "Saturday" ~ "weekdend",
        data4$days == "Sunday" ~ "weekdend"
)
```

Next, we calculate the mean of steps by interval across all weekdays or weekends. A new dataset is generated "data4_average"

```r
data4_average <- data4 %>%
        group_by(days, interval) %>%
        mutate(mean = mean(steps, na.rm = TRUE))
```

Finally, we build a new plot called "plot4" displaying the intervals on the x-axis and the mean steps by interval across weekday and weekend. Weekday and weekend data are separated on two graphs enabling us to compare the pattern of activity during the weekdays vs weekends.

```r
library(ggplot2)
plot4 <- ggplot(data4_average, aes(x=interval, y=mean)) 
plot4 +  geom_line(color = "#1b98e0") + labs(y = "average steps per day", title = "Average daily activity pattern") + facet_grid(vars(days))
```

![](PA1_template_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

```r
dev.copy(png, file = "figure/plot4.png")
```

```
## quartz_off_screen 
##                 3
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```

