# Load any libraries needed
library('ggplot2')
library('dplyr')

# Downlaod file into working directory
theURL <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(theUrl,destfile = './activity.csv',method='curl')

# Reading & processing the dataset
rawFile <- read.csv('./activity.csv',header=TRUE)

# Histogram of the total no. of steps taken each day
dailySteps <- aggregate(steps~date, rawFile, FUN = sum, na.action = NULL)
hist(dailySteps$steps, ylim = c(0,30),
     main = 'Total number of steps taken each day',
     xlab = 'Total number of steps taken',
     ylab = 'Number of days')

# Mean & median no. of steps taken each day
## Mean
meanSteps <- mean(dailySteps$steps, na.rm=TRUE)
print(paste0('The mean number of steps taken each day is: ', meanSteps))
## Median
medianSteps <- median(dailySteps$steps, na.rm=TRUE)
print(paste0('The median number of steps taken each day is: ', medianSteps))

# Time series plot of the average no. of steps taken
meanSteps <- aggregate(steps~date, rawFile, FUN = mean, na.action = NULL)
meanSteps[is.na(meanSteps)] <- 0
## Convert column 'date' to a date from char variable
meanSteps$date <- as.Date(meanSteps$date)
p <- ggplot(meanSteps, aes(x = date, y = steps)) + geom_line()
print(p)

# The 5-min interval that, on average, contains the max no. of steps
intervalSteps <- aggregate(steps~interval, rawFile, FUN = mean)
maxSteps <- intervalSteps[which.max(intervalSteps$steps),1]
print(paste0('The 5-min interval is ', maxSteps))

# Code to describe & show a strategy for imputing missing data
## Method chosen: Replacing NA values with intervals
imputeFile <- rawFile
imputeFile <- merge(imputeFile, intervalSteps, by = 'interval')
imputeFile$steps <- ifelse(is.na(imputeFile$steps.x),
                          imputeFile$steps.y, imputeFile$steps.x)
## Clean up data set and convert the format if needed
drops <- c('steps.x', 'steps.y')
imputeFile <- imputeFile[,!names(imputeFile) %in% drops]
imputeFile <- arrange(imputeFile, date)
### > str(imputeFile)
### 'data.frame':	17568 obs. of  3 variables:
### $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
### $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
### $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
imputeFile$date <- as.Date(imputeFile$date)

# Histogram of the total no. of steps taken each day after missing values are imputed
dailyStepsrev <- aggregate(steps~date, imputeFile, FUN = sum)
p <- ggplot(dailyStepsrev, aes(x = date, y = steps)) + geom_line()
print(p)

# Panel plot comparing the average no. of steps taken per 5-min interval across weekdays & weekends
## Add a column to the data to indicate weekdays & weekends
imputeFile$day <- ifelse(weekdays(imputeFile$date) == 'Saturday' |
                           weekdays(imputeFile$date) == 'Sunday',
                         'Weekday', 'Weekend')
## plot
p <- ggplot(imputeFile, aes(x=interval, y=steps, color=day)) +
  geom_line() +
  facet_wrap(~ day, ncol = 1, nrow = 2)
print(p)

# All of the R code needed to reproduce the results (numbers, plot, etc.) in the report
