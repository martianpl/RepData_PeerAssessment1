# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

library(knitr)
library(dplyr) 
library(lubridate)
library(ggplot2)
opts_chunk$set(echo = TRUE)

data <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric","character","integer"))

data$date <- ymd(data$date)

## What is mean total number of steps taken per day?

steps <- data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
  
ggplot(steps, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
  

mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)

## What is the average daily activity pattern?

interval <- data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
  
ggplot(interval, aes(x=interval, y=steps)) +
  geom_line(color = "firebrick")
  
interval[which.max(interval$steps),]

## Imputing missing values

sum(is.na(data$steps))

data_full <- data
nas <- is.na(data_full$steps)

avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)

data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]

sum(is.na(data_full$steps))

steps_full <- data_full %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
  
ggplot(steps_full, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")
  
mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
median_steps_full <- median(steps_full$steps, na.rm = TRUE)



## Are there differences in activity patterns between weekdays and weekends?

data_full <- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == "Saturday" | weekdays(data_full$date) == "Sunday", "weekend", "weekday"))
data_full$weektype <- as.factor(data_full$weektype)
head(data_full)

interval_full <- data_full %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))
s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)

From the two plots it seems that the test object is more active on weekend
