#packages
library(ggplot2)
library(dplyr)

#load data

zip.url <- "https://github.com/coolhandluke999/RepData_PeerAssessment1/raw/master/activity.zip"

dir <- getwd()
zip.file <- "activity.zip"
zip.combine <- as.character(paste(dir, zip.file, sep = "/"))

download.file(zip.url, destfile = zip.combine)

unzip(zip.file)

data <- read.csv("activity.csv")

#clean data
#transform date from factor into date value

data$date <- as.Date(as.character(data$date))

#calculate total number of steps per day

sumSteps <- data %>% group_by(date) %>% summarize(totalSteps = sum(steps))


ggplot(data = sumSteps, mapping = aes(x = date, y = totalSteps)) +
        geom_histogram(stat = "identity", color = "white", fill = "steelblue") +
        labs(title = "Total Steps Per Day", x = "Date", y = "Total Steps")

#calculate and report the mean and median of the total number of steps taken per day

summary <- sumSteps %>% summarize(mean = mean(totalSteps, na.rm = TRUE),
                                  median = median(totalSteps, na.rm = TRUE))

#average daily activity pattern

avgStepsInt <- data %>% group_by(interval) %>% summarize(meanSteps = mean(steps, na.rm = TRUE))

ggplot(data = avgStepsInt, mapping = aes(x = interval, y = meanSteps)) + 
        geom_line()

#identify interval with highest steps

which.max(avgStepsInt$meanSteps)
avgStepsInt$interval[104]

#which 5 min interval, on average across all the days in the dataset, contains 
        #the maximum number of steps

#interval 835 with 206 steps on average per day

#Total number of missing values in the dataset

sum(is.na(data))

#Strategy for filling in missing values
        #for any missing value replace with the mean number of steps for that interval

#add interval step means column to every record in dataset
data$stepsAvg <- avgStepsInt$meanSteps

#assign mean interval value to any row with steps = NA
data$steps[which(is.na(data$steps))] = data$stepsAvg[which(is.na(data$steps))]

sum(is.na(data))
which(is.na(data$steps))

#create new data set equal to data minus the added interval mean column
newData <- data[,1:3]

stepsMeanDay <- newData %>% group_by(date) %>% summarize(meanValue = mean(steps),
                                                         medianValue = median(steps))


ggplot(data = newData, mapping = aes(x = date, y = steps)) +
        geom_histogram(stat = "identity", fill = "steelblue") +
        labs(title = "Total Steps Per Day", x = "Date", y = "Total Steps")

#steps increase dramatically when replacing NAs with interval means

#create factor variable for weekday and weekend types for each day

newData$dayOfWeek <- as.character(weekdays(newData$date))
weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

newData$dayType <- sapply(newData$dayOfWeek, FUN = function(x) ifelse((x %in% weekday), newData$dayType <- "Weekday", newData$dayType <- "Weekend"))

newData$dayType <- as.factor(newData$dayType)

stepsMeanDayType <- newData %>% group_by(dayType, interval) %>% summarize(meanValue = mean(steps))

ggplot(data = stepsMeanDayType, aes(x = interval, y = meanValue)) +
        geom_line(color = "steelblue", size = 1) +
        labs(title = "Average Number of Steps By Five Second Interval",
             y = "Average Number of Steps",
             x = "Five Second Interval") +
        facet_wrap(~ dayType, ncol = 1)

