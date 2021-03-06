##Set working directory and unzip the data
setwd("~/R.Studio/Reproducible_research/RepData_PeerAssessment1")
unzip("activity.zip")
##read the data, with NAs
activity <- read.csv("activity.csv")


## Histogram of the total number of steps taken each day
dailysteps <- tapply(activity$steps, activity$date, sum)
hist(dailysteps, main = "Total number of steps taken each day")

## Mean and median number of steps taken each day
mean(dailysteps, na.rm = T)

median(dailysteps, na.rm = T)

##get the complete data without NAs, and get the average steps 
good <- complete.cases(activity)
gooddata <- activity[good, ]
meansteps <- tapply(gooddata$steps, gooddata$interval, mean)
interval <- as.numeric(names(meansteps))
plot(x = interval, y = meansteps, type = "l", xlab = "5-minute interval", 
     ylab = "Average number of steps taken")

##the maximum number of steps
which.max(meansteps)
meansteps[104]

##imputing missing data
bad <- !complete.cases(activity)
sum(bad)

##fill in the missing data with the mean value of its interval, create a new dataset
meandata <- as.numeric(meansteps)
filldata <- cbind(interval, meandata)
colnames(filldata) <- c("interval","steps")
activity1 <- activity
for (i in 1: 17568) {
        if (is.na(activity[i,1])){
                activity1[i,1] <- filldata[interval == activity1[i, 3], 2]
        }
}

##make a histogram of total number taken each day, calculate the mean and median
newdailystep <- tapply(activity1$steps, activity1$date, sum)
hist(newdailystep, breaks = 20, xlab = "Total number of steps taken daily", 
     ylab = "Counts")
mean(newdailystep)
median(newdailystep)

##create a new variable
date1 <- as.Date(activity1$date)
weekday <- weekdays(date1)
weekdays <- weekday
for (i in 1:17568) {
        if (weekdays[i] == "Saturday" | weekdays[i] == "Sunday") {
                weekdays[i] <- "weekend"
        } else {
                weekdays[i] <- "weekday"
        }
}
activity2 <- cbind(activity1, weekdays)

##Subset the weekday and weekend data, get the mean interval
weekenddata <- subset(activity2, weekdays == "weekend")
weekdaydata <- subset(activity2, weekdays == "weekday")
weekdaymean <- tapply(weekdaydata$steps, weekdaydata$interval, mean)
weekendmean <- tapply(weekenddata$steps, weekenddata$interval, mean)
par(mfrow = c(2,1), mar = c(4,4.5,2,2))
plot(interval, weekdaymean, type = "l", ylab = "Number of steps", 
     xlab = "", main = "Weekday")
plot(interval, weekendmean, type = "l", ylab = "Number of steps", 
     xlab = "Interval", main = "Weekend")


