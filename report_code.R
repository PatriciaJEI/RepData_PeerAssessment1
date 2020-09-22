### Loading and transforming the data

unzip("activity.zip")
activity <- read.csv("activity.csv")

activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

### We prepare a new dataset to store the total number of steps per day.

library(dplyr)

total.per.day <- activity %>%
        group_by(date) %>%
        summarise(total.steps=sum(steps,na.rm = T))

### And we produce the histogram and store a summary of the data.

hist(total.per.day$total.steps, breaks = 10, col = "springgreen4",
     xlab="Steps", main = "Total steps per day")

steps.summary <- summary(total.per.day$total.steps)

### Now we want to study the steps along the day, and calculate the mean, by
### time interval, across the registed days.

mean.per.interval <- activity %>%
        group_by(interval) %>%
        summarise(mean.steps=mean(steps,na.rm = T))

### We plot the average daily pattern

plot(x=mean.per.interval$interval,y=mean.per.interval$mean.steps, 
     xlab = "Time interval", ylab = "Steps", main = "Daily activity pattern",
     type = "l", lwd = 3, col = "darkseagreen")

### We look for the time interval with the highest number of steps registered

max.index <- which.max(mean.per.interval$mean.steps)
max.interval <- mean.per.interval$interval[max.index]

### We extract the number of NAs in the dataset

number.NA <- sum(is.na(activity$steps))
percentage.NA <- number.NA/nrow(activity)*100

### We prepare a new dataset to imput the NAs with the average number of steps

activity.filled <- activity

### With this for loop we look for NAs, and search for the corresponding mean
### value in the mean.per.interval dataset that stored the mean by time interval
for (i in 1:nrow(activity)){
        if(is.na(activity$steps[i])){
                int <- activity$interval[i]
                activity.filled$steps[i] <- mean.per.interval$mean.steps[which(mean.per.interval$interval==int)]
        }
}

### We repeat the first steps of the study with he new and completed dataset
### to produce the new histogram and mean and median values.

filled.total.day <- activity.filled %>%
        group_by(date) %>%
        summarise(total = sum(steps))

hist(filled.total.day$total, breaks = 10, col = "springgreen4",
     xlab="Steps", main = "Total steps per day", ylim = c(0,25))

filled.summary <- summary(filled.total.day$total)

### Now we prepare a new column to store the type of day (weekday or weekend).
### We create a new column filed with "weekday" to then substitute some of
### the entries (corresponding to saturday and sunday) with "weekend". Note that
### my dates are in spanish, change this bit of code if your dates are stored
### in some other language.

activity.filled$day <- rep("weekday", times=nrow(activity))
activity.filled$day[grep("sábado|domingo",weekdays(activity.filled$date))] <- "weekend"
activity.filled$day <- factor(activity.filled$day)

### Now we group the data by interval and day type to calculate the mean by
### these subsets.
total.weekdays <- activity.filled %>%
        group_by(interval,day) %>%
        summarise(total=mean(steps))

### Using the package lattice we produce an average day routine, by type of day.
library(lattice)

with(total.weekdays, xyplot(total ~ interval|day, type = "l", 
                            xlab = "Time interval", ylab = "Number of steps",
                            main = "Mean number of steps per time interval"))
