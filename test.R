zipfile <- unzip("activity.zip")
adata <- read.csv("activity.csv", na.strings = "NA", header = T)

options(scipen=999)
 

meanact <- tapply( adata$steps, adata$interval, mean, na.rm = TRUE)
splitdata <- split(adata$steps, factor(adata$interval))

for (i in 1: length(splitdata)){
    splitdata[[i]][is.na(splitdata[[i]])] <- meanact[[i]]
}


meansteps <- unsplit(splitdata, adata$interval)
meandata <- adata
meandata$steps <- meansteps
## Day of the week
dow <- as.POSIXlt(as.Date(meandata$date))$wday
meandata$day <- dow
meandata$day <- as.numeric(meandata$day)
meandata$day[meandata$day == 1| meandata$day ==2 | meandata$day ==3 | meandata$day ==4 | meandata$day ==5] <- "Weekday"
meandata$day[meandata$day ==0 | meandata$day ==6] <- "Weekend"
meandata$day <- factor (meandata$day)


weekdata <- subset(meandata,meandata$day=="Weekday")

weekenddata <- subset(meandata,meandata$day=="Weekend")

meanweekact <- tapply( weekdata$steps, weekdata$interval, mean)
meanweekendact <- tapply( weekenddata$steps, weekenddata$interval, mean)

ilevels <- levels(factor(meandata$interval))
par(mfrow = c(2,1))
par(mar=c(1,1,1,1))
plot(ilevels, meanweekact, type = "l", xlab = "Intervals", ylab = "Average number of steps")
plot(ilevels, meanweekendact, type = "l", xlab = "Intervals", ylab = "Average number of steps")