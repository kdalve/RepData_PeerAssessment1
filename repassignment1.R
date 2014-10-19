
install.packages("knitr")
library(knitr)


Download & Unzip the Activity file. Set your working directory.
list.files()
activityData<-read.table("activity.csv", sep=",", header=TRUE)
head(activityData)
tail(activityData)

steps<-activityData[ ,1]
date<-activityData[ ,2]
interval<-activityData[ ,3]

perday<-tapply(steps, INDEX=date, FUN=sum)
mean(perday, na.rm=TRUE)
median(perday, na.rm=TRUE)
hist(perday, xlab= "Steps per Day", col="blue", main="Total Number of Steps Take Each Day")

perinterval<-tapply(steps, INDEX=interval, FUN=mean, na.rm=TRUE)
plot(perinterval,type='l', xlab="Time Interval", ylab="Average Steps per Interval", main="Average Daily Activity Pattern")
summary(perinterval)
perinterval[which.max(perinterval)]


sum(is.na(steps))
sum(is.na(activityData))

perinterval <- aggregate(steps ~ interval, data = activityData, FUN = mean)
activity2 <- merge(activityData, perinterval, by = "interval")
head(activity2)

activity2$steps.x[is.na(activity2$steps.x)] <- activity2$steps.y[is.na(activity2$steps.x)]
head(activity2)

steps2<-activity2[ ,2]
date2<-activity2[ ,3]
interval2<-activity2[ ,1]

perday2<-tapply(steps2, INDEX=date2, FUN=sum)
mean(perday2)
median(perday2)
hist(perday2, xlab= "Steps per Day", col="green", main="Total Number of Steps Take Each Day")

install.packages("plotrix")
library(plotrix)
perdays<-list(perday, perday2)
multhist(perdays, xlab= "Steps per Day", col=c("blue","green"), main="Total Number of Steps Take Each Day")
legend("topright", c("With Missing","With Imputation"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue", "green"))

mean(perday,na.rm=TRUE)
mean(perday2)

median(perday,na.rm=TRUE)
median(perday2)

dayofweek <- function(date) {
        if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
                "weekend"
        } else {
                "weekday"
        }
}

activity2$dayofweek <- as.factor(sapply(activity2$date, dayofweek))
head(activity2)

par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
        stepsday <- aggregate(steps.x ~ interval, data = activity2, subset = activity2$dayofweek == 
                                        type, FUN = mean)
        plot(stepsday, type = "l", main = type,col=c("red"),xlab="Interval",ylab="Average Steps per Day")
}