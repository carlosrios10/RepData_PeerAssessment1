setwd("C:/Users/Usuarioç/Desktop/carlos/reproducible research/RepData_PeerAssessment1")
getwd()
#Loading and preprocessing the data
activity<-read.csv("data/activity.csv")
str(activity)
table(activity$date)
activitywithoutNa<-activity[!is.na(activity$steps),]
sum(is.na(activitywithoutNa$steps))
#What is mean total number of steps taken per day?
totalStepDay<-ddply(activitywithoutNa,.(date),summarise,totalSteps=sum(steps))
m <- ggplot(totalStepDay, aes(x=totalSteps))
m + geom_histogram()
mean(totalStepDay$totalSteps)
median(totalStepDay$totalSteps)
#What is the average daily activity pattern?
averageStepInterval<-ddply(activitywithoutNa,.(interval),summarise,average=mean(steps))
m <- ggplot(averageStepInterval, aes(x=interval,y=average))
m + geom_line()
averageStepInterval[which.max(averageStepInterval$average),1]
#Imputing missing values
sum(is.na(activity$steps))
averageStepInterval<-ddply(activitywithoutNa,.(interval),summarise,average=mean(steps))
activityCompleted<-join(activity, averageStepInterval, by = "interval", type = "left", match = "all")
activityCompleted[is.na(activity$steps),]$steps<-activityCompleted[is.na(activity$steps),4]

totalStepDayAcCompleted<-ddply(activityCompleted,.(date),summarise,totalSteps=sum(steps))
m <- ggplot(totalStepDayAcCompleted, aes(x=totalSteps))
m + geom_histogram()
mean(totalStepDayAcCompleted$totalSteps)
median(totalStepDayAcCompleted$totalSteps)

#Are there differences in activity patterns between weekdays and weekends?
str(activityCompleted)
activityCompleted$date<-as.Date(as.character(activityCompleted$date))
activityCompleted$day<-weekdays(activityCompleted$date)
activityCompleted$partOfWeek[isWeekend(activityCompleted$date)]<-"weekend"
activityCompleted$partOfWeek[!isWeekend(activityCompleted$date)]<-"weekday"
activityCompleted$partOfWeek<-as.factor(activityCompleted$partOfWeek)
averageStepIntervalOfWeek<-ddply(activityCompleted,.(partOfWeek,interval),summarise,average=mean(steps))
m <- ggplot(averageStepIntervalOfWeek, aes(x=interval,y=average))
m + geom_line() + facet_grid(partOfWeek ~ . )


knit("PA1_template.Rmd")
