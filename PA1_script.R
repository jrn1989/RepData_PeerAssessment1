## Coded by Jose R

## Most of the comments for this script can be found in the PA_template.Rmd
## markdown document.

library("plyr")
library("ggplot2")
setwd("~/Documents/DataScienceSpecialization/5-RepRes/RepData_PeerAssessment1")

activityData <- read.csv("~/Documents/DataScienceSpecialization/5-RepRes/RepData_PeerAssessment1/activity.csv")

activityData$steps = as.numeric(as.character(activityData$steps))
activityData$interval = as.numeric(as.character(activityData$interval))
activityData$date = as.Date(activityData$date,"%Y-%m-%d")

activityDataCompleteCases = activityData[complete.cases(activityData),]

activityDataCompleteCasesTotalSteps = ddply(activityDataCompleteCases,  "date", summarise, steps = sum(steps))

plot(activityDataCompleteCasesTotalSteps$date, activityDataCompleteCasesTotalSteps$steps,type="h",lwd=10,col="blue",xlab="Days", ylab="Total steps")

mean(activityDataCompleteCasesTotalSteps$steps)

median(activityDataCompleteCasesTotalSteps$steps)

sum(!complete.cases(activityData))

meansEachDay = ddply(activityData,  "date", summarise, steps = mean(steps))
meansEachDay$steps[is.na(meansEachDay$steps)] <- 0

activityData2 = activityData

j=1
for(i in 1:nrow(activityData2)){
  if(is.na(activityData2[i,1])){
    if(activityData2[i,2]==meansEachDay[j,1]){
      activityData2[i,1] = meansEachDay[j,2]
      
    }else{
      j = j+1
      activityData2[i,1] = meansEachDay[j,2]
    
    }
  }
}

#meansEachDay2 = ddply(activityData2,  "date", summarise, steps = mean(steps))
final = ddply(activityData2,  "date", summarise, steps = sum(steps))

mean(final$steps)

median(final$steps)

activityData2$date = as.Date(activityData2$date,"%Y-%m-%d")
activityData2$weekend = "Weekend"
activityData2[weekdays(activityData2$date) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")
      ,c("weekend") ] = "Weekday"


my5interval2 = ddply(activityData2,  c("weekend","interval"), summarise, steps = mean(steps))
my5interval2$weekend = as.factor(my5interval2$weekend)


ggplot(my5interval2, aes(interval, steps)) + geom_line() + facet_grid(weekend ~ .)

