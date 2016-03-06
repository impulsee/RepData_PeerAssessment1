library(ggplot2)
library(dplyr)
library(lubridate)
# This function downloads file from corresponding URL
downloadData<-function(url, destfile){    
    if(!file.exists(destfile)){
        ## first try normal method, if no - wget
        tryCatch(download.file(url, destfile=destfile) , error = function(e) e )
        e<-download.file(url, destfile=destfile, method = "wget")
    }
}

## This function tries to extract data from downloaded data
extractDownloadedData<-function(srcfile){
    if (file.exists(srcfile)){
        unzip(srcfile)
    }
    else{
        warning("No file to unzip! Use downloadData first");
    }
    
}
#This function download and extract data for this course project
DownloadExtractData<-function(){
    url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    dstfile<-"repdata-data-activity.zip"
    downloadData(url,dstfile)
    extractDownloadedData(dstfile)
}
DownloadExtractData()
activitydata<-(read.csv("activity.csv",na.strings = "NA",stringsAsFactors = FALSE))
activityavgdates<-aggregate(steps~date,activitydata,sum, na.rm = TRUE)
ggplot(activityavgdates,aes(x=steps)) + geom_histogram() + ggtitle("Total steps per day")

activitydata.splt <- split(activitydata, as.factor(activitydata$date))

mean(sapply(activitydata.splt, function(x) sum(x$steps, na.rm=T)))
median(sapply(activitydata.splt, function(x) sum(x$steps, na.rm=T)))
mean(activityavgdates[,2])
median(activityavgdates[,2])
activityavginterval<-aggregate(steps~interval,activitydata,mean,na.rm=TRUE)
ggplot(data=activityavginterval,aes(x=interval,y=steps)) + geom_line()+xlab("5-min interval") + ylab ("avg number of steps")
activityavginterval[which.max(activityavginterval$steps),]
sum(is.na(activitydata$steps))

getavg<-function (step,inter){
    avgsteps = ifelse(is.na(step),filter(activityavginterval,interval==inter)[,2],step)
    avgsteps
    
}

activitywithavg<-mutate(activitydata,steps = mapply(getavg,activitydata$steps,activitydata$interval))
activityavgdates_afteravg<-aggregate(steps~date,activitywithavg,sum, na.rm = TRUE)
ggplot(activityavgdates_afteravg,aes(x=steps)) + geom_histogram() + ggtitle("Total steps per day")

activitydatawithavg.splt <- split(activitywithavg, as.factor(activitydata$date))

mean(sapply(activitydatawithavg.splt, function(x) sum(x$steps, na.rm=T)))
median(sapply(activitydatawithavg.splt, function(x) sum(x$steps, na.rm=T)))

mean(activityavgdates_afteravg[,2])
median(activityavgdates_afteravg[,2])

activitywithavg<-mutate(activitywithavg,weekd_weekend = ifelse((wday(date) == 1 | wday(date)==7),"weekend","weekday"))
averagesinterval_wday_wend <- aggregate(steps ~ interval + weekd_weekend, data=activitywithavg, mean)
ggplot(averagesinterval_wday_wend, aes(interval, steps)) + geom_line() + facet_grid(weekd_weekend ~ .) +
    xlab("5-min interval") + ylab("avg number of steps")




