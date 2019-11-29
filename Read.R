setwd("C:/Users/ENKHUY/Documents/TRAINING/R/Gy/MTNSA")
library(tidyr)
library(dplyr)
library(reshape2)
library(data.table)
library(tidyselect)
library(datasets)
library(lattice)
library(ggplot2)
library(XML)
library(readxl)


occData<-read.csv("occData.csv",stringsAsFactors = FALSE)
#occData<-na.omit(occData,cols=c("msisdn"))
# this omit did not work properly. So the records before NA values are taken (NA msisdn's omitted)
occData<-occData[1:3407970,]
k<-length(occData$msisdn)


# dataOcc$duration<-abs(endTime-startTime)
# if duration does not exist on input calculate and put to df.

print("number of CDR's")
print(k)



# Sort all data with msisdn and start-time
ordered<-order(occData$msisdn,occData$startTime)
occData<-occData[ordered,]
# All data sorted

durationmin<-as.integer(occData$duration/60)


# find MSCC

msccc<-read.csv("mscc.csv")
maxmscc<-length(msccc$x)-1
uniqmsi<-msccc$x[maxmscc+1]
mscc<-msccc$x[1:maxmscc]

avgmscc<-0
for (i in 1:maxmscc)
  {
    avgmscc<-avgmscc+i*mscc[i]
  }
avgmscc<-avgmscc/k  
# sessions with number of mscc calculated

print("Unique MSISDN")
print(uniqmsi)
print("Max MSCC")
print(maxmscc)
#print(maxmsccmsi)
print("Avreage session for MSISDN")
print(k/uniqmsi)

occData2 <- occData %>% group_by(endTime) %>% count()
names(occData2)[2]<-"count"
names(occData2)[1]<-"endtime"
print("max. tps can be found below on the maximum of count")
summary(occData2)
