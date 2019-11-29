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



occData <- data.frame(msisdn,startTime,endTime,duration,volume,resultCode,serviceId,serviceClass,
	dedicatedAccountUsage,daID,offerID,session,stringAsFactors=FALSE)
# make data frame

write.csv(occData,"occData.csv")
# write proccessed data to file
