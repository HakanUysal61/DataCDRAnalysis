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

occData<-na.omit(occData,cols=c("msisdn"))
# this omit did not work properly. So the records before NA values are taken (NA msisdn's omitted)
# occData<-occData[1:3407970,]
# Check if this omit really removes only NA msisdn's.


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

msc<-rep(0,100)
uniqmsi<-0
msi<-'000'
nmscc<-1
maxmscc<-1
for (i in 1:k)
{
  if (occData$msisdn[i] != msi) #MSISDN changed
    {
         msi<-occData$msisdn[i]
         if(i %% 10000 ==0 ) print(i)
  	 msc[nmscc]<-msc[nmscc]+1
  	 nmscc<-1
  	 uniqmsi<-uniqmsi+1 
   }
  else
	{
	 if (occData$startTime[i]<occData$endTime[i-1] & occData$serviceId[i]!=occData$serviceId[i-1])  # same MSISDN started another session before previous finishes	 
  		{
  			nmscc<- nmscc+1
  			if (nmscc>maxmscc) 
  			  {
  			    maxmscc<-nmscc; maxmsccmsi<-occData$msisdn[i]
  			  }
  			} 
  			
	   else 
	     {
		    msc[nmscc]<-msc[nmscc]+1			
		    nmscc<-1
	     } #endif starttime
    } # endif same MSISDN
} # end for  

mscc<-msc[1:maxmscc]
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
print(maxmsccmsi)
print("Avreage session for MSISDN")
print(k/uniqmsi)
msccc<-rep(0,maxmscc+1)
msccc[1:maxmscc]<-mscc[1:maxmscc]
msccc[maxmscc+1]<-uniqmsi
write.csv(msccc,"mscc.csv")
# write mscc proccessed data to file
# last record shows uniqmsi



