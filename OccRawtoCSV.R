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

removeNonNumeric <- function(s)
{
  
     r<- gsub("[^0-9]","",s,fixed=FALSE)
 }

processGyCdr <- function (f,r)
{
  print(f)
  con<-file(f,"r")
  k<-r
  g<<-readLines(con,n=1)
  
repeat
{  
  while (regexpr("onlineCreditControl",g)!= -1 && length(g)>0) 
  {  
    g<<-readLines(con,n=1)
    if (length(g)==0)  { close(con) ; return(k) }
    
  } # end while until start of CDR
    k<-k+1
    # A new CDR is found
    
    while(regexpr("sdpRealm",g)== -1)
    {
      for (i in 1:length(property))
        {
          
          if(regexpr(property[i],g)!= -1)
          {
            
            if (i==1) {
              msisdn[k]<<-substr(g,nchar(g)-14,nchar(g)-2)
              #print(msisdn[k])
              break
            }
            if (i==2) {
              session[k]<<-substr(g,20,nchar(g)-2)
             # print(session[k])
              break
            }
            if (i==3) {
              resultCode[k]<<-as.integer(removeNonNumeric(g))
              #print(resultCode[k])
              break
            }
            if (i==4) {
              serviceId[k]<<-as.integer(removeNonNumeric(g))
              #print(serviceId[k])
              break
            }
            if (i==5) {
              volume[k]<<-as.integer(removeNonNumeric(g))
              #print(volume[k])
              break
            }
            if (i==6) {
              s<-substr(g,nchar(g)-11,nchar(g)-6)
              startTime[k]<<-as.integer(substr(s,1,2))*3600+
                as.integer(substr(s,3,4))*60+
                as.integer(substr(s,5,6))
              #print(startTime[k])
              break
            }
            if (i==7) {
              s<-substr(g,nchar(g)-11,nchar(g)-6)
              endTime[k]<<-as.integer(substr(s,1,2))*3600+
                as.integer(substr(s,3,4))*60+
                as.integer(substr(s,5,6))
              #print(endTime[k])
              break
            }
            if (i==8) {
              serviceClass[k]<<-as.integer(removeNonNumeric(g))
              #print(serviceClass[k])
              break
            }
            if (i==9) {
              dedicatedAccountUsage[k]<<-as.integer(removeNonNumeric(g))
              #print(dedicatedAccountUsage[k])
              break
            }
            if (i==10) {
              daID[k]<<-as.integer(removeNonNumeric(g))
              #print(daID[k])
              break
            }  
            if (i==11) {
              offerID[k]<<-as.integer(removeNonNumeric(g))
              #print(offerID[k])
              break
            }
          } # end for  
          }  # end if (features found)
        
        g<<-readLines(con,n=1)
        if (length(g)== 0) { close(con);return(k) }
      } # end while
      g<<-readLines(con,n=1)
      if (length(g)== 0) { close(con);return(k) }
  }  # end repeat
} # end function

maxdata<- 1000
property<-rep("NA",11)

msisdn<-rep(NA,maxdata)
property[1]<-"servedAccount"

session<-rep(NA,maxdata)
property[2]<-"SessionID"

resultCode<-rep(0L,maxdata)
property[3]<-"resultCode"

serviceId<-rep(0L,maxdata)
property[4]<-"serviceIdentifier"

volume<-rep(0L,maxdata)
property[5]<-"totalOctetsUnit"

startTime<-rep(0L,maxdata)
# time will be represented as seconds in the day
property[6]<-'triggerTime : "2019'

endTime<-rep(0L,maxdata)
property[7]<-"eventTime"

duration<-rep(0L,maxdata)
# duration will be calculated endTime-startTime

serviceClass<-rep(0L,maxdata)
property[8]<-"serviceClassID"

dedicatedAccountUsage<-rep(0L,maxdata)
property[9]<- "dedicatedAccountUnitsChange"

daID<-rep(0L,maxdata)
property[10]<-"dedicatedAccountID"

offerID<-rep(0L,maxdata)
property[11]<-"offerID"

h<-""
globalVariables(c("property","msisdn","session","resultCode","serviceId"
                  ,"volume","startTime","endTime","duration",
                  "serviceClass","DedicatedAccountUsage","daID",
                  "offerID","h"))

k<-0
files<-list.files("OCC")
for (i in 1:length(files))
{
  print(i)
  f<-files[i]
  f<-paste0("./OCC/",f)
  k<- processGyCdr(f,k)
  print(k)
  
}
# All CDR's are in memory

duration<-abs(endTime-startTime)

print("number of CDR's")
print(k)

occData <- data.frame(msisdn,startTime,endTime,duration,volume,resultCode,serviceId,serviceClass,
	dedicatedAccountUsage,daID,offerID,session,stringAsFactors=FALSE)
# make data frame

#occData<-na.omit(occData,cols=c("msisdn"))
# this omit did not work properly. So the records before NA values are taken (NA msisdn's omitted)
# occData<-occData[1:3407970,]
# find a way to omit NA msisdns using upper methods
write.csv(occData,"occData.csv")
# write proccessed data to file

