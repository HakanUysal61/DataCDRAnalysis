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


#occData<-na.omit(occData,cols=c("msisdn"))
# this omit did not work properly. So the records before NA values are taken (NA msisdn's omitted)
#occData<-occData[1:3407970,]
k<-length(occData$msisdn)

occData3<-subset(occData,serviceId!=500 & serviceId!=1)
occData4<-subset(occData,duration<4)


occData5 <- occData4 %>% group_by(offerID) %>% count()
names(occData5)[2]<-"count"
names(occData5)[1]<-"offerID"
occData5<-na.omit(occData5)
summary(occData5)
write.csv(occData5,"lessthan4-of.csv")

occData6 <- occData4 %>% group_by(serviceId) %>% count()
names(occData6)[2]<-"Count"
names(occData6)[1]<-"ServiceID"
occData6<-na.omit(occData6)
summary(occData6)
write.csv(occData6,"lessthan4-ser.csv")

occData4<-subset(occData,duration<60)


occData5 <- occData4 %>% group_by(offerID) %>% count()
names(occData5)[2]<-"count"
names(occData5)[1]<-"offerID"
occData5<-na.omit(occData5)
summary(occData5)
write.csv(occData5,"lessthanmin-of.csv")

occData6 <- occData4 %>% group_by(serviceId) %>% count()
names(occData6)[2]<-"Count"
names(occData6)[1]<-"ServiceID"
occData6<-na.omit(occData6)
summary(occData6)
write.csv(occData6,"lessthanmin-ser.csv")

png(file="plot53.png")
plot(occData5,main="Less than 4 sec. Offers",col="blue",
        type="l",xlab="OfferID")
dev.off()

png(file="plot54.png")
plot(occData6,main="Less than 4 sec. Offers",col="red",
     type="l",xlab="DAID")
dev.off()

png(file="plot31.png")
hist(na.omit(occData$volume)/1024/1024,breaks=20,freq=TRUE,main="Volume per Interrogation",col="green",xlab="Volume(MB)"
     , xlim=c(0,50),border="blue")
abline(v=mean(occData$volume,na.rm=TRUE)/1024/1024,col="red",lwd=3,lty=2)
dev.off()

png(file="plot32.png")
hist(na.omit(occData$duration),breaks=100,main="Duration per Interrogation",col="blue",
     xlab="Duration(sec)",freq=TRUE,xlim=c(0,3600))
abline(v=mean(occData$duration,na.rm=TRUE),col="red",lwd=3,lty=2)
dev.off()

png(file="plot34.png")
hist(na.omit(durationmin),breaks=100,main="Duration per Interrogation",col="blue",
     xlab="Duration(min)",freq=TRUE,xlim=c(0,60))
abline(v=mean(durationmin,na.rm=TRUE),col="red",lwd=3,lty=2)
dev.off()

png(file="plot35.png")
barplot(occData$volume/1024/1024,main="Volume per Interrogation",col="blue",
        xlab="Volume(MB)",xlim=c(0,50))
dev.off()

png(file="plot37.png")
barplot(mscc,main="Paralel Sessions(mscc)",col="red",
        xlab="mscc",names=c(1:maxmscc))
abline(v=mean(mscc),col="blue",lwd=3,lty=2)
dev.off()

# DA non DA

danonda<-rep(0,2)

total<-sum(occData$volume,na.rm=TRUE)/1024/1024
danonda[2]<-sum(occData$dedicatedAccountUsage,na.rm=TRUE)/1024/1024
danonda[1]<-total-danonda[2]
percentda<-c(0,0)
percentda[2]<-as.integer((danonda[2]/total)*100)
percentda[1]<-100-percentda[2]
tit1<-paste0(as.character(percentda[1]),"% non DA")
tit2<-paste0(as.character(percentda[2]),"% DA")

png(file="plot39.png")
pie(danonda,labels= c(tit1,tit2),main="DA vs. non-DA usage")
dev.off()

# Offer non-offer

danoff<-rep(0,2)
isoffer<-occData$offerID>0
danoff[2]<-sum(occData$volume[isoffer],na.rm=TRUE)/1024/1024
danoff[1]<-total-danoff[2]
percentof<-c(0,0)
percentof[2]<-as.integer((danoff[2]/total)*100)
percentof[1]<-100-percentof[2]

tit1<-paste0(as.character(percentof[1]),"% No offer")
tit2<-paste0(as.character(percentof[2]),"% Offer")

png(file="plot40.png")
pie(danoff,labels= c(tit1,tit2),main="Offer vs. non-Offer usage")
dev.off()

png(file="plot42.png")
plot(occData$volume/1024/1024,occData$daID,main="Volume-DAID relation",xlab="Volume(MB)",ylab="DA ID")
dev.off()

png(file="plot43.png")
plot(occData$volume/1024/1024,occData$offerID,main="Volume-Offer ID relation",xlab="Volume(MB)",ylab="Offer ID")
dev.off()

occData2 <- occData %>% group_by(serviceId) %>% summarise(sum(volume)/1024/1024)
names(occData2)[2]<-"volume"
names(occData2)[1]<-"serviceId"

png(file="plot44.png")
plot(occData2$volume,occData2$serviceId,xlab="Volume(MB)",ylab="ServiceId"
     ,main  ="Total Volume/Service",pch=20,lwd=2,type="line")
dev.off()

occData2 <- occData %>% group_by(serviceId) %>% count()
names(occData2)[2]<-"count"
names(occData2)[1]<-"serviceId"

png(file="plot441.png")
barplot(occData2$count,occData2$serviceId,
 xlab="Service ID",ylab="Count",col="green",axisnames = TRUE,
    main  ="Count/Service",names=occData2$serviceId)
dev.off()

occData2 <- occData %>% group_by(daID) %>% summarise(sum(volume)/1024/1024)
names(occData2)[2]<-"volume"
names(occData2)[1]<-"daID"

png(file="plot45.png")
plot(occData2$volume,occData2$daID,xlab="Volume(MB)",ylab="DA ID",main  ="Total Volume/DA",pch=20,lwd=2,type="l")
dev.off()



png(file="plot47.png")
ggplot(occData, aes(x = volume, y = duration)) +
    geom_point(aes(color = factor(serviceId)))+
    labs(title="Volume by Duration")
dev.off()

png(file="plot48.png")
gg<-ggplot(occData, aes(x = volume, y = resultCode)) +
    geom_point(aes(color = factor(serviceId)))+
    labs(title="Volume by Result Code")
plot(gg)
dev.off()

png(file="plot49.png")
barplot(occData$volume/1024/1024,main="Volume for Result Codes",col="darkblue",
        xlab="volume(MB)")
dev.off()

occData2 <- occData %>% group_by(duration) %>% summarise(serviceId)
names(occData2)[2]<-"serviceId"
names(occData2)[1]<-"duration"

png(file="plot50.png")
hist(na.omit(occData2$duration),breaks=60,main="Duration per Services",col="blue",
     xlab="Duration(sec)",freq=TRUE,xlim=c(0,59))
dev.off()

png(file="plot51.png",width = 1024,heigh=768)
occData %>% 
  ggplot( aes(x=endTime,y=volume/1024/1024)) +
  geom_line(color="blue",size=0.2)+ 
  facet_wrap(.~serviceId,strip.position = "top",dir="v")+
  labs(title="service based volume",x="Time",y="Volume(MB)")
dev.off()

png(file="plot52.png",width = 1024,heigh=768)
occData %>% 
  ggplot( aes(x=endTime,y=volume/1024/1024)) +
  geom_line(color="blue",size=0.2)+ 
  facet_wrap(.~daID,strip.position = "top",dir="v")+
  labs(title="da based volume",x="Time",y="Volume(MB)")
dev.off()