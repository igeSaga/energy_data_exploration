rm(list=ls())

strom<-read.csv("../data/pv.csv")
names(strom)[1]<-"time"

strom$time<-strptime(as.character(strom$time), format="%d.%m.%y %H:%M")

library(xts)

meanPv<-apply(strom[,-1],1,function(x){
  mean(x,na.rm=T)
  })

tsPv<-strom$time[!is.na(meanPv)]
meanPv<-meanPv[!is.na(meanPv)]

str.m<-xts(meanPv,tsPv)

plot(str.m)

period<-spec.pgram(str.m, log="no",plot=F,spans=50)

year <- round(abs(as.numeric(tsPv[1]-tsPv[length(tsPv)]))/365)
  
period<-data.frame(occPerYear= (1:length(period[[2]]))/year, spec=period[[2]])

plot(spec~occPerYear, data=period[1:1000,],type="l", xlab="occurrences per year")
