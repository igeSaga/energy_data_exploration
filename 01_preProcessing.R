library(ggplot2)
library(xts)
library(zoo)
library(forecast)
library(tidyverse)
library(lubridate)
library(dplyr)


rm(list=ls())


dataPath<-"/Volumes/data$/ta/60 FuE/6040 ZIG/604050 Projekte/60405010 Laufende/1121201_SCCER_FEEBD-I/06-Modelling_Simulation_Testing/Inputfiles/Suurstoffi/Energy Hub/Messdaten/Datenordner_WWZ/"
codes<-read.csv("data/sensor_codes.csv")

#list all csv in folder
temp <- list.files(path=dataPath,pattern="*.csv")

#read each csv into a list
myfiles <- lapply(temp, function(x){read.delim(file=paste(dataPath,x,sep=""), sep=";")})

#ignore every second column
myfiles <- sapply(myfiles, function(x){
  x<-x[,-seq(from=3,to=ncol(x),by=2)]
  return(x)
})

#only "ausspeisung"
myfiles <- sapply(myfiles, function(x){
  cols<-x[2,2:ncol(x)]=="Aussp"
  x<-x[,c(TRUE,cols)]
  return(x)
})

#ignore the header lines
myfiles <- sapply(myfiles, function(x){
  x<-x[8:nrow(x),]
  return(x)
  })

#clean time-stamp
myfiles<-sapply(myfiles, function(x){
  stamp<-gsub(" b","",x$ZP)
  x$ZP<-stamp
  return(x)
  })

#clean time-stamp II
myfiles<-sapply(myfiles, function(x){
  stamp<-strtrim(x$ZP,16)
  x$ZP<-stamp
  return(x)
  })

#only appartements
codes<-codes[codes$type=="whg",]
myfiles<-sapply(myfiles, function(x){
  wCol<-names(x)%in%codes$sensor
  wCol[1]<-TRUE
  x<-x[,wCol]
  return(x)
})

save(myfiles,file="output/inputList_13_15.Rdata")

allNames<-unlist(sapply(myfiles,function(x){rbind(names(x))}))
allNames<-unique(allNames[-1])

allTimes<-unlist(sapply(myfiles,function(x){rbind(as.character(x[,1]))}))
allTimes<-unique(allTimes)

allData<-matrix(data = rep(NA,length(allNames)*length(allTimes)),ncol = length(allNames),dimnames = list(allTimes,allNames))

#i<-5
for(i in 1:length(myfiles)){
  print(i)
  myDat<-myfiles[[i]]
  #j<-10
  for(j in 2:ncol(myDat)){
    house<-names(myDat)[j]
    if(house=="CH1021501234571357980000000000000"){
      print(paste(min(myDat[,1])))
      print(paste(max(myDat[,1])))
    }
    allData[myDat[,1],house]<-as.numeric(as.character(myDat[,j]))
  }
}

time<-as.POSIXct(rownames(allData), format="%d.%m.%Y %H:%M", tz = "GMT")
attributes(time)$tzone <- "Europe/Zurich"
time<-time - 3600

allData<-allData[order(time),]

#duplikate in den zeiten
table(duplicated(rownames(allData)))

#time differenzen
time<-as.POSIXct(rownames(allData), format="%d.%m.%Y %H:%M", tz = "GMT")
attributes(time)$tzone <- "Europe/Zurich"
time<-time - 3600

summary(as.numeric(time[1:(length(time)-1)]-time[2:length(time)]))

allData<-allData[,1:(ncol(allData)-1)]

save(allData,file="output/allData_13_15.Rdata")

load(file="output/allData_13_15.Rdata")

time<-as.POSIXct(row.names(allData), format="%d.%m.%Y %H:%M", tz = "GMT")
attributes(time)$tzone <- "Europe/Zurich"
time<- time - 3600


time<-data.frame(time.m=time)

#15min werte pro stunde summieren
time<-time %>%
  mutate(time.h = as.POSIXct(round(time.m,"hours")))

allData.h<-as.matrix(aggregate.Matrix(allData,groupings=factor(time$time.h),fun="sum"))

allData<-allData.h

save(allData,file="output/allDataAggH_13_15.Rdata")
