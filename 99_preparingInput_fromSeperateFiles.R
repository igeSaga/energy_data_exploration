#### Description
## The following code allows to read date from all CSV files in a folder.
## The files are required to follow the same structure: Time-Tag, Value
## The sampling rate or the recorded time period can be different
##
#### Output
## The output is a matrix and stored in the RDATA format.
## Columns contain all measures from a sensor. Column names contain the respective labels.
## Rows represent time-tags, as represented by the rownames.
## Two such matrices are produced; 
## one with the original temporal resolution and one with hourly aggregations.


#reading libraries
library(ggplot2)
library(xts)
library(zoo)
library(forecast)
library(tidyverse)
library(lubridate)
library(dplyr)
library(Matrix.utils)

#clear workspace
rm(list=ls())

#reading metadata for all sensors
codes<-read.csv("../externalData/sensor_codes.csv")

#path to the datafolder on the T-drive
dataPath<-"/Volumes/data$/ta/60 FuE/6040 ZIG/604050 Projekte/60405010 Laufende/1121201_SCCER_FEEBD-I/06-Modelling_Simulation_Testing/Inputfiles/Suurstoffi/Energy Hub/Messdaten/Datenordner_WWZ/"

#list all CSV files in the datafolder
temp <- list.files(path=dataPath,pattern="*.csv")

#each CSV file is read into a list
myfiles <- lapply(temp, function(x){read.delim(file=paste(dataPath,x,sep=""), sep=";")})

##the following filtering steps are particular for the data used in this example
#skipping every second column
myfiles <- sapply(myfiles, function(x){
  x<-x[,-seq(from=3,to=ncol(x),by=2)]
  return(x)
})
#only reading the "ausspeisung"
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
#clean time-stamp round I
myfiles<-sapply(myfiles, function(x){
  stamp<-gsub(" b","",x$ZP)
  x$ZP<-stamp
  return(x)
  })
#clean time-stamp round II
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

#save the list with the data from all sensors
save(myfiles,file="../externalData/inputList_13_15.Rdata")

#create one extensive time vector, containing all unique time-stamps in the correct order
allNames<-unlist(sapply(myfiles,function(x){rbind(names(x))}))
allNames<-unique(allNames[-1])
allTimes<-unlist(sapply(myfiles,function(x){rbind(as.character(x[,1]))}))
allTimes<-unique(allTimes)

#crating an empty matrix of the correct dimensions: n time-stamps x m sensors
allData<-matrix(data = rep(NA,length(allNames)*length(allTimes)),ncol = length(allNames),dimnames = list(allTimes,allNames))

#filling-up the empty matrix by iterating through all input tables
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

#converting the date column into the correct time-format
time<-as.POSIXct(rownames(allData), format="%d.%m.%Y %H:%M", tz = "GMT")
attributes(time)$tzone <- "Europe/Zurich"
time<-time - 3600

#order everthing
allData<-allData[order(time),]

#do some restructuring
allData<-allData[,1:(ncol(allData)-1)]

#reformat the header by using more intuitive labels
codes$labels<-paste("whg nr: ",1:nrow(codes)," (haus nr; ",codes$house,", / fl; ",codes$fl," / zim; ",codes$zimmer,")",sep="")

headers<-data.frame(sensor=colnames(allData))
headers<-headers%>%
  left_join(codes,by="sensor")

colnames(allData)<-headers$labels

#saving the matrix as RDATA
save(allData,file="../externalData/allData_13_15.Rdata")


##in a second step, the temporal resolution is downsampled to 1h aggregations
#load the newly created matrix
load(file="../externalData/allData_13_15.Rdata")

#make sure the time column is in the right format
time<-as.POSIXct(row.names(allData), format="%d.%m.%Y %H:%M", tz = "GMT")
attributes(time)$tzone <- "Europe/Zurich"
time<- time - 3600

#convert to dataframe
time<-data.frame(time.m=time)

#1h resolution of the time axis
time<-time %>%
  mutate(time.h = as.POSIXct(round(time.m,"hours")))

#aggregate all columns in the matrix at once to the new 1h aggregation
allData.h<-as.matrix(aggregate.Matrix(allData,groupings=factor(time$time.h),fun="sum"))
allData<-allData.h

#save the aggregated matrix
save(allData,file="../externalData/allDataAggH_13_15.Rdata")
